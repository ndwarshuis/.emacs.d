;;; org-x-dag.el --- Org-in-a-DAG -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nathan Dwarshuis

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Welcome to Dagestan, you will be smeshed...

;;; Code:

;; TODO this depends on other stuff in org-x like the file and id operations
(require 'org)
(require 'org-ml)
(require 'dash)
(require 'dag)
(require 'ht)

;;; GLOBAL STATE

;; variables to store state

(defun org-x-dag-create (d m f)
  (list :dag d :id->meta m :file->ids f))

(defun org-x-dag-empty ()
  (org-x-dag-create (dag-empty) (ht-create #'equal) (ht-create #'equal)))

(defvar org-x-dag (org-x-dag-empty)
                        
  "The org-x DAG.

Each node in this DAG represents a headline with the following
characteristics:
- contained in a file as given by `org-x-dag-get-files'
- has a keyword
- either has an immediate parent with a keyword or has no parents
  with keywords

Each node is represented by a key, which is either a string
representing the headlines's ID property or a cons cell
like (FILE POS) representing the staring position in file/buffer
of the headline (aka a \"pseudo-marker\").")

(defvar org-x-dag-sync-state nil
  "An alist representing the sync state of the DAG.

The car of each cell is the file path, and the cdr is the md5 of
that file as it currently sits on disk.")

;; functions to construct nodes within state

(defun org-x-dag-build-meta (file point level todo tags parent)
  (list :file file
        :point point
        :level level
        :todo todo
        :tags tags
        :buffer-parent parent))

;; state lookup functions
;;
;; all functions with `org-x-dag->' or `org-x-dag-id->' depend on the value of
;; `org-x-dag'

(defun org-x-dag->metatable ()
  (plist-get org-x-dag :id->meta))

(defun org-x-dag->dag ()
  (plist-get org-x-dag :dag))

(defun org-x-dag->adjacency-list ()
  (dag-get-adjacency-list (org-x-dag->dag)))

(defun org-x-dag-id->metaprop (id prop)
  (-> (org-x-dag->metatable)
      (ht-get id)
      (plist-get prop)))

(defun org-x-dag-id->file (id)
  "Return file for ID."
  (org-x-dag-id->metaprop id :file))

(defun org-x-dag-id->point (id)
  "Return point for ID."
  (org-x-dag-id->metaprop id :point))

(defun org-x-dag-id->todo (id)
  "Return todo keyword for ID."
  (org-x-dag-id->metaprop id :todo))

(defun org-x-dag-id->local-tags (id)
  "Return local tags for ID."
  (org-x-dag-id->metaprop id :tags))

(defun org-x-dag-id->is-done-p (id)
  "Return t if ID has done keywords."
  (member (org-x-dag-id->todo id) org-x-done-keywords))

(defun org-x-dag-id->is-floating-p (id)
  (-> (plist-get org-x-dag :dag)
      (dag-get-floating-nodes)
      (ht-get id)))

(defun org-x-dag-id->is-toplevel-p (id)
  (or (not (org-x-dag-id->metaprop id :buffer-parent))
      ;; TODO pretty sure this will never be used
      (org-x-dag-id->is-floating-p id)))

(defun org-x-dag-id->tags (inherit? init id)
  (cl-labels
      ((ascend
        (id tags)
        (-if-let (parent (org-x-dag-id->metaprop id :buffer-parent))
            ;; tags in the front of the list have precedence over latter tags,
            ;; so putting parent tags at the end means child tags have
            ;; precedence
            (->> (org-x-dag-id->local-tags parent)
                 (append tags)
                 (ascend parent))
          tags)))
    ;; likewise, init tags have the lowest precedence (the likely use case for
    ;; this argument is for file tags)
    (let ((local-tags (org-x-dag-id->local-tags id))
          (parent-tags (and inherit? (ascend id nil))))
      (append local-tags parent-tags init))))

(defun org-x-dag-id->bucket (inherit? id)
  (-some->> (org-x-dag-id->tags inherit? nil id)
    (--find (= (elt it 0) org-x-tag-category-prefix))
    (s-chop-prefix "_")
    (intern)))

(defun org-x-dag-id->link (id)
  (org-x-dag-with-id-in-file id
    (let ((desc (org-get-heading t t t t)))
      (->> (org-ml-build-secondary-string! desc)
           (apply #'org-ml-build-link id :type "id")))))

(defun org-x-dag-id->link-item (id)
  (->> (org-x-dag-id->link id)
       (org-ml-build-paragraph)
       (org-ml-build-item)))

(defun org-x-dag-id->parents (id)
  (->> (plist-get org-x-dag :dag)
       (dag-get-parents id)))

(defun org-x-dag-id->children (id)
  (->> (plist-get org-x-dag :dag)
       (dag-get-children id)))

(defun org-x-dag-id->headline-children (id)
  (->> (org-x-dag-id->children id)
       (--filter (equal (org-x-dag-id->metaprop it :buffer-parent) id))))

(defun org-x-dag-id->all-headline-children (id)
  (->> (org-x-dag-id->headline-children id)
       (-mapcat #'org-x-dag-id->all-headline-children)
       (cons id)))

(defun org-x-dag-files->ids (files)
  (let ((filemap (plist-get org-x-dag :file->ids)))
    (--mapcat (ht-get filemap it) files)))

(defun org-x-dag->epg-ids ()
  (org-x-dag-files->ids `(,(org-x-get-endpoint-goal-file))))

(defun org-x-dag->ltg-ids ()
  (org-x-dag-files->ids `(,(org-x-get-lifetime-goal-file))))

(defun org-x-dag->leaf-epg-ids ()
  (-remove #'org-x-dag-id->headline-children (org-x-dag->epg-ids)))

(defun org-x-dag->leaf-ltg-ids ()
  (let ((epg-file (org-x-get-endpoint-goal-file)))
    (->> (org-x-dag->ltg-ids)
         (-remove #'org-x-dag-id->headline-children)
         (--remove (equal (org-x-dag-id->file it) epg-file)))))

(defun org-x-dag-goal-count-tasks (id)
  (->> (org-x-dag-id->children id)
       (-mapcat #'org-x-dag-id->all-headline-children)
       ;; TODO this isn't very efficient, looking up children twice
       (-remove #'org-x-dag-id->headline-children)
       (length)))

(defun org-x-dag-rank-leaf-goals (quarter ids)
  (cl-flet
      ((score
        (buckets id)
        ;; TODO what happens when I don't have a bucket?
        (let ((idx (-elem-index (org-x-dag-id->bucket t id) (reverse buckets)))
              (ntasks (org-x-dag-goal-count-tasks id)))
          (list idx ntasks))))
    (let ((bs (org-x-qtp-get-buckets quarter)))
      (org-x-dag-ids-rank (score bs it) ids))))

;; planning state

;; TODO might be less tedious to just set the date and have functions handy
;; to get the current quarter and week start
(defvar org-x-dag-selected-quarter nil
  "The current quarter to be used for planning.
Is a list like (YEAR QUARTER).")

(defvar org-x-dag-selected-week nil
  "The current week to be used for planning.
A date like (YEAR MONTH DAY).")

(defvar org-x-dag-week-start-index 0
  "The day considered to start a week (0 = Sunday).")

(defvar org-x-dag-selected-date nil
  "The current week to be used for planning.
A date like (YEAR MONTH DAY).")

(defun org-x-dag-gregorian-to-date (greg)
  (-let (((m d y) greg))
    `(,y ,m ,d)))

(defun org-x-dag-set-planning-quarter-at-date (date)
  ;; ASSUME the date is valid with no overflow digits
  (-let (((y m _) date))
    (setq org-x-dag-selected-quarter `(,y ,(/ m 3)))))

(defun org-x-dag-date-to-week-number (date)
  (-let* (((y m d) date)
          (greg (org-x-dag-date-to-gregorian date))
          (abs (calendar-absolute-from-gregorian greg))
          (daynum (calendar-day-of-week greg))
          ;; Catch the special case where the first few days of January might
          ;; belong to the previous year
          (start-year (if (and (= 1 m) (< d (1+ daynum))) (1- y)))
          (start-greg `(1 1 ,start-year))
          (start-daynum (calendar-day-of-week start-greg))
          (start-abs (calendar-absolute-from-gregorian start-greg))
          (start-diff (if (= 0 start-daynum) 0 (- 7 start-daynum))))
    (1+ (/ (- abs start-abs start-diff) 7))))

(defun org-x-dag-week-number-to-date (year weeknum)
  (let* ((start-greg `(1 1 ,year))
         (start-abs (calendar-absolute-from-gregorian start-greg))
         (start-weeknum (calendar-day-of-week start-greg))
         (start-diff (if (= 0 start-weeknum) 0 (- 7 start-weeknum))))
    (->> (* (1- weeknum) 7)
         (+ start-abs start-diff)
         (calendar-gregorian-from-absolute)
         (org-x-dag-gregorian-to-date))))

(defun org-x-dag-date-to-week-start (date)
  ""
  (let ((greg (org-x-dag-date-to-gregorian date)))
    (while (not (= (calendar-day-of-week greg) org-x-dag-week-start-index))
      (setq greg (->> (calendar-absolute-from-gregorian greg)
                      (1-)
                      (calendar-gregorian-from-absolute))))
    (org-x-dag-gregorian-to-date greg)))

(defun org-x-dag-set-planning-week-at-date (date)
  (setq org-x-dag-selected-week date))

(defun org-x-dag-set-planning-date (date)
  ;; TODO validate date?
  (let ((week-start (org-x-dag-date-to-week-start date)))
    (setq org-x-dag-selected-date date)
    (setq org-x-dag-selected-week week-date)
    (org-x-dag-set-planning-quarter-at-date week-date)))

;;; PLANNING

(defun org-x--qtp-headline-get-year (headline)
  (let ((rt (org-ml-get-property :raw-value headline)))
    (if (s-matches-p "[0-9]\\{4\\}" rt) (string-to-number rt)
      (error "Invalid year headline in quarterly plan: %s" rt))))

(defun org-x--qtp-headline-get-quarter (headline)
  (let ((rt (org-ml-get-property :raw-value headline)))
    (-if-let ((_ qt) (s-match "Q\\([0-9]\\)" rt)) (string-to-number qt)
      (error "Invalid quarter headline in quarterly plan: %s" rt))))

(defun org-x-dag-qtp-to-children (qt-plan)
  (-let* (((&plist :categories :goals) qt-plan)
          ;; TODO what happens if there are no categories?
          (sec (-some->> categories
                 (--map-indexed (org-ml-build-item!
                                 :bullet it-index
                                 :paragraph (symbol-name it)))
                 (apply #'org-ml-build-plain-list)
                 (org-ml-build-drawer org-x-drwr-categories)
                 (org-ml-build-section))))
    (if sec (cons sec goals) goals)))

(defun org-x-dag-qtp-from-children (children)
  ;; ignore properties, planning, etc
  (-let* (((sec goals) (if (org-ml-is-type 'section (car children))
                           `(,(car children) ,(cdr children))
                         `(nil ,children)))
          (cats (-some->> sec
                  (--find (org-x--is-drawer-with-name org-x-drwr-categories it))
                  (org-x-qtp-drawer-to-categories))))
    (list :categories cats :goals goals)))

(defun org-x-dag-qtp-get (quarter)
  (org-x-with-file (org-x-qtp-get-file)
    (-let (((year qnum) quarter))
      (->> (org-ml-parse-subtrees 'all)
           (org-x--qtp-headline-find-year year)
           (org-ml-headline-get-subheadlines)
           (org-x--qtp-headline-find-quarter qnum)
           (org-ml-get-children)
           (org-x-dag-qtp-from-children)))))

(defun org-x-dag-qtp-set (quarter qt-plan)
  (cl-flet*
      ((build-qt-headline
        (quarter children)
        (let ((title (list (format "Q%s" quarter))))
          (apply #'org-ml-build-headline :title title :level 2 children)))
       (build-yr-headline
        (year qnum children)
        (->> (build-qt-headline qnum children)
             (org-ml-build-headline! :title-text (number-to-string year)))))
    (org-x-with-file (org-x-qtp-get-file)
      (-let* (((year qnum) quarter)
              (sts (org-ml-parse-subtrees 'all))
              (children (org-x-dag-qtp-to-children qt-plan)))
        (-if-let (st-yr (org-x--qtp-headline-find-year year sts))
            (-if-let (st-qt (->> (org-ml-headline-get-subheadlines st-yr)
                                 (org-x--qtp-headline-find-quarter qnum)))
                (org-ml-update* (org-ml-set-children children it) st-qt)
              (org-ml-update*
                (-snoc it (build-qt-headline qnum children))
                st-yr))
          (let ((end (1+ (org-ml-get-property :end (-last-item sts)))))
            (org-ml-insert end (build-yr-headline year qnum children))))))))

(defmacro org-x-dag-qtp-map (quarter form)
  (declare (indent 1))
  `(let ((it (org-x-dag-qtp-get ,quarter)))
     (org-x-dag-qtp-set ,quarter ,form)))

(defun org-x-dag-qtp-get-key (key quarter)
  (plist-get (org-x-dag-qtp-get quarter) key))

(defun org-x-dag-qtp-set-key (quarter key xs)
  (org-x-dag-qtp-map quarter
    (plist-put it key xs)))

(defun org-x-qtp-get-buckets (quarter)
  (org-x-dag-qtp-get-key :categories quarter))

(defun org-x-qtp-get-goals (quarter)
  (org-x-dag-qtp-get-key :goals quarter))

(defun org-x-qtp-get-goal-ids (quarter)
  (->> (org-x-qtp-get-goals quarter)
       (--map (org-ml-headline-get-node-property "ID" it))))

(defun org-x-qtp-get-goal-parent-ids (quarter)
  (->> (org-x-qtp-get-goals quarter)
       (-mapcat #'org-x-dag-headline-get-parent-links)))

(defun org-x-qtp-set-categories (quarter categories)
  (org-x-dag-qtp-set-key quarter :categories categories))

(defun org-x-qtp-set-goals (quarter goals)
  (org-x-dag-qtp-set-key quarter :goals goals))

(defmacro org-x-qtp-map-categories (quarter form)
  `(let ((it (org-x-qtp-get-buckets ,quarter)))
     (org-x-qtp-set-categories ,quarter ,form)))

(defmacro org-x-qtp-map-goals (quarter form)
  `(let ((it (org-x-qtp-get-goals ,quarter)))
     (org-x-qtp-set-goals ,quarter ,form)))

(defun org-x-qtp-add-goal (quarter headline)
  (org-x-qtp-map-goals quarter (cons headline it)))

(defun org-x-dag-headline-add-id (headline)
  (org-ml-headline-set-node-property "ID" (org-id-new) headline))

(defun org-x-qtp-build-goal-headline (ids title allocation)
  ;; ASSUME the allocation is in a valid format
  (let ((d (org-x-dag-build-parent-link-drawer ids)))
    (->> (org-ml-build-headline! :level 3
                                 :title-text title
                                 :todo-keyword org-x-kw-todo
                                 :section-children (list d))
         (org-x-dag-headline-add-id)
         (org-ml-headline-set-node-property org-x-prop-allocate allocation))))

(defun org-x-qtp-add-goal-ids (quarter ids title allocation)
  (->> (org-x-qtp-build-goal-headline ids title)
       (org-x-qtp-add-goal quarter)))

(defun org-x-dag-headlines-find-tag (tag headlines)
  (--find (org-ml-headline-has-tag tag it) headlines))

(defun org-x-dag-headlines-find-year (year headlines)
  (org-x-dag-headlines-find-tag (format "Y%d" (mod year 2000)) headlines))

(defun org-x-dag-headlines-find-week (weeknum headlines)
  (org-x-dag-headlines-find-tag (format "W%02d" weeknum) headlines))

(defconst org-x-dag-weekly-tags
  '((0 . "SUN")
    (1 . "MON")
    (2 . "TUE")
    (3 . "WED")
    (4 . "THU")
    (5 . "FRI")
    (6 . "SAT")))

(defun org-x-dag-headlines-find-day-of-week (daynum headlines)
  (-> (alist-get daynum org-x-dag-weekly-tags)
      (org-x-dag-headlines-find-tag headlines)))

(defun org-x-dag-headlines-find-month (month headlines)
  (org-x-dag-headlines-find-tag (format "M%02d" month) headlines))

(defun org-x-dag-headlines-find-day (day headlines)
  (org-x-dag-headlines-find-tag (format "D%02d" day) headlines))

(defun org-x-dag-weekly-headlines-to-alist (headlines)
  (->> (-map #'car org-x-dag-weekly-tags)
       (--map (->> (org-x-dag-headlines-find-day-of-week it headlines)
                   (org-ml-headline-get-subheadlines)
                   (cons it)))))

(defun org-x-dag-weekly-alist-to-headlines (plan)
  (--map (-let (((daynum . hls) it))
           (apply #'org-ml-build-headline!
                  :tags (list (alist-get daynum org-x-dag-weekly-tags))
                  :level 3
                  :title-text (elt calendar-day-name-array daynum)
                  hls))
         plan))

(defun org-x-dag-wkp-get (week)
  (org-x-with-file (org-x-get-weekly-plan-file)
    (-let (((year weeknum) week))
      (->> (org-ml-parse-subtrees 'all)
           (org-x-dag-headlines-find-year year)
           (org-ml-headline-get-subheadlines)
           (org-x-dag-headlines-find-week weeknum)
           (org-ml-headline-get-subheadlines)
           (org-x-dag-weekly-headlines-to-alist)))))

(defun org-x-dag-wkp-set (week plan)
  (cl-flet*
      ((build-wk-headline
        (year weeknum children)
        (-let* (((_ m d) (org-x-dag-week-number-to-date year weeknum))
                (m* (calendar-month-name m))
                (title (format "%s %s" m* d))
                (tag (format "W%02d" weeknum)))
          (apply #'org-ml-build-headline!
                 :title-text title
                 :level 2
                 :tags (list tag)
                 children)))
       (build-yr-headline
        (year weeknum children)
        (let ((title (number-to-string year))
              (tag (format "Y%d" year)))
          (->> (build-wk-headline year weeknum children)
               (org-ml-build-headline! :title-text title :tag (list tag))))))
    (org-x-with-file (org-x-get-weekly-plan-file)
      (-let* (((year weeknum) week)
              (sts (org-ml-parse-subtrees 'all))
              (children (org-x-dag-weekly-alist-to-headlines plan)))
        (-if-let (st-yr (org-x-dag-headlines-find-year year sts))
            (-if-let (st-wk (->> (org-ml-headline-get-subheadlines st-yr)
                                 (org-x-dag-headlines-find-week weeknum)))
                (org-ml-update* (org-ml-set-children children it) st-wk)
              (org-ml-update*
                (-snoc it (build-wk-headline year weeknum children))
                st-yr))
          (let ((end (1+ (org-ml-get-property :end (-last-item sts)))))
            (org-ml-insert end (build-yr-headline year weeknum children))))))))

(defmacro org-x-dag-wkp-map (week form)
  (declare (indent 1))
  (let ((w (make-symbol "--week")))
    `(let* ((,w ,week)
            (it (org-x-dag-wkp-get ,w)))
       (org-x-dag-wkp-set ,w ,form))))

(defun org-x-dag-wkp-day-get (week daynum)
  (alist-get daynum (org-x-dag-wkp-get week)))

(defun org-x-dag-wkp-day-set (week daynum headlines)
  (org-x-dag-wkp-map week
    (--replace-where (= daynum (car it)) (cons daynum headlines) it)))

(defmacro org-x-dag-wkp-day-map (week daynum form)
  (declare (indent 2))
  (let ((w (make-symbol "--week"))
        (d (make-symbol "--daynum")))
    `(let* ((,w ,week)
            (,d ,daynum)
            (it (org-x-dag-wkp-day-get ,w ,d)))
       (org-x-dag-wkp-day-set ,w ,d ,form))))

(defun org-x-dag-wkp-day-add (week daynum headline)
  (org-x-dag-wkp-day-map week daynum (cons headline it)))

(defun org-x-dag-wkp-add-goal (week daynum title ids desc)
  (let ((p (org-ml-build-paragraph! desc)))
    (->> (org-ml-build-headline! :level 4
                                 :title-text title
                                 :todo-keyword org-x-kw-todo
                                 :section-children (list p))
         (org-x-dag-headline-add-id)
         (org-x-dag-headline-set-parent-links ids)
         (org-x-dag-wkp-day-add week daynum))))

;; TODO not DRY
(defun org-x-dag-dlp-get (date)
  (org-x-with-file (org-x-get-weekly-plan-file)
    (-let (((y m d) date))
      (->> (org-ml-parse-subtrees 'all)
           (org-x-dag-headlines-find-year y)
           (org-ml-headline-get-subheadlines)
           (org-x-dag-headlines-find-week m)
           (org-ml-headline-get-subheadlines)
           (org-x-dag-headlines-find-day d)))))

(defun org-x-dag-dlp-set (date headlines)
  (cl-flet*
      ((build-day-headline
        (date headlines)
        (-let* (((y m d) date)
                (title (format "%d-%02d-%02d" y m d))
                (tag (format "D%02d" d)))
          (apply #'org-ml-build-headline!
                 :title-text title
                 :tags (list tag)
                 :level 3
                 headlines)))
       (build-mo-headline
        (date headlines)
        (-let* (((_ m _) date)
                (title (calendar-month-name m))
                (tag (format "M%02d" m)))
          (->> (build-day-headline date headlines)
               (org-ml-build-headline! :title-text title
                                       :level 2
                                       :tags (list tag)))))
       (build-yr-headline
        (date headlines)
        (-let* (((y _ _) date)
                (title (number-to-string y))
                (tag (format "Y%d" y)))
          (->> (build-mo-headline date headlines)
               (org-ml-build-headline! :title-text title :tags (list tag))))))
    (org-x-with-file (org-x-get-daily-plan-file)
      (-let (((y m d) date)
             (sts (org-ml-parse-subtrees 'all)))
        (-if-let (st-yr (org-x-dag-headlines-find-year y sts))
            (-if-let (st-mo (->> (org-ml-headline-get-subheadlines st-yr)
                                 (org-x-dag-headlines-find-month m)))
                (-if-let (st-day (->> (org-ml-headline-get-subheadlines st-mo)
                                      (org-x-dag-headlines-find-day d)))
                    (org-ml-update* (org-ml-set-children headlines it) st-day)
                  (org-ml-update*
                    (-snoc it (build-day-headline date headlines))
                    st-mo))
              (org-ml-update*
                (-snoc it (build-mo-headline date headlines))
                st-yr))
          (let ((end (1+ (org-ml-get-property :end (-last-item sts)))))
            (org-ml-insert end (build-yr-headline date headlines))))))))

(defmacro org-x-dag-dlp-map (date form)
  (declare (indent 1))
  (let ((d (make-symbol "--date")))
    `(let* ((,d ,date)
            (it (org-x-dag-dlp-get ,d)))
       (org-x-dag-dlp-set ,d ,form))))

(defun org-x-dag-dlp-add (date headline)
  (org-x-dag-dlp-map date (cons headline it)))

(defun org-x-dag-dlp-add-task (date title ids time)
  (let ((datetime `(,@date ,@time)))
    (->> (org-ml-build-headline! :level 4
                                 :title-text title
                                 :planning `(:scheduled ,datetime)
                                 :todo-keyword org-x-kw-todo)
         (org-x-dag-headline-add-id)
         (org-x-dag-headline-set-parent-links ids)
         (org-x-dag-dlp-add date))))

;;; BUFFER SCANNING

(defun org-x-dag-get-local-property (prop)
  (car (org--property-local-values prop nil)))

(defun org-x-dag-get-link-property ()
  (-some->> (org-x-dag-get-local-property org-x-prop-goal)
    (s-split ";")
    (--map (->> (s-trim it)
                (s-match "^\\[\\[id:\\(.*\\)\\]\\[.*\\]\\]$")
                (cadr)))))

(defun org-x-dag-get-parent-links ()
  (save-excursion
    (let ((re (concat
               "^[ \t]*:X_PARENT_LINKS:[ \t]*\n"
               "\\(\\(?:^- .*?\n\\)*?\\)"
               "[ \t]*:END:[ \t]*$"))
          (end (save-excursion (outline-next-heading))))
      (-some->> (and (re-search-forward re end t) (match-string 1))
        (s-trim)
        (s-split "\n")
        (--map (cadr (s-match "id:\\([^][]\\{36\\}\\)" it)))))))

(defun org-x-dag-get-buffer-nodes (file kws)
  "Return a list of nodes from FILE.

A node will only be returned if the headline to which it points
has a valid (meaning in KWS) keyword and either its parent has a
valid keyword or none of its parents have valid keywords."
  (let ((more t)
        cur-path this-point this-key this-level this-todo has-todo this-parent
        this-tags this-meta all-tags this-file-links this-links acc acc-meta
        this-parent-key)
    ;; TODO add org-mode sanity check
    (goto-char (point-min))
    ;; If not on a headline, check for a property drawer with links in it
    (unless (= ?* (following-char))
      ;; (setq this-file-links (org-x-dag-get-link-property)))
      (setq this-file-links (org-x-dag-get-parent-links)))
    ;; move forward until on a headline
    (while (and (not (= ?* (following-char))) (= 0 (forward-line 1))))
    ;; Build alist; Keep track of how 'deep' we are in a given org-tree using a
    ;; stack. The stack will have members like (LEVEL KEY) where LEVEL is the
    ;; level of the headline and KEY is the node key if it has a keyword. Only
    ;; add a node to the accumulator if it has a keyword, and only include its
    ;; parent headline if the parent also has a keyword (add the link targets
    ;; regardless).
    (while more
      (when (= ?* (following-char))
        (setq this-point (point)
              this-key nil)
        ;; TODO this might be optimizable
        ;; Get tags (must be done from the first column)
        (setq this-tags (org--get-local-tags))
        ;; Get the level if the following char is a space (if it isn't this is
        ;; actually a bolded object that starts on the first column like
        ;; '*blabla*'
        (while (= ?* (following-char)) (forward-char 1))
        (when (= 32 (following-char))
          (setq this-level (current-column))
          ;; Check if the headline has a keyword
          (forward-char 1)
          (while (not (memq (following-char) '(?  ?\n))) (forward-char 1))
          (setq this-todo (-> (+ 1 this-point this-level)
                              (buffer-substring (+ this-point (current-column))))
                has-todo (member this-todo kws))
          ;; Adjust the stack so that the top headline is the parent of the
          ;; current headline
          (while (and cur-path (<= this-level (nth 0 (car cur-path))))
            (!cdr cur-path))
          (setq this-parent (car cur-path)
                this-parent-key (nth 1 this-parent))
          ;; Add the current headline to accumulator if it has a keyword, but only
          ;; if its parent has a keyword or none of its parents have keywords
          (when (and has-todo (or this-parent-key (--none-p (nth 1 it) cur-path))
                     (setq this-key (org-x-dag-get-local-property "ID")))
            ;; If parent is not a todo and we want tag inheritance, store all tags
            ;; above this headline (sans file-tags which we can get later easily)
            ;; (org-entry-get nil org-x-prop-parent-type)
            ;; TODO add the file tags here so I don't need to worry about them
            ;; later
            (setq all-tags (if (and (not this-parent-key)
                                    org-use-tag-inheritance)
                               (->> cur-path
                                    (--mapcat (nth 2 it))
                                    (append this-tags))
                             this-tags)
                  this-links (or (org-x-dag-get-parent-links)
                                 ;;(org-x-dag-get-link-property)
                                 (when (not this-parent-key) this-file-links))
                  this-meta (org-x-dag-build-meta file
                                                  this-point
                                                  this-level
                                                  (substring-no-properties this-todo)
                                                  all-tags
                                                  this-parent-key))
            (!cons (cons this-key this-meta) acc-meta)
            (!cons (cons this-key (append (list (nth 1 this-parent)) this-links))
                   acc))
          ;; Add current headline to stack
          (!cons (list this-level this-key this-tags) cur-path)))
      (setq more (= 0 (forward-line 1))))
    ;; TODO reverse these to make things make sense later?
    (list acc acc-meta)))

(defun org-x-dag-get-file-nodes (file)
  "Return all nodes in FILE in one pass."
  (org-x-with-file file
    (org-x-dag-get-buffer-nodes file org-todo-keywords-1)))

;;; DAG SYNCHRONIZATION/CONSTRUCTION

(defun org-x-dag-get-files ()
  "Return a list of all files to be used in the DAG."
  `(,(org-x-get-lifetime-goal-file)
    ,(org-x-get-endpoint-goal-file)
    ,(org-x-qtp-get-file)
    ,@(org-x-get-action-and-incubator-files)))

(defun org-x-dag-get-md5 (path)
  "Get the md5 checksum of PATH."
  (org-x-with-file path (buffer-hash)))

(defun org-x-dag-get-sync-state ()
  "Return the sync state.

The returned value will be a list like (TO-REMOVE TO-INSERT
TO-UPDATE) which will contain the file paths the should be
removed from, added to, or edited within the DAG respectively."
  (cl-flet
      ((lookup-md5
        (path)
        (alist-get path org-x-dag-sync-state nil nil #'equal)))
    (-let* ((existing-files (org-x-dag-get-files))
            (state-files (-map #'car org-x-dag-sync-state))
            (to-remove (-difference state-files existing-files))
            ((&alist 'to-insert 'to-update 'no-change)
             (->> (--map (cons it (org-x-dag-get-md5 it)) existing-files)
                  (--group-by (-let* (((path . new-md5) it)
                                      (old-md5 (lookup-md5 path)))
                                (cond
                                 ((null old-md5) 'to-insert)
                                 ((equal old-md5 new-md5) 'no-change)
                                 (t 'to-update)))))))
      (list to-remove to-insert to-update no-change))))

(defun org-x-dag-read-files (files)
  (cl-flet
      ((append-results
        (acc file)
        (-let* (((acc-ids acc-meta acc-filemaps) acc)
                ((ids metas) (org-x-dag-get-file-nodes file))
                (filemap (cons file (-map #'car ids))))
          (list (append ids acc-ids)
                (append metas acc-meta)
                (cons filemap acc-filemaps)))))
    (-reduce-from #'append-results nil files)))

(defun org-x-dag-update-ht (to-remove to-insert ht)
  (--each to-remove
    (ht-remove ht it))
  (--each to-insert
    (ht-set ht (car it) (cdr it))))

(defun org-x-dag-update-dag (to-insert to-remove)
  (let* ((dag (org-x-dag->dag))
         (dag* (if (dag-is-empty-p dag) (dag-alist-to-dag to-insert)
                 (dag-edit-nodes to-remove to-insert dag))))
    (plist-put org-x-dag :dag dag*)))

;; TODO there is a HUGE DIFFERENCE between a 'key' (the things in the hash table
;; the look things up) and a 'node' (which is a cons cell, the car of which is a
;; 'key' and the cdr of which is a 'relation'). These names suck, but the point
;; is we need to distinguish between them otherwise really strange things happen
(defun org-x-dag-update (to-remove to-insert to-update)
  "Update the DAG given files to add and remove.

TO-REMOVE, TO-INSERT, and TO-UPDATE are lists of files to remove
from, add to, and update with the DAG."
  (-let* (((&plist :id->meta :file->ids) org-x-dag)
          (files2rem (append to-update to-remove))
          (files2ins (append to-update to-insert))
          (ids2rem (org-x-dag-files->ids files2rem))
          ((ids2ins meta2ins fms2ins) (org-x-dag-read-files files2ins)))
    (org-x-dag-update-ht ids2rem meta2ins id->meta)
    (org-x-dag-update-ht files2rem fms2ins file->ids)
    (org-x-dag-update-dag ids2ins ids2rem)))

(defun org-x-dag-sync (&optional force)
  "Sync the DAG with files from `org-x-dag-get-files'.

If FORCE is non-nil, sync no matter what."
  (when force
    (setq org-x-dag-sync-state nil
          org-x-dag (org-x-dag-empty)))
  ;; TODO verify integrity somewhere in here
  (-let (((to-remove to-insert to-update no-change) (org-x-dag-get-sync-state)))
    (org-x-dag-update to-remove (-map #'car to-insert) (-map #'car to-update))
    (setq org-x-dag-sync-state (append to-insert to-update no-change))
    nil))

;; NODE FORMATTING

(defconst org-x-dag-tag-prefix-order (list org-x-tag-misc-prefix
                                           org-x-tag-resource-prefix
                                           org-x-tag-location-prefix
                                           org-x-tag-category-prefix)
  "Order in which tags should appear in the agenda buffer (from right to left.")

(defun org-x-dag-collapse-tags (tags)
  "Return TAGS with duplicates removed.

In the case of mutually exclusive tags, only the first tag
encountered will be returned."
  (-let (((x non-x) (--separate (memq (elt it 0) org-x-exclusive-prefixes) tags)))
    (->> (--group-by (elt it 0) x)
         (--map (car (cdr it)) )
         (append (-uniq non-x))
         ;; this removes the 'inherited' property on some of the tags, which
         ;; makes the agenda look cleaner (to me) since there are no
         ;; double-colons to separate inherited from non-inherited
         ;;
         ;; NOTE: this appears to have no effect on `org-agenda-tags' (eg the
         ;; inherited tags still show up in the menu properly)
         (-map #'substring-no-properties))))

(defun org-x-dag-sort-tags (tags)
  (cl-flet
      ((get-ranking
        (tag)
        (-if-let (i (-elem-index (elt tag 0) org-x-dag-tag-prefix-order))
            (1+ i)
          0)))
    (->> (--map (cons it (get-ranking it)) tags)
         (--sort (< (cdr it) (cdr other)))
         (-map #'car))))

(defun org-x-dag-prepare-tags (tags)
  (->> (org-x-dag-collapse-tags tags)
       (org-x-dag-sort-tags)))

(defun org-x-dag-add-default-props (item)
  (org-add-props item nil
    'help-echo (org-x-dag-help-echo)
    'org-not-done-regexp org-not-done-regexp
    'org-todo-regexp org-todo-regexp
    'org-complex-heading-regexp org-complex-heading-regexp
    'mouse-face 'highlight))

(defun org-x-dag-id->formatted-level (id)
  (-> (org-x-dag-id->metaprop id :level)
      (org-reduced-level)
      (make-string ?\s)))

(defun org-x-dag-help-echo ()
  (->> (or (buffer-file-name (buffer-base-buffer))
           (buffer-name (buffer-base-buffer)))
       (abbreviate-file-name)
       (format "mouse-2 or RET jump to Org file %S")))

(defun org-x-dag-headlines-get-regexp (re)
  (let ((end (save-excursion (outline-next-heading))))
    (-when-let (p (save-excursion (re-search-forward re end t)))
      (list (1- (match-beginning 1)) (match-string 1)))))

(defun org-x-dag-timestamp-to-absolute (ts)
  (->> (org-ml-get-properties '(:month-start :day-start :year-start) ts)
       (calendar-absolute-from-gregorian)))

;; TODO 'modulus' only applies to the repeater
(defun org-ml-timestamp-extract-modulus (modtype ts)
  "Return the modulus of timestamp TS for MODTYPE."
  (cl-flet
      ((convert-value
        (islongp value unit)
        (pcase unit
          ('year (* 12 value))
          ('month value)
          (_ (if islongp
                 ;; TODO make these messages not suck
                 (pcase unit
                   ('week (* 7 1440 value))
                   ('day (* 1440 value))
                   ('hour (* 60 value))
                   ('minute value)
                   (_ (error)))
               (pcase unit
                 ('week (* 7 value))
                 ('day value)
                 ((or 'hour 'minute) (message "WARNING: ..."))
                 (_ (error)))))))
       (convert-unit
        (unit)
        (if (memq unit '(year month)) 'month 'submonth)))
    (-let* ((props (pcase modtype
                     ('warning '(:warning-value :warning-unit :warning-type))
                     ('repeater '(:repeater-value :repeater-unit :repeater-type))))
            (islongp (->> (org-ml-timestamp-get-start-time ts)
                          (org-ml-time-is-long))))
      (-when-let ((value unit type) (org-ml-get-properties props ts))
        (let ((v (convert-value islongp value unit))
              (u (convert-unit unit)))
          `(,v ,u ,type))))))

(defun org-x-dag-partition-timestamp (ts)
  (list :datetime (org-ml-timestamp-get-start-time ts)
        :pos (org-ml-get-property :begin ts)
        :repeater (org-ml-timestamp-extract-modulus 'repeater ts)
        :warning (org-ml-timestamp-extract-modulus 'warning ts)))

(defmacro org-x-dag-with-times (datetime0 datetime1 form)
  ;; ASSUME all digits in this comparison are on the calendar/clock (eg day 32
  ;; does not 'rollover' to day 1 on the next month)
  (declare (indent 2))
  `(if (or (and (org-ml-time-is-long ,datetime0)
                (org-ml-time-is-long ,datetime1))
           (not (or (org-ml-time-is-long ,datetime0)
                    (org-ml-time-is-long ,datetime1))))
       ,form
     (error "Datetimes are invalid lengths: %S and %S" ,datetime0 ,datetime1)))

;; TODO this function name sucks, call it datetime
(defun org-x-dag-date< (datetime0 datetime1)
  (org-x-dag-with-times datetime0 datetime1
    (-when-let (next (->> (-zip-with #'cons datetime0 datetime1)
                          (--drop-while (= (car it) (cdr it)))
                          (car)))
      (< (car next) (cdr next)))))

(defun org-x-dag-date= (datetime0 datetime1)
  (org-x-dag-with-times datetime0 datetime1
    (->> (-zip-with #'cons datetime0 datetime1)
         (--drop-while (= (car it) (cdr it)))
         (not))))

(defun org-x-dag-time-shift (datetime shift unit)
  (cl-flet*
      ((enc-dec-long
        (y m d H M)
        (-let (((_ M* H* d* m* y* _ _ _)
                (->> (list 0 M H d m y nil nil (current-time-zone))
                     (encode-time)
                     (decode-time))))
          (list y* m* d* H* M*)))
       (enc-dec-short
        (y m d)
        (-take 3 (enc-dec-long y m d 0 0))))
    (pcase datetime
      ((or `(,y ,m ,d) `(,y ,m ,d nil nil))
       (pcase unit
         ('month (enc-dec-short y (+ m shift) d))
         ('submonth (enc-dec-short y m (+ d shift)))))
      (`(,y ,m ,d ,H ,M)
       (pcase unit
         ('month (enc-dec-long y (+ m shift) d H M))
         ('submonth (enc-dec-long y m d H (+ M shift))))))))

(defun org-x-dag-repeater-get-next (sel-datetime datetime shift shifttype reptype)
  "Return the next timestamp repeater of DATETIME."
  (pcase reptype
    ('catch-up
     ;; Next time is a multiple of repeater in the future relative to the base
     ;; time; shift one interval at a time since they may not be spaced evenly
     ;; (DST, leap year, different days in each month, etc). Think of this like
     ;; a path function from p-chem; shifting 3 months once might be different
     ;; than shifting by 1 month three times.
     (let ((next datetime)
           (pastp t))
       (while pastp
         (setq next (org-x-dag-time-shift next shift shifttype)
               pastp (org-x-dag-date< next sel-datetime)))
       next))
    ('restart
     ;; Next time is one repeater interval after now
     ;;
     ;; ASSUME cur needs to match the length of time
     (org-x-dag-time-shift sel-datetime shift shifttype))
    ('cumulate
     ;; Next time is one repeater interval after the base timestamp
     (org-x-dag-time-shift datetime shift shifttype))))

(defun org-x-dag-unfold-timestamp (cur datetime rep future-limit)
  "Return all timestamps associated with DATETIME.

If REP is nil, return a singleton list just containing DATETIME.
If REP is non-nil, return DATETIME and all repeaters up until
FUTURE-LIMIT in a list."
  ;; ASSUME pts and future-limit are both long or short timestamps
  (unless (org-x-dag-date< future-limit datetime)
    (pcase rep
      (`nil `(,datetime))
      (`(,value ,unit ,reptype)
       (->> (org-x-dag-repeater-get-next cur datetime value unit reptype)
            (--unfold (unless (org-x-dag-date< future-limit it)
                        (cons it (org-x-dag-time-shift it value unit))))
            (cons datetime))))))

(defun org-x-dag-date-add-time (date)
  (-let (((_ M H) (decode-time (current-time))))
    `(,@date ,H ,M)))

(defun org-x-dag-get-scheduled-at (sel-date pts)
  (-let* (((&plist :datetime d :repeater r) pts)
          (islongp (org-ml-time-is-long d))
          (future-limit (if islongp `(,@sel-date 23 59) sel-date))
          (sel-datetime (if islongp (org-x-dag-date-add-time sel-date) sel-date)))
    (org-x-dag-unfold-timestamp sel-datetime d r future-limit)))

(defun org-x-dag-get-deadlines-at (sel-date pts)
  (-let* (((&plist :datetime d :repeater r :warning w) pts)
          (islongp (org-ml-time-is-long d))
          ((warn-shift warn-shifttype)
           (if w w
             (let ((f (if islongp 1440 1)))
               `(,(* f org-deadline-warning-days) submonth))))
          (sel-datetime (if islongp (org-x-dag-date-add-time sel-date) sel-date))
          (future-limit (org-x-dag-time-shift sel-datetime warn-shift warn-shifttype)))
    (org-x-dag-unfold-timestamp sel-datetime d r future-limit)))

(defun org-x-dag-headline-get-planning ()
  (let ((end (save-excursion (outline-next-heading))))
    (save-excursion
      (when (re-search-forward org-planning-line-re end t)
        ;; TODO this is rather slow since I'm using a general org-ml parsing
        ;; function; I'm also not even using the match results from the planning
        ;; line re, which might be useful
        (-let* ((pl (org-ml-parse-this-element)))
          (->> (org-ml-get-properties '(:deadline :scheduled) pl)
               (--map (-some-> it (org-x-dag-partition-timestamp)))))))))

(defun org-x-dag-format-tag-node (category tags key)
  ;; ASSUME I don't use subtree-level categories
  (-let* (;; (category (org-get-category))
          (tags* (org-x-dag-prepare-tags tags))
          (head (org-get-heading))
          (level (org-x-dag-id->formatted-level key))
          (marker (org-agenda-new-marker))
          ;; no idea what this function actually does
          ((ts . ts-type) (org-agenda-entry-get-agenda-timestamp (point)))
          (item (org-agenda-format-item "" head level category tags*))
          (priority (org-get-priority item)))
    (-> (org-x-dag-add-default-props item)
        (org-add-props nil
            ;; face
            'face 'default
            'done-face 'org-agenda-done
            'undone-face 'default
            ;; marker
            'org-hd-marker marker
            'org-marker marker
            ;; headline stuff
            'todo-state (org-x-dag-id->todo key)
            'priority priority
            'ts-date ts
            ;; misc
            'type (concat "tagsmatch" ts-type)))))

(defun org-x-dag-date-to-gregorian (date)
  (-let (((y m d) date))
    `(,m ,d ,y)))

(defun org-x-dag-time-get-clock-time (time)
  (when (org-ml-time-is-long time) (list (nth 3 time) (nth 4 time))))

(defun org-x-dag-time-partition (time)
  (if (org-ml-time-is-long time) (-split-at 3 time) `(,(-take 3 time) nil)))

(defun org-x-dag-date-diff (date0 date1)
  ""
  (pcase (list date0 date1)
    (`((,y0 ,m0 ,d0) (,y1 ,m1 ,d1))
     (- (calendar-absolute-from-gregorian `(,m0 ,d0 ,y0))
        (calendar-absolute-from-gregorian `(,m1 ,d1 ,y1))))
    (_ (error "Invalid date format(s): %S or %S" date0 date1))))

(defun org-x-dag-format-item (id extra category tags time)
  (let* ((tags* (org-x-dag-prepare-tags tags))
         (head (org-get-heading))
         (level (org-x-dag-id->formatted-level id))
         (todo-state (org-x-dag-id->todo id))
         (time-str (-some->> time (apply #'format "%02i:%02i ")))
         (item (org-agenda-format-item extra head level category tags* time-str))
         ;; TODO why am I getting the priority after sending the headline
         ;; through some crazy formatting function?
         (priority (org-get-priority item)))
    (-> (org-x-dag-add-default-props item)
        (org-add-props nil
            'todo-state todo-state
            'priority priority))))

(defun org-x-dag-planning-props (id face pos date ts-date type)
  (cl-flet
      ((to-abs
        (date)
        (->> (org-x-dag-date-to-gregorian date)
             (calendar-absolute-from-gregorian))))
    (list
     ;; face
     'face (if (org-x-dag-id->is-done-p id) 'org-agenda-done face)
     'undone-face face
     'done-face 'org-agenda-done
     ;; marker
     'org-hd-marker (org-agenda-new-marker)
     'org-marker (org-agenda-new-marker pos)
     ;; headline stuff
     'date (to-abs date)
     'ts-date (to-abs ts-date)
     'type type)))

(defun org-x-dag-format-scheduled-node (sel-date pos datetime category tags id)
  (-let* (((this-date this-time) (org-x-dag-time-partition datetime))
          (diff (org-x-dag-date-diff this-date sel-date))
          (pastp (< diff 0))
          (todayp (= diff 0))
          ;; hopefully this is right...if it is this seems silly
          (extra (-let (((today past) org-agenda-scheduled-leaders))
                   (cond (todayp today)
                         (pastp (format past (- diff)))
                         (t "")))) ;; This should never actually be used
          (face (cond (pastp 'org-scheduled-previously)
                      (todayp 'org-scheduled-today)
                      (t 'org-scheduled)))
          ((date type) (if pastp `(,this-date "past-scheduled")
                         `(,sel-date "scheduled")))
          (props (org-x-dag-planning-props id face pos date this-date type)))
    ;; NOTE: I don't care about habits, no need to consider them
    (-> (org-x-dag-format-item id extra category tags this-time)
        (org-add-props props))))

(defun org-x-dag-format-deadline-node (sel-date pos datetime category tags id)
  (-let* (((this-date this-time) (org-x-dag-time-partition datetime))
          (diff (org-x-dag-date-diff this-date sel-date))
          (pastp (< diff 0))
          (futurep (< 0 diff))
          (extra (-let* (((now future past) org-agenda-deadline-leaders))
                   (cond
                    (futurep (format future diff))
                    (pastp (format past diff))
                    (t now))))
          ;; TODO the stock deadline formatter uses the warning time to
          ;; determine this based on percentage; I'm lazy and don't feel like
          ;; doing that (now) but I might in the future
          (face (cond
                 ((< 5 diff) 'org-upcoming-distant-deadline)
                 ((< 1 diff) 'org-upcoming-deadline)
                 (t 'org-warning)))
          ((date type) (if futurep `(,sel-date "upcoming-deadline")
                         `(,this-date "deadline")))
          (props (org-x-dag-planning-props id face pos date this-date type)))
    (-> (org-x-dag-format-item id extra category tags this-time)
        (org-add-props props))))

;;; ID FUNCTIONS

;; ranking

(defmacro org-x-dag-ids-rank (form ids)
  (declare (indent 1))
  `(cl-labels
       ((compare
         (a b)
         (cond
          ((not (or a b)) t)
          ((= (car a) (car b)) (compare (cdr a) (cdr b)))
          (t (> (car a) (car b))))))
     (->> (--map (cons it ,form) ,ids)
          (--sort (compare (cdr it) (cdr other))))))
  
(defmacro org-x-dag-ids-rank-by-children (form ids)
  `(org-x-dag-ids-rank
       (let ((it (org-x-dag-id->children it)))
         ,form)
     ,ids))

(defmacro org-x-dag-ids-rank-by-parents (form ids)
  `(org-x-dag-ids-rank
       (let ((it (org-x-dag-id->parents it)))
         ,form)
     ,ids))

;; reductions

;; TODO this is a naive approach that will effectively expand the dag into
;; a tree for nodes that share common children/parents. I might want to handle
;; these special cases in a better way (example, 'summation' could count nodes
;; multiple times, which may or may not make sense)
(defmacro org-x-dag--id-reduce (id-getter branch-form leaf-form init id)
  (declare (indent 1))
  (let ((cs (make-symbol "--children")))
    `(cl-labels
         ((reduce
           (acc it)
           (-if-let (,cs (,id-getter ,id))
               (--reduce-from (reduce acc it) ,branch-form ,cs)
             ,leaf-form)))
       (reduce ,init ,id))))

(defmacro org-x-dag-id-reduce-down (branch-form leaf-form init id)
  `(org-x-dag--id-reduce org-x-dag-id->children
     ,branch-form ,leaf-form ,init ,id))

(defmacro org-x-dag-id-reduce-up (branch-form leaf-form init id)
  `(org-x-dag--id-reduce org-x-dag-id->parents
     ,branch-form ,leaf-form ,init ,id))

;;; HEADLINE PREDICATES
;;
;; The following are predicates that require the point to be above the
;; headline in question

(defun org-x-headline-has-timestamp (re want-time)
  (let ((end (save-excursion (outline-next-heading))))
    (-when-let (p (save-excursion (re-search-forward re end t)))
      (if want-time (org-2ft (match-string 1)) p))))

(defun org-x-dag-headline-is-deadlined-p (want-time)
  (org-x-headline-has-timestamp org-deadline-time-regexp want-time))

(defun org-x-dag-headline-is-scheduled-p (want-time)
  (org-x-headline-has-timestamp org-scheduled-time-regexp want-time))

(defun org-x-dag-headline-is-closed-p (want-time)
  (org-x-headline-has-timestamp org-closed-time-regexp want-time))

(defun org-x-dag-is-created-p (want-time)
  (save-excursion
    (-when-let (ts (org-x-dag-get-local-property org-x-prop-created))
      (if want-time (org-2ft ts) t))))

(defun org-x-dag-headline-is-iterator-p ()
  (save-excursion
    (->> (org-x-dag-get-local-property org-x-prop-parent-type)
         (equal org-x-prop-parent-type-iterator))))

(defconst org-x-headline-task-status-priorities
  '((:archivable . -1)
    (:complete . -1)
    (:expired . 0)
    (:done-unclosed . 0)
    (:undone-closed . 0)
    (:active . 1)
    (:inert . 2)))

(defconst org-x-project-status-priorities
  '((:archivable . -1)
    (:complete . -1)
    (:scheduled-project . 0)
    (:invalid-todostate . 0)
    (:undone-complete . 0)
    (:done-incomplete . 0)
    (:stuck . 0)
    (:wait . 1)
    (:held . 2)
    (:active . 3)
    (:inert . 4)))

(defun org-x-dag-time-is-archivable-p (epochtime)
  (< (- (float-time) epochtime) (* 60 60 24 org-x-archive-delay)))

;;; STATUS DETERMINATION

(defmacro org-x-dag-with-id (key &rest body)
  (declare (indent 1))
  `(progn
     (goto-char (org-x-dag-id->point ,key))
     ,@body))

(defmacro org-x-dag-with-id-in-file (id &rest body)
  (declare (indent 1))
  `(org-x-with-file (org-x-dag-id->file ,id)
     (org-x-dag-with-id ,id ,@body)))

(defun org-x-headline-get-task-status-0 (kw)
  (if (member kw org-x-done-keywords)
      (-if-let (c (org-x-dag-headline-is-closed-p t))
          (if (org-x-dag-time-is-archivable-p c)
              :archivable
            :complete)
        :done-unclosed)
    (cond
     ((org-x-headline-is-expired-p) :expired)
     ((org-x-headline-is-inert-p) :inert)
     ((org-x-dag-headline-is-closed-p nil) :undone-closed)
     (t :active))))

(defun org-x-dag-get-max-index (ys xs)
  "Return the member of XS that has the highest index in YS."
  (--max-by (> (-elem-index it ys) (-elem-index other ys)) xs))

(defmacro org-x-dag-descend-into-project (keys parent-tags codetree task-form
                                               callback)
  (declare (indent 2))
  (let ((allowed-codes (-map #'car codetree))
        (trans-tbl (--mapcat (-let (((a . bs) it))
                               (--map (cons it a) bs))
                             codetree)))
    `(cl-flet
         ((get-project-or-task-status
           (key)
           (-if-let (children (org-x-dag-id->headline-children key))
               (let* ((tags (org-x-dag-id->tags nil ,parent-tags key))
                      (child-results (funcall ,callback key tags children))
                      ;; ASSUME the car of the results will be the toplevel
                      ;; key/status pair for this (sub)project
                      (top-status (plist-get (car child-results) :status))
                      (top-status* (if (member top-status ',allowed-codes)
                                       top-status
                                     (alist-get top-status ',trans-tbl))))
                 (cons top-status* child-results))
             (let ((it-kw (org-x-dag-id->todo key)))
               (org-x-dag-with-id key
                 (-> ,task-form
                     (nth ',allowed-codes)
                     (list)))))))
       (let* ((results (-map #'get-project-or-task-status ,keys))
              (status (->> (-map #'car results)
                           (org-x-dag-get-max-index ',allowed-codes))))
         (cons status (-mapcat #'cdr results))))))

(defun org-x-dag-headline-get-project-status (key tags children)
  ;; ASSUME children will always be at least 1 long
  (org-x-dag-with-id key
    (let ((keyword (org-x-dag-id->todo key)))
      (-let (((status . child-results)
              (cond
               ((org-x-dag-headline-is-scheduled-p nil)
                (list :scheduled-project))
               ((equal keyword org-x-kw-hold)
                (list (if (org-x-headline-is-inert-p) :inert :held)))
               ((member keyword org-x--project-invalid-todostates)
                (list :invalid-todostate))
               ((equal keyword org-x-kw-canc)
                (list (if (org-x-headline-is-archivable-p) :archivable :complete)))
               ((equal keyword org-x-kw-done)
                (org-x-dag-descend-into-project children tags
                  ((:archivable)
                   (:complete)
                   (:done-incomplete :stuck :inert :held :wait :active
                                     :scheduled-project :invalid-todostate
                                     :undone-complete))
                  (if (member it-kw org-x-done-keywords)
                      (if (org-x-headline-is-archivable-p) 0 1)
                    2)
                  #'org-x-dag-headline-get-project-status))
               ((equal keyword org-x-kw-todo)
                (org-x-dag-descend-into-project children tags
                  ((:undone-complete :complete :archivable)
                   (:stuck :scheduled-project :invalid-todostate
                           :done-incomplete)
                   (:held)
                   (:wait)
                   (:inert)
                   (:active))
                  (cond
                   ((and (not (member it-kw org-x-done-keywords))
                         (org-x-headline-is-inert-p))
                    4)
                   ((equal it-kw org-x-kw-todo)
                    (if (org-x-dag-headline-is-scheduled-p nil) 5 1))
                   ((equal it-kw org-x-kw-hold)
                    2)
                   ((equal it-kw org-x-kw-wait)
                    3)
                   ((equal it-kw org-x-kw-next)
                    5)
                   (t 0))
                  #'org-x-dag-headline-get-project-status))
               (t (error "Invalid keyword detected: %s" keyword)))))
        (cons (list :key key :status status :tags tags) child-results)))))

(defun org-x-dag-headline-get-iterator-project-status (key children)
  (org-x-dag-with-id key
    (let* ((kw (org-x-dag-id->todo key))
           (status
            (cond
             ((or (member kw org-x--project-invalid-todostates)
                  (org-x-dag-headline-is-scheduled-p nil))
              (list :project-error))
             ((equal kw org-x-kw-canc)
              (list :empt))
             ;; TODO this is a bit awkward since I don't care about the child statuses
             ;; and I don't care about tags
             ((equal kw org-x-kw-done)
              (org-x-dag-descend-into-project children nil
                ((:empt)
                 (:project-error :unscheduled :actv))
                (if (member it-kw org-x-done-keywords) 0 1)
                ;; TODO this has an argument mismatch
                #'org-x-dag-headline-get-iterator-project-status))
             ((equal kw org-x-kw-todo)
              (org-x-dag-descend-into-project children nil
                ((:unscheduled :project-error)
                 (:empt)
                 (:actv))
                ;; TODO this triggers a compiler warning because I don't use
                ;; `it-kw'
                (let ((ts (org-x-dag-headline-is-scheduled-p t)))
                  (cond
                   ((not ts) 0)
                   ((> org-x-iterator-active-future-offset (- ts (float-time))) 1)
                   (t 2)))
                #'org-x-dag-headline-get-iterator-project-status))
             (t (error "Invalid keyword detected: %s" kw)))))
      status)))

(defun org-x-dag-headline-get-iterator-task-status (key)
  (org-x-dag-with-id key
    (if (org-x-dag-id->is-done-p key) :empt
      (-if-let (ts (or (org-x-dag-headline-is-scheduled-p t)
                       (org-x-dag-headline-is-deadlined-p t)))
          (if (< org-x-iterator-active-future-offset (- ts (float-time)))
              :actv
            :empt)
        :unscheduled))))

(defun org-x-dag-headline-get-iterator-status (key)
  (cl-flet
      ((get-status
        (key)
        (-if-let (children (org-x-dag-id->headline-children key))
            (->> children
                 (org-x-dag-headline-get-iterator-project-status key)
                 (car))
          (org-x-dag-headline-get-iterator-task-status key))))
    (->> (org-x-dag-id->headline-children key)
         (-map #'get-status)
         (org-x-dag-get-max-index org-x--iter-statuscodes))))

;;; SCANNERS
;;
;; Not sure what to call these, they convert the DAG to a list of agenda strings

(defmacro org-x-dag-with-files (files pre-form form)
  (declare (indent 2))
  (let* ((lookup-form '(ht-get file->ids it-file))
         (pre-form* (if pre-form
                        `(--filter ,pre-form ,lookup-form)
                      lookup-form)))
    `(-let (((&plist :file->ids) org-x-dag))
       (cl-flet
           ((proc-file
             (it-file)
             (-when-let (keys ,pre-form*)
               (org-x-with-file it-file
                 ;; NOTE there are other ways in org to get the category; the
                 ;; only one I ever cared about was the filename. Very simple,
                 ;; category = filename. Done
                 (let ((it-category (f-base it-file)))
                   (--mapcat ,form keys))))))
         (-non-nil (-mapcat #'proc-file ,files))))))

(defun org-x-dag-scan-projects ()
  (cl-flet*
      ((format-result
        (cat result)
        (-let* (((&plist :key :status :tags) result)
                (priority (alist-get status org-x-project-status-priorities)))
          (when (>= priority 0)
            (org-x-dag-with-id key
              (-> (org-x-dag-format-tag-node cat tags key)
                  (org-add-props nil
                      'x-toplevelp (org-x-dag-id->is-toplevel-p key)
                      'x-status status
                      'x-priority priority))))))
       (format-key
        (cat key)
        (let ((tags (org-x-dag-id->tags t org-file-tags key)))
          ;; TODO don't hardcode these things
          (org-x-dag-with-id key
            (unless (or (member org-x-tag-incubated tags)
                        (org-x-dag-headline-is-iterator-p))
              (-some->> (org-x-dag-id->headline-children key)
                (org-x-dag-headline-get-project-status key tags)
                (--map (format-result cat it))))))))
    (org-x-dag-with-files (org-x-get-action-files)
        (and (org-x-dag-id->is-toplevel-p it)
             (not (org-x-dag-id->is-done-p it)))
      (format-key it-category it))))

(defun org-x-dag-scan-iterators ()
  (cl-flet*
      ((format-result
        (tags cat key)
        (-let ((status (org-x-dag-headline-get-iterator-status key)))
          (org-x-dag-with-id key
            (-> (org-x-dag-format-tag-node cat tags key)
                (org-add-props nil
                    'x-status status))))))
    (org-x-dag-with-files (org-x-get-action-files)
        (org-x-dag-id->is-toplevel-p it)
      (let ((tags (org-x-dag-id->tags t org-file-tags it)))
        (unless (member org-x-tag-incubated tags)
          (org-x-dag-with-id it
            (when (org-x-dag-headline-is-iterator-p)
              (list (format-result tags it-category it)))))))))
  
(defun org-x-dag-get-task-nodes (id)
  (declare (indent 2))
  (cl-labels
      ((descend
        (children)
        ;; TODO don't hardcode this
        (->> (--remove (member (org-x-dag-id->todo it)
                               (list org-x-kw-canc org-x-kw-hold))
                       children)
             (--mapcat (-if-let (cs (org-x-dag-id->headline-children it))
                           (descend cs)
                         (list it))))))
    (-some-> (org-x-dag-id->headline-children id)
      (descend))))

(defun org-x-dag-scan-tasks ()
  (cl-flet
      ((format-key
        (category is-standalone key)
        (let ((tags (org-x-dag-id->tags t org-file-tags key)))
          ;; filter out incubators
          (org-x-dag-with-id key
            (unless (or (member org-x-tag-incubated tags)
                        (org-x-dag-headline-is-scheduled-p nil)
                        (org-x-dag-headline-is-deadlined-p nil))
              (let* ((s (org-x-headline-get-task-status-0 (org-x-dag-id->todo key)))
                     (p (alist-get s org-x-headline-task-status-priorities)))
                (unless (= p -1)
                  (-> (org-x-dag-format-tag-node category tags key)
                      (org-add-props nil
                          'x-is-standalone is-standalone
                          'x-status s)))))))))
    (org-x-dag-with-files (org-x-get-action-files)
        (org-x-dag-id->is-toplevel-p it)
      (-if-let (project-tasks (org-x-dag-get-task-nodes it))
          (--map (format-key it-category nil it) project-tasks)
        (list (format-key it-category t it))))))

(defun org-x-dag-scan-incubated ()
  (cl-flet
      ((format-key
        (category key)
        (let ((tags (org-x-dag-id->tags t org-file-tags key)))
          (when (member org-x-tag-incubated tags)
            (org-x-dag-with-id key
              (let* ((sch (org-x-dag-headline-is-scheduled-p t))
                     (dead (org-x-dag-headline-is-deadlined-p t))
                     (is-project (org-x-dag-id->headline-children key)))
                (-> (org-x-dag-format-tag-node category tags key)
                    (org-add-props nil
                        'x-project-p is-project
                        'x-scheduled sch
                        'x-deadlined dead))))))))
    (org-x-dag-with-files (org-x-get-action-and-incubator-files)
        (and (org-x-dag-id->is-toplevel-p it)
             (not (org-x-dag-id->is-done-p it)))
      (list (format-key it-category it)))))

(defun org-x-dag-scan-archived ()
  (cl-flet
      ((format-key
        (category key)
        (let ((tags (org-x-dag-id->tags t org-file-tags key)))
          (unless (member org-x-tag-incubated tags)
            (org-x-dag-with-id key
              (-let (((is-archivable is-project)
                      (-if-let (children (org-x-dag-id->headline-children key))
                          (-> (org-x-dag-headline-get-project-status key tags children)
                              (alist-get org-x-project-status-priorities)
                              (eq :archivable)
                              (list t))
                        (-> (org-x-headline-get-task-status-0 (org-x-dag-id->todo key))
                            (alist-get org-x-headline-task-status-priorities)
                            (eq :archivable)
                            (list t)))))
                (when is-archivable
                  (-> (org-x-dag-format-tag-node category tags key)
                      (org-add-props nil
                          'x-project-p is-project)))))))))
    (org-x-dag-with-files (org-x-get-action-files)
        (org-x-dag-id->is-toplevel-p it)
      (org-x-dag-with-id it
        (if (org-x-dag-headline-is-iterator-p)
            (->> (org-x-dag-id->headline-children it)
                 (--map (format-key it-category it)))
          (list (format-key it-category it)))))))

(defun org-x-dag-scan-ltgs ()
  (let ((child-files (append (list (org-x-get-lifetime-goal-file)
                                   (org-x-get-endpoint-goal-file))
                             (org-x-get-action-files))))
    (cl-flet
        ((format-id
          (category id)
          (let ((toplevelp (org-x-dag-id->is-toplevel-p id))
                (has-children (->> (org-x-dag-id->children id)
                                   (--any-p (member (org-x-dag-id->file it)
                                                    child-files))))
                (tags (org-x-dag-id->tags t nil id)))
            (-> (org-x-dag-format-tag-node category tags id)
                (org-add-props nil
                    'x-goal-status (list :type 'ltg
                                         :childlessp (not has-children)
                                         :toplevelp toplevelp
                                         :parentlessp nil))))))
      (org-x-dag-with-files (list (org-x-get-lifetime-goal-file))
          nil
        (org-x-dag-with-id it
          (list (format-id it-category it)))))))

(defun org-x-dag-scan-epgs ()
  (let ((child-files (cons (org-x-get-endpoint-goal-file)
                           (org-x-get-action-files)))
        (parent-files (list (org-x-get-endpoint-goal-file)
                            (org-x-get-lifetime-goal-file))))
    (cl-flet
        ((format-id
          (category id)
          (let ((toplevelp (org-x-dag-id->is-toplevel-p id))
                (has-children (->> (org-x-dag-id->children id)
                                   (--any-p (member (org-x-dag-id->file it)
                                                    child-files))))
                (has-parents (->> (org-x-dag-id->parents id)
                                  (--any-p (member (org-x-dag-id->file it)
                                                   parent-files))))
                (tags (org-x-dag-id->tags t nil id)))
            (-> (org-x-dag-format-tag-node category tags id)
                (org-add-props nil
                    'x-goal-status (list :type 'epg
                                         :childlessp (not has-children)
                                         :toplevelp toplevelp
                                         :parentlessp (not has-parents)))))))
      (org-x-dag-with-files (list (org-x-get-endpoint-goal-file))
          nil
        (org-x-dag-with-id it
          (list (format-id it-category it)))))))

(defun org-x-dag-scan-goals ()
  (append (org-x-dag-scan-ltgs) (org-x-dag-scan-epgs)))

(defun org-x-dag-scan-errors ()
  (cl-flet
      ((format-id
        (category id)
        (org-x-dag-with-id id
          (-when-let (error-type
                      (if (org-x-dag-headline-is-iterator-p)
                          (unless (org-x-dag-get-local-property "ARCHIVE")
                            :missing-archive)
                        (-if-let (created (org-x-dag-is-created-p t))
                            (when (<= (float-time) created)
                              :future-created)
                          :missing-created)))
            (-> (org-x-dag-format-tag-node category nil id)
                (org-add-props nil
                    'x-error error-type))))))
    (org-x-dag-with-files (org-x-dag-get-files)
        (not (org-x-dag-id->is-done-p it))
      (org-x-dag-with-id it
        (list (format-id it-category it))))))

(defun org-x-dag-scan-agenda (sel-date)
  (cl-flet*
      ((format-timestamps
        (todayp sel-date cat id pts get-datetimes-fun format-datetime-fun)
        (-when-let (datetimes (funcall get-datetimes-fun sel-date pts))
          (let ((tags (org-x-dag-id->tags t org-file-tags id)))
            (unless (member org-x-tag-incubated tags)
              (-let (((&plist :pos) pts)
                     (donep (org-x-dag-id->is-done-p id)))
                (--> datetimes
                     (--remove (and donep (not (org-x-dag-date= (-take 3 it) sel-date))) it)
                     (if (not todayp) (--remove (org-x-dag-date< (-take 3 it) sel-date) it) it)
                     (--map (funcall format-datetime-fun sel-date pos it cat tags id) it)))))))
       (format-id
        (todayp cat id)
        (org-x-dag-with-id id
          (-when-let (res (org-x-dag-headline-get-planning))
            (-let (((dead sched) res))
              (append
               (when dead
                 (format-timestamps todayp sel-date cat id dead
                                    #'org-x-dag-get-deadlines-at
                                    #'org-x-dag-format-deadline-node))
               (when sched
                 (format-timestamps todayp sel-date cat id sched
                                    #'org-x-dag-get-scheduled-at
                                    #'org-x-dag-format-scheduled-node))))))))
    (org-x-dag-with-files (org-x-get-action-files)
        nil
      (let ((todayp (->> (org-x-dag-date-to-gregorian sel-date)
                         (calendar-absolute-from-gregorian)
                         (= (org-today)))))
        (format-id todayp it-category it)))))

(defun org-x-dag-scan-quarterly-plan ()
  (cl-flet
      ((format-id
        (category id)
        (-> (org-x-dag-format-tag-node category nil id)
            (org-add-props nil))))
    (org-x-dag-with-files (list (org-x-qtp-get-file))
        nil
      (org-x-dag-with-id it
        (list (format-id it-category it))))))

;;; AGENDA VIEWS

;; (defun org-x-dag-show-tasks (_match)
;;   (org-x-dag-sync t)
;;   ;; hack to make the loop only run once
;;   (let ((org-agenda-files (list (car (org-x-get-action-files)))))
;;     (nd/with-advice
;;         (('org-scan-tags :override (lambda (&rest _) (org-x-dag-scan-tasks))))
;;       (org-tags-view '(4) "TODO"))))

(defun org-x-dag-show-nodes (get-nodes)
  (org-x-dag-sync)
  (let* ((org-tags-match-list-sublevels org-tags-match-list-sublevels)
         (completion-ignore-case t))
    (catch 'exit
      (org-agenda-prepare (concat "DAG-TAG"))
      (org-compile-prefix-format 'tags)
      (org-set-sorting-strategy 'tags)
      (let ((org-agenda-redo-command `(org-x-dag-show-nodes ',get-nodes))
            (rtnall (funcall get-nodes)))
        (org-agenda--insert-overriding-header
          (with-temp-buffer
            (insert "Headlines with TAGS match: \n")
            (add-text-properties (point-min) (1- (point))
                                 (list 'face 'org-agenda-structure))
            (buffer-string)))
        (org-agenda-mark-header-line (point-min))
        (when rtnall
          (insert (org-agenda-finalize-entries rtnall 'tags) "\n"))
        (goto-char (point-min))
        (or org-agenda-multi (org-agenda-fit-window-to-buffer))
        (add-text-properties
         (point-min) (point-max)
         `(org-agenda-type tags
                           org-last-args (,get-nodes)
                           org-redo-cmd ,org-agenda-redo-command
                           org-series-cmd ,org-cmd))
        (org-agenda-finalize)
        (setq buffer-read-only t)))))

;; make the signature exactly like `org-agenda-list' ...for now
(defun org-x-dag-show-daily-nodes (&optional _ start-day _ _)
  (org-x-dag-sync)
  (-let ((completion-ignore-case t)
         ;; TODO not sure if this if thing is actually necessary
         ((arg start-day span with-hour) (or org-agenda-overriding-arguments
                                     (list nil start-day 'day nil))))
    (catch 'exit
      (org-agenda-prepare "DAG-DAILY")
      (org-compile-prefix-format 'agenda)
      (org-set-sorting-strategy 'agenda)
      (-let* ((today (org-today))
              (sd (or start-day today))
              (org-agenda-redo-command
               `(org-x-dag-show-daily-nodes 'nil ,start-day ',span ,with-hour))
              ((m d y) (calendar-gregorian-from-absolute sd))
              (rtnall (org-x-dag-scan-agenda `(,y ,m ,d))))
        (setq-local org-starting-day sd)
        (setq-local org-arg-loc arg)
        ;; TODO just day (for now)
        (setq-local org-agenda-current-span span)
        (org-agenda--insert-overriding-header
          (with-temp-buffer
            (insert (format "Agenda for %d-%02d-%02d: \n" y m d))
            (add-text-properties (point-min) (1- (point))
                                 (list 'face 'org-agenda-structure))
            (buffer-string)))
        (org-agenda-mark-header-line (point-min))
        ;; TODO handle time grid here somehow
        (-some--> (org-agenda-add-time-grid-maybe rtnall 1 (= sd today))
          (org-agenda-finalize-entries it 'agenda)
          (insert it "\n"))
        (goto-char (point-min))
        (or org-agenda-multi (org-agenda-fit-window-to-buffer))
        (add-text-properties
         (point-min) (point-max)
         `(org-agenda-type agenda
                           org-last-args (,arg ,start-day ,span)
                           org-redo-cmd ,org-agenda-redo-command
                           org-series-cmd ,org-cmd))
        (org-agenda-finalize)
        (setq buffer-read-only t)))))

;;; PARENT LINK FUNCTONS

(defconst org-x-drwr-parent-links "X_PARENT_LINKS")

(defun org-x-dag-headline-get-parent-links (headline)
  (cl-flet
      ((parse-item
        (item)
        (let ((first (car (org-ml-item-get-paragraph item))))
          (if (and (org-ml-is-type 'link first)
                   (equal (org-ml-get-property :type first) "id"))
              (->> (org-ml-get-children first)
                   (-map #'org-ml-to-trimmed-string)
                   (apply #'concat)
                   (cons (org-ml-get-property :path first)))
            (error "Invalid link node: %S" first)))))
    (-when-let (first (->> headline
                           (org-ml-headline-get-contents (org-x-logbook-config))
                           (--find (org-x--is-drawer-with-name org-x-drwr-parent-links it))
                           (org-ml-get-children)
                           (car)))
      (if (org-ml-is-type 'plain-list first)
          (->> (org-ml-get-children first)
               (-map #'parse-item))
        (error "Invalid parent link drawer")))))

(defun org-x-dag-build-parent-link-drawer (ids)
  (->> (-map #'org-x-dag-id->link-item ids)
       (apply #'org-ml-build-plain-list)
       (org-ml-build-drawer "X_PARENT_LINKS")))

(defun org-x-dag-headline-set-parent-links (ids headline)
  (org-ml-headline-map-contents* (org-x-logbook-config)
    (-if-let (i (--find-index (org-x--is-drawer-with-name
                               org-x-drwr-parent-links it)
                              it))
        (let ((d (nth i it))
              (pl (->> (-map #'org-x-dag-build-parent-link-item ids)
                       (apply #'org-ml-build-plain-list))))
          (-replace-at i (org-ml-set-children (list pl) d) it))
      (cons (org-x-dag-build-parent-link-drawer ids) it))
    headline))

(defmacro org-x-dag-headline-map-parent-links* (form headline)
  (let ((h (make-symbol "--headline")))
    `(let* ((,h ,headline)
            (it (org-x-dag-headline-get-parent-links ,h)))
       (org-x-dag-headline-set-parent-links ,form ,h))))

(defun org-x-dag-headline-add-parent-link (id desc headline)
  (org-x-dag-headline-map-parent-links* (cons (cons id desc) it) headline))

(defun org-x-dag-headline-remove-parent-link (id headline)
  (org-x-dag-headline-map-parent-links*
   (--remove-first (equal (car it) id) it)
   headline))

;;; ALLOCATION

(defun org-x-dag-quarter-to-date (quarter)
  (-let (((y q) quarter))
    (list y (1+ (* q 3)) 1)))

(defun org-x-dag-shift-quarter (quarter n unit)
  (-let (((y q) quarter))
    (pcase unit
      (`year `(,(+ n y) ,q))
      (`quarter
       (let* ((x (+ q n))
              (q* (mod x 4))
              (y* (+ y (floor (/ x 4.0)))))
         `(,y* ,q*))))))

(defun org-x-dag-quarter-diff (quarter1 quarter2)
  (cl-flet
      ((qt-to-abs
        (q)
        (->> (org-x-dag-quarter-to-date q)
             (org-x-dag-date-to-gregorian)
             (calendar-absolute-from-gregorian))))
    (- (qt-to-abs quarter1) (qt-to-abs quarter2))))

(pcase-defmacro regexp (capture regexp)
  `(and x (let ,capture (s-match ,regexp x))))

;; this function can also be used to check the format of an allocation during
;; assignment
(defun org-x-dag-allocation-fraction (quarter allocation)
  (cl-flet
      ((hhmm-to-mins
        (H M)
        (let ((H* (string-to-number H))
              (M* (string-to-number M)))
          (+ (* 60.0 H*) M*))))
    (let* ((qt-days (-> (org-x-dag-shift-quarter quarter 1 'quarter)
                        (org-x-dag-quarter-diff quarter)
                        (float)))
           (qt-mins (* qt-days 1440))
           (hhmm-regexp "\\(2[0-4]\\|[0-1][0-9]\\|[0-9]\\):\\([0-6][0-9]\\)"))
      (pcase allocation
        ;; match 'X%' where X is a flat percent of the quarter
        ((regexp `(,_ ,percent) "^\\([0-9]+\\)%$")
         (/ (string-to-number percent) 100.0))
        ;; match 'H:M' where H is hours and M is minutes (both clock digits)
        ((regexp `(,_ ,H ,M) (format "^%s$" hhmm-regexp))
         (/ (hhmm-to-mins H M) 1440.0))
        ;; match 'H:M/Dd' where H/M are like above and D is number of days
        ;; per quarter
        ((regexp `(,_ ,H ,M ,d) (format "^%s/\\([0-9]+\\)d$" hhmm-regexp))
         (let ((d* (string-to-number d))
               (mins (hhmm-to-mins H M)))
           (/ (* mins d*) qt-mins)))
        (e (error "Invalid allocation: %s" e))))))

(provide 'org-x-dag)
;;; org-x-dag.el ends here
