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

;;; DATE/TIME FUNCTIONS

;; current state

(defun org-x-dag-current-datetime ()
  (->> (current-time)
       (decode-time)
       (-drop 1)
       (-take 5)
       (reverse)))

(defun org-x-dag-current-date ()
  (-take 3 (org-x-dag-current-datetime)))

(defun org-x-dag-current-time ()
  (-drop 3 (org-x-dag-current-datetime)))

(defun org-x-dag-date-at-current-time (date)
  `(,@date ,@(org-x-dag-current-time)))

;; calendar interface

(defun org-x-dag-gregorian-to-date (greg)
  (-let (((m d y) greg))
    `(,y ,m ,d)))

(defun org-x-dag-date-to-gregorian (date)
  (-let (((y m d) date))
    `(,m ,d ,y)))

(defun org-x-dag-date-to-absolute (date)
  (->> (org-x-dag-date-to-gregorian date)
       (calendar-absolute-from-gregorian)))

(defun org-x-dag-absolute-to-date (abs)
  (->> (calendar-gregorian-from-absolute abs)
       (org-x-dag-gregorian-to-date)))

;; datetime operations

(defun org-x-dag-datetime< (datetime0 datetime1)
  (org-x-dag-with-times datetime0 datetime1
    (-when-let (next (->> (-zip-with #'cons datetime0 datetime1)
                          (--drop-while (= (car it) (cdr it)))
                          (car)))
      (< (car next) (cdr next)))))

(defun org-x-dag-datetime= (datetime0 datetime1)
  (org-x-dag-with-times datetime0 datetime1
    (->> (-zip-with #'cons datetime0 datetime1)
         (--drop-while (= (car it) (cdr it)))
         (not))))

(defun org-x-dag-datetime-split (datetime)
  ;; TODO this function doesn't guarantee that a short timestamp is properly
  ;; formatted
  (if (org-ml-time-is-long datetime)
      (-split-at 3 datetime)
    `(,(-take 3 datetime) nil)))

(defun org-x-dag-datetime-shift (datetime shift unit)
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

(defun org-x-dag-date-diff (date0 date1)
  ""
  (pcase (list date0 date1)
    (`((,y0 ,m0 ,d0) (,y1 ,m1 ,d1))
     (- (calendar-absolute-from-gregorian `(,m0 ,d0 ,y0))
        (calendar-absolute-from-gregorian `(,m1 ,d1 ,y1))))
    (_ (error "Invalid date format(s): %S or %S" date0 date1))))

;; date <-> week

(defun org-x-dag-date-to-week-number (date)
  (-let* (((y m d) date)
          (greg (org-x-dag-date-to-gregorian date))
          (abs (calendar-absolute-from-gregorian greg))
          (daynum (calendar-day-of-week greg))
          ;; Catch the special case where the first few days of January might
          ;; belong to the previous year
          (start-year (if (and (= 1 m) (< d (1+ daynum))) (1- y) y))
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
         (org-x-dag-absolute-to-date))))

(defun org-x-dag-date-to-week-start (date)
  ""
  (let* ((greg (org-x-dag-date-to-gregorian date))
         (daynum (calendar-day-of-week greg)))
    (-> (calendar-absolute-from-gregorian greg)
        (- daynum)
        (org-x-dag-absolute-to-date))))

;; date <-> quarter

(defun org-x-dag-quarter-to-date (quarter)
  (-let (((y q) quarter))
    (list y (1+ (* q 3)) 1)))

(defun org-x-dag-date-to-quarter (date)
  (-let (((y m _) date))
    (list y (1+ (/ m 3)))))

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
             (org-x-dag-date-to-absolute))))
    (- (qt-to-abs quarter1) (qt-to-abs quarter2))))

;;; GLOBAL STATE

;; variables to store state

(defun org-x-dag-create (d m fis il if s c fs)
  (list :dag d
        :id->meta m
        :id->status s
        :file->ids fis
        :illegal-local il
        :illegal-foreign if
        :current-date c
        :files fs))

(defun org-x-dag-read-file-paths ()
  (list :goal-files (list :lifetime (org-x-get-lifetime-goal-file)
                          :endpoint (org-x-get-endpoint-goal-file)
                          :survival (org-x-get-survival-goal-file))
        :plan-files (list :daily (org-x-get-daily-plan-file)
                          :weekly (org-x-get-weekly-plan-file)
                          :quarterly (org-x-qtp-get-file))
        :action-files (append (org-x-get-action-files)
                              (org-x-get-incubator-files))))
        

;; (defun org-x-dag-flatten-goal-file-state (state)
;;   (-let (((&plist :lifetime l :endpoint e :survival s) state))
;;     `(,l ,e ,s)))

;; (defun org-x-dag-flatten-planning-file-state (state)
;;   (-let (((&plist :quarterly q :weekly w :daily d) state))
;;     `(,q ,w ,d)))

(defun org-x-dag-flatten-file-state (state)
  (cl-flet
      ((flat-flip
        (plist)
        (->> (-partition-all 2 plist)
             (--map (cons (cadr it) (car it))))))
  (-let (((&plist :goal-files g :plan-files p :action-files a) state))
    (append (flat-flip g) (flat-flip p) (--map (cons it :action) a)))))

(defun org-x-dag-empty ()
  (org-x-dag-create (dag-empty)
                    (ht-create #'equal)
                    (ht-create #'equal)
                    (ht-create #'equal)
                    (ht-create #'equal)
                    (ht-create #'equal)
                    (org-x-dag-current-date)
                    nil))

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

(defun org-x-dag-build-meta (file point level todo title tags planning props parent)
  (list :file file
        :point point
        :level level
        :todo todo
        :title title
        :tags tags
        :planning planning
        :props props
        :buffer-parent parent))

;; state lookup functions
;;
;; all functions with `org-x-dag->' or `org-x-dag-id->' depend on the value of
;; `org-x-dag'

;; global state slot lookup

(defun org-x-dag->metatable ()
  (plist-get org-x-dag :id->meta))

(defun org-x-dag->dag ()
  (plist-get org-x-dag :dag))

(defun org-x-dag->adjacency-list ()
  (dag-get-adjacency-list (org-x-dag->dag)))

;; state files

(defun org-x-dag->file-state ()
  (plist-get org-x-dag :files))

(defun org-x-dag->goal-file-state ()
  (plist-get (org-x-dag->file-state) :goal-files))

(defun org-x-dag->planning-file-state ()
  (plist-get (org-x-dag->file-state) :plan-files))

(defun org-x-dag->goal-file (which)
  (plist-get (org-x-dag->goal-file-state) which))

(defun org-x-dag->planning-file (which)
  (plist-get (org-x-dag->planning-file-state) which))

(defun org-x-dag->action-files ()
  (plist-get (org-x-dag->file-state) :action-files))

(defun org-x-dag->files ()
  (org-x-dag-flatten-file-state (org-x-dag->file-state)))

;; id properties

(defun org-x-dag-id->metaprop (id prop)
  (-> (org-x-dag->metatable)
      (ht-get id)
      (plist-get prop)))

(defun org-x-dag-id->file (id)
  "Return file for ID."
  (org-x-dag-id->metaprop id :file))

(defun org-x-dag-id->file-group (id)
  "Return file group for ID.
Return one of seven values: :lifetime, :survival, :endpoint,
:quarterly, :weekly, :daily, or nil (which means action files)."
  (let* ((f (org-x-dag-id->file id))
         (g (or (--find (equal f (org-x-dag->goal-file it))
                        '(:lifetime :survival :endpoint))
                (--find (equal f (org-x-dag->planning-file it))
                        '(:quarterly :weekly :daily)))))
    (list f g)))

(defun org-x-dag-id->point (id)
  "Return point for ID."
  (org-x-dag-id->metaprop id :point))

(defun org-x-dag-id->level (id)
  "Return level for ID."
  (org-x-dag-id->metaprop id :level))

(defun org-x-dag-id->todo (id)
  "Return todo keyword for ID."
  (org-x-dag-id->metaprop id :todo))

(defun org-x-dag-id->title (id)
  "Return title for ID."
  (org-x-dag-id->metaprop id :title))

(defun org-x-dag-id->local-tags (id)
  "Return local tags for ID."
  (org-x-dag-id->metaprop id :tags))

(defun org-x-dag-id->tags (parent-tags id)
  "Return all tags for ID.

If PARENT-TAGS is nil, return all inherited tags based on the
parents of ID. If PARENT-TAGS is a list of strings, these are
used as the parent tags instead of looking them up.

Returned tags will be ordered from left to right as lowest to
highest in the tree."
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
    (let ((local-tags (org-x-dag-id->local-tags id)))
      `(,@local-tags ,@(or parent-tags (ascend id nil))))))

(defun org-x-dag-id->bucket (parent-tags id)
  (-some->> (org-x-dag-id->tags parent-tags id)
    (--find (= (elt it 0) org-x-tag-category-prefix))
    (s-chop-prefix "_")
    (intern)))

(defun org-x-dag-id->link (id)
  "Return the link node for ID."
  (let ((desc (org-x-dag-id->title id)))
    (->> (org-ml-build-secondary-string! desc)
         (apply #'org-ml-build-link id :type "id"))))

(defun org-x-dag-id->link-item (id)
  "Return the link node of ID wrapped in an item node."
  (->> (org-x-dag-id->link id)
       (org-ml-build-paragraph)
       (org-ml-build-item)))

;; id relationships

(defun org-x-dag-id->parents (id)
  "Return parent nodes of ID."
  (->> (plist-get org-x-dag :dag)
       (dag-get-parents id)))

(defun org-x-dag-id->children (id)
  "Return child nodes of ID."
  (->> (plist-get org-x-dag :dag)
       (dag-get-children id)))

(defun org-x-dag-id->buffer-parent (id)
  "Return the buffer parent id (if any) of ID."
  (org-x-dag-id->metaprop id :buffer-parent))

(defun org-x-dag-id->split-parents-2 (id)
  "Return the buffer and non-buffer parents of ID.

Return value is a list like (BUFFER NON-BUFFER)."
  (let ((parents (org-x-dag-id->parents id)))
    (-if-let (buffer-parent (org-x-dag-id->buffer-parent id))
        (cons buffer-parent (-remove-item buffer-parent parents))
      (cons nil parents))))

(defun org-x-dag-split-3 (fun id)
  (-let* (((buffer linked) (funcall fun id))
          (f (org-x-dag-id->file id))
          ((local foreign) (--separate (equal f (org-x-dag-id->file it)) linked)))
    (list buffer local foreign)))

(defun org-x-dag-id->split-parents-3 (id)
  "Return the buffer, local, and foreign parents of ID.

Return value is a list like (BUFFER LOCAL FOREIGN)."
  (org-x-dag-split-3 #'org-x-dag-id->split-parents-2 id))

(defun org-x-dag-id->linked-parents (id)
  "Return non-buffer (foreign) parents of ID."
  (cdr (org-x-dag-id->split-parents-2 id)))

(defun org-x-dag-id->split-children-2 (id)
  "Return buffer and non-buffer children of ID.

Return value is a list like (BUFFER NON-BUFFER)."
  (->> (org-x-dag-id->children id)
       (--separate (equal (org-x-dag-id->buffer-parent it) id))))

(defun org-x-dag-id->split-children-3 (id)
  "Return buffer, local, and foreign children of ID.

Return value is a list like (BUFFER LOCAL FOREIGN)."
  (org-x-dag-split-3 #'org-x-dag-id->split-children-2 id))

(defun org-x-dag-id->buffer-children (id)
  "Return children of ID that are not linked."
  (car (org-x-dag-id->split-children-2 id)))

(defun org-x-dag-id->linked-children (id)
  "Return children of ID that are linked."
  (cadr (org-x-dag-id->split-children-2 id)))

(defmacro org-x-dag-id->with-split-parents (id &rest body)
  (declare (indent 1))
  `(let ((it-buffer it-foreign) (org-x-dag-id->split-parents-2 ,id))
     ,@body))

(defmacro org-x-dag-id->with-split-children (id &rest body)
  (declare (indent 1))
  `(let ((it-buffer it-foreign) (org-x-dag-id->split-children-2 ,id))
     ,@body))

(defun org-x-dag-id->group-parent-links-by-file-p (id)
  "Return parent links for ID grouped by file."
  (org-x-dag-id->with-split-parents id
    (-group-by #'org-x-dag-id->file it-foreign)))

(defun org-x-dag-id->group-child-links-by-file-p (id)
  "Return child links for ID grouped by file."
  (org-x-dag-id->with-split-children id
    (-group-by #'org-x-dag-id->file it-foreign)))

(defun org-x-dag-id->all-buffer-children (id)
  "Return nested children of ID that are in the same buffer."
  (->> (org-x-dag-id->buffer-children id)
       (-mapcat #'org-x-dag-id->all-buffer-children)
       (cons id)))

(defun org-x-dag-id->epg-status (id)
  (-let* (((cbuffer clocal cforeign) (org-x-dag-id->split-children-3 id))
          ((pbuffer plocal pforeign) (org-x-dag-id->split-parents-3 id))
          ;; (deadline (org-x-dag-id->planning-timestamp :deadline id))
          (leafp (not local)))
    (list :leafp leafp
          :toplevelp (org-x-dag-id->is-toplevel-p id)
          ;; :deadline ;; past, current, out of range (if after parent deadline)
          :committed ;; t if linked to the LTG
          :planned ;; t if on a plan
          :fulfilled ;; t if any child tasks
          )))

(defun org-x-dag-id->goal-status (which id)
  (let* ((ps (org-x-dag-id->linked-parents id))
         (ks (->> (-map #'org-x-dag-id->file ps)
                  (--map (cond
                          ((equal it (org-x-dag->goal-file :survival))
                           :survival)
                          ((member it `(,(org-x-dag->goal-file :endpoint)
                                        ,(org-x-dag->goal-file :lifetime)))
                           :non-survival)
                          (t
                           :other)))))
         (status (cond
                  ((null ks)
                   :no-goals)
                  ((memq :other ks)
                   :invalid-goals)
                  ((and (memq :non-survival ks) (memq :survival ks))
                   :mixed-goals)
                  ((memq :survival ks)
                   :survival)
                  (t
                   (let ((gs (org-x-dag->qtp-goal-ids which)))
                     (if (--any-p (member it gs) ps) :planned :committed))))))
    (list ps status)))

;; id predicates/identities

(defun org-x-dag-id->is-done-p (id)
  "Return t if ID has done keywords."
  (member (org-x-dag-id->todo id) org-x-done-keywords))

(defun org-x-dag-id->is-closed-p (id)
  "Return t if ID is closed.
This means the ID has a closed timestamp in the past."
  (-when-let (c (org-x-dag-id->planning-epoch :closed id))
    (<= c (float-time))))

(defun org-x-dag-id->is-archivable-p (id)
  "Return t if ID is archivable.
This means the ID has be closed for longer than
`org-x-archive-delay'."
  (-when-let (c (org-x-dag-id->planning-epoch :closed id))
    (org-x-dag-time-is-archivable-p c)))

(defun org-x-dag-id->id-survival-p (id)
  "Return t if ID has a parent survival goal."
  (let ((f (org-x-dag->goal-file :survival)))
    (->> (org-x-dag-id->linked-parents id)
         (--any-p (equal (org-x-dag-id->file it) f)))))

(defun org-x-dag-id->is-incubated (which id)
  "Return t if ID is incubated.

This is defined as not having a linked parent that is a goal which
is also referenced in WHICH quarterly plan."
  (let ((q (org-x-dag->qtp-goal-ids which)))
    (--none-p (member it q) (org-x-dag-id->linked-parents id))))

(defun org-x-dag-id->is-uncommitted (id)
  "Return t if ID is uncommitted (not assigned a goal).

This is equivalent to the GTD adjective \"maybe\". An ID can only
be uncommitted if it is also incubated."
  (let ((fs `(,(org-x-dag->goal-file :lifetime)
              ,(org-x-dag->goal-file :endpoint))))
    (->> (org-x-dag-id->linked-parents id)
         (--none-p (member (org-x-dag-id->file it) fs)))))

;; (defun org-x-dag-id->is-floating-p (id)
;;   "Return t if ID is floating."
;;   (-> (plist-get org-x-dag :dag)
;;       (dag-get-floating-nodes)
;;       (ht-get id)))

(defun org-x-dag-id->is-toplevel-p (id)
  "Return t if ID is at the top of its buffer."
  (not (org-x-dag-id->buffer-parent id)))

(defun org-x-dag-id->is-buffer-leaf-p (id)
  "Return t if ID has no buffer children."
  (not (org-x-dag-id->buffer-children id)))

(defun org-x-dag-id->is-childless-p (id)
  "Return t if ID has no buffer children."
  (not (org-x-dag-id->children id)))

(defun org-x-dag-id->is-parentless-p (id)
  "Return t if ID has no buffer children."
  (not (org-x-dag-id->parents id)))

(defun org-x-dag-id->is-goal-p (which id)
  "Return t if ID is a goal defined by WHICH."
  (let ((f (org-x-dag->goal-file which)))
    (equal f (org-x-dag-id->file id))))

(defun org-x-dag-id->is-plan-p (which id)
  "Return t if ID is a plan defined by WHICH."
  (let ((f (org-x-dag->planning-file which)))
    (equal f (org-x-dag-id->file id))))

(defun org-x-dag-id->parent-link-in-file-p (file id)
  "Return t if ID has a parent link in FILE."
  (org-x-dag-id->with-split-parents id
    (--any-p (equal file (org-x-dag-id->file it)) it-foreign)))

(defun org-x-dag-id->child-link-in-file-p (file id)
  "Return t if ID has a child link in FILE."
  (org-x-dag-id->with-split-children id
    (--any-p (equal file (org-x-dag-id->file it)) it-foreign)))

(defun org-x-dag-id->parent-link-in-files-p (files id)
  "Return t if ID has a parent link in any of FILES."
  (org-x-dag-id->with-split-parents id
    (--any-p (member (org-x-dag-id->file it) files) it-foreign)))

(defun org-x-dag-id->child-link-in-files-p (files id)
  "Return t if ID has a child link in any of FILES."
  (org-x-dag-id->with-split-children id
    (--any-p (member (org-x-dag-id->file it) files) it-foreign)))

;; files to ids

(defun org-x-dag-files->ids (files)
  (let ((filemap (plist-get org-x-dag :file->ids)))
    (--mapcat (ht-get filemap it) files)))

(defun org-x-dag-file->ids (file)
  (org-x-dag-files->ids `(,file)))

(defun org-x-dag->epg-ids ()
  (org-x-dag-file->ids (org-x-get-endpoint-goal-file)))

(defun org-x-dag->ltg-ids ()
  (org-x-dag-file->ids (org-x-get-lifetime-goal-file)))

(defun org-x-dag->svg-ids ()
  (org-x-dag-file->ids (org-x-get-survival-goal-file)))

(defun org-x-dag->current-date ()
  (plist-get org-x-dag :current-date))

;; (defun org-x-dag->qtp-ids ()
;;   (org-x-dag-file->ids (org-x-qtp-get-file)))

;; (defun org-x-dag->wkp-ids ()
;;   (org-x-dag-file->ids (org-x-get-weekly-plan-file)))

(defun org-x-dag-filter-ids-tags (tags ids)
  (--filter (-intersection (org-x-dag-id->tags nil it) tags) ids))

(defun org-x-dag-quarter-tags-to-date (tags)
  (-let (((y q) (reverse tags)))
    (org-x-dag-quarter-to-date (list (org-x-dag-tag-to-year y)
                                     (org-x-dag-tag-to-quarter q)))))

(defun org-x-dag-weekly-tags-to-date (tags)
  (-let (((y w) (reverse tags)))
    (org-x-dag-week-number-to-date (list (org-x-dag-tag-to-year y)
                                         (org-x-dag-tag-to-week w)))))

(defun org-x-dag-daily-tags-to-date (tags)
  (-let (((y m d) (reverse tags)))
    (org-x-dag-week-number-to-date (list (org-x-dag-tag-to-year y)
                                         (org-x-dag-tag-to-month m)
                                         (org-x-dag-tag-to-day d)))))

(defun org-x-dag-date-to-quarter-tags (date)
  (-let (((y q) (org-x-dag-date-to-quarter date)))
    (list (org-x-dag-format-year-tag y)
          (org-x-dag-format-quarter-tag q))))

(defun org-x-dag-date-to-week-tags (date)
  (-let (((y _ _) date)
         (w (org-x-dag-date-to-week-number date)))
    (list (org-x-dag-format-year-tag y)
          (org-x-dag-format-week-tag w))))

(defun org-x-dag-date-to-daily-tags (date)
  (-let (((y m d) date))
    (list (org-x-dag-format-year-tag y)
          (org-x-dag-format-month-tag m)
          (org-x-dag-format-day-tag d))))

;; (defun org-x-dag-date->tagged-ids (id-getter tag-getter date)
;;   (let ((target-tags (funcall tag-getter date)))
;;     (org-x-dag-filter-ids-tags target-tags (funcall id-getter))))

(defun org-x-dag-date->tagged-ids (ids tag-getter date)
  (let ((target-tags (funcall tag-getter date)))
    (org-x-dag-filter-ids-tags target-tags ids)))

;; (defun org-x-dag-date->qtp-ids (date)
;;   (org-x-dag-date->tagged-ids #'org-x-dag->qtp-ids
;;                               #'org-x-dag-date-to-quarter-tags
;;                               date))

;; (defun org-x-dag-date->wkp-ids (date)
;;   (org-x-dag-date->tagged-ids #'org-x-dag->wkp-ids
;;                               #'org-x-dag-date-to-week-tags
;;                               date))

;; (defun org-x-dag->qtp-current-ids ()
;;   (org-x-dag-date->qtp-ids (org-x-dag->current-date)))

;; (defun org-x-dag->wkp-current-ids (date)
;;   (org-x-dag-date->wkp-ids (org-x-dag->current-date)))


;; (defun org-x-dag->dlp-ids ()
;;   (org-x-dag-file->ids (org-x-get-daily-plan-file)))

;; (defun org-x-dag-date->dlp-ids (date)
;;   (org-x-dag-date->tagged-ids #'org-x-dag->dlp-ids
;;                               #'org-x-dag-date-to-daily-tags
;;                               date))

;; (defun org-x-dag->dlp-current-ids (date)
;;   (org-x-dag-date->dlp-ids (org-x-dag->current-date)))

(defun org-x-dag-which->ids (file-key date-to-tag which)
  (cl-flet
      ((date-ids
        (ids date)
        (org-x-dag-date->tagged-ids ids date-to-tag date)))
    (let ((ids (org-x-dag-file->ids (org-x-dag->planning-file file-key))))
      (pcase which
        (`all ids)
        (`current (date-ids ids (org-x-dag->current-date)))
        (date (date-ids ids date))))))

(defun org-x-dag->qtp-ids (which)
  (org-x-dag-which->ids :quarterly #'org-x-dag-date-to-quarter-tags which))

(defun org-x-dag->wkp-ids (which)
  (org-x-dag-which->ids :weekly #'org-x-dag-date-to-week-tags which))

(defun org-x-dag->dlp-ids (which)
  (org-x-dag-which->ids :daily #'org-x-dag-date-to-daily-tags which))

(defun org-x-dag-partition-child-ids (files ids)
  (->> (org-x-dag-files->ids files)
       (--split-with (-intersection ids (org-x-dag-id->children it)))))

(defun org-x-dag-id->has-child-in-files-p (id files)
  (-intersection (org-x-dag-id->children id) (org-x-dag-files->ids files)))

(defun org-x-dag-id->has-parent-in-files-p (id files)
  (-intersection (org-x-dag-id->parents id) (org-x-dag-files->ids files)))

(defun org-x-dag->dlp-action-ids (which)
  (->> (org-x-dag->dlp-ids which)
       (org-x-dag-partition-child-ids (org-x-dag->action-files))))

(defun org-x-dag->wkp-qtp-ids (which)
  (->> (org-x-dag->wkp-ids which)
       (org-x-dag-partition-child-ids (list (org-x-qtp-get-file)))))

(defun org-x-dag->qtp-goal-ids (which)
  "Return all goal IDs associated with WHICH quarter."
  (let ((fs `(,(org-x-dag->goal-file :endpoint)
              ,(org-x-dag->goal-file :lifetime))))
    (->> (org-x-dag->qtp-ids which)
         (-mapcat #'org-x-dag-id->linked-parents)
         (--filter (member (org-x-dag-id->file it) fs))
         (-uniq))))

;; (defun org-x-dag-date->dlp-parent-ids (date)
;;   (let ((dlp-ids (org-x-dag-date->dlp-ids date)))
;;     (->> (org-x-get-action-and-incubator-files)
;;          (org-x-dag-files->ids)
;;          (--filter (-intersection (org-x-dag-id->children it) dlp-ids)))))

(defun org-x-dag->leaf-epg-ids ()
  (-remove #'org-x-dag-id->buffer-children (org-x-dag->epg-ids)))

(defun org-x-dag->leaf-ltg-ids ()
  (let ((epg-file (org-x-get-endpoint-goal-file)))
    (->> (org-x-dag->ltg-ids)
         (-remove #'org-x-dag-id->buffer-children)
         (--remove (equal (org-x-dag-id->file it) epg-file)))))

(defun org-x-dag-goal-count-tasks (id)
  (->> (org-x-dag-id->children id)
       (-mapcat #'org-x-dag-id->all-buffer-children)
       ;; TODO this isn't very efficient, looking up children twice
       (-remove #'org-x-dag-id->buffer-children)
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

;; (defvar org-x-dag-week-start-index 0
;;   "The day considered to start a week (0 = Sunday).")

(defvar org-x-dag-selected-date nil
  "The current week to be used for planning.
A date like (YEAR MONTH DAY).")

;;; PLANNING

;; planning buffer tags
;;
;; use tags to encode date/time information in the buffer since it is really
;; easy to look up tags in the DAG

(defconst org-x-dag-weekly-tags
  '((0 . "SUN")
    (1 . "MON")
    (2 . "TUE")
    (3 . "WED")
    (4 . "THU")
    (5 . "FRI")
    (6 . "SAT")))

(defun org-x-dag--parse-date-tag (prefix tag)
  (let ((re (format "%s\\([0-9]+\\)" prefix)))
    (-some->> (s-match re tag)
      (nth 1)
      (string-to-number))))

(defun org-x-dag-tag-to-year (tag)
  (-some->> (org-x-dag--parse-date-tag "Y" tag)
    (+ 2000)))

(defun org-x-dag-tag-to-quarter (tag)
  (org-x-dag--parse-date-tag "Q" tag))

(defun org-x-dag-tag-to-week (tag)
  (org-x-dag--parse-date-tag "W" tag))

(defun org-x-dag-tag-to-day-of-week (tag)
  (car (rassoc tag org-x-dag-weekly-tags)))

(defun org-x-dag-tag-to-month (tag)
  (org-x-dag--parse-date-tag "M" tag))

(defun org-x-dag-tag-to-day (tag)
  (org-x-dag--parse-date-tag "D" tag))

(defun org-x-dag-format-year-tag (year)
  (format "Y%02d" (mod year 2000)))

(defun org-x-dag-format-quarter-tag (quarter)
  (format "Q%d" quarter))

(defun org-x-dag-format-month-tag (month)
  (format "M%02d" month))

(defun org-x-dag-format-week-tag (week)
  (format "W%02d" week))

(defun org-x-dag-format-day-of-week-tag (daynum)
  (alist-get daynum org-x-dag-weekly-tags))

(defun org-x-dag-format-day-tag (day)
  (format "D%02d" day))

;; headline lookup

(defun org-x-dag-headlines-find-tag (tag headlines)
  (--find (org-ml-headline-has-tag tag it) headlines))

(defun org-x-dag-headlines-find-year (year headlines)
  (-> (org-x-dag-format-year-tag year)
      (org-x-dag-headlines-find-tag headlines)))

(defun org-x-dag-headlines-find-quarter (quarter headlines)
  (-> (org-x-dag-format-quarter-tag quarter)
      (org-x-dag-headlines-find-tag headlines)))

(defun org-x-dag-headlines-find-week (weeknum headlines)
  (-> (org-x-dag-format-week-tag weeknum)
      (org-x-dag-headlines-find-tag headlines)))

(defun org-x-dag-headlines-find-day-of-week (daynum headlines)
  (-> (org-x-dag-format-day-of-week-tag daynum)
      (org-x-dag-headlines-find-tag headlines)))

(defun org-x-dag-headlines-find-month (month headlines)
  (-> (org-x-dag-format-month-tag month)
      (org-x-dag-headlines-find-tag headlines)))

(defun org-x-dag-headlines-find-day (day headlines)
  (-> (org-x-dag-format-day-tag day)
      (org-x-dag-headlines-find-tag headlines)))

;; headline builders

(defun org-x-dag-build-planning-headline (title tag level subheadlines)
  (apply #'org-ml-build-headline!
         :title-text title
         :tag (list tag)
         :level level
         subheadlines))

(defun org-x-dag-build-year-headline (year subheadlines)
  (let ((title (number-to-string year))
        (tag (org-x-dag-format-year-tag year)))
    (org-x-dag-build-planning-headline tag title 1 subheadlines)))

(defun org-x-dag-build-quarter-headline (quarter subheadlines)
  (let ((title (format "Quarter %d" quarter))
        (tag (org-x-dag-format-quarter-tag quarter)))
    (org-x-dag-build-planning-headline title tag 2 subheadlines)))

(defun org-x-dag-build-week-headline (year weeknum subheadlines)
  (-let* (((_ m d) (org-x-dag-week-number-to-date year weeknum))
          (m* (calendar-month-name m))
          (title (format "%s %s" m* d))
          (tag (org-x-dag-format-week-tag weeknum)))
    (org-x-dag-build-planning-headline title tag 2 subheadlines)))

(defun org-x-dag-build-month-headline (month subheadlines)
  (let ((title (calendar-month-name month))
        (tag (org-x-dag-format-month-tag month)))
    (org-x-dag-build-planning-headline title tag 2 subheadlines)))

(defun org-x-dag-build-day-headline (date subheadlines)
  (let ((title (format "%d-%02d-%02d" y m d))
        (tag (org-x-dag-format-day-tag d)))
    (org-x-dag-build-planning-headline title tag 3 subheadlines)))

(defun org-x-dag-build-day-of-week-headline (daynum subheadlines)
  (let ((title (elt calendar-day-name-array daynum))
        (tag (alist-get daynum org-x-dag-weekly-tags)))
    (org-x-dag-build-planning-headline title tag 3 subheadlines)))

;; id headline builders

(defun org-x-dag-build-planning-id-headline (title level paragraph ids)
  (let ((sec (-some-> paragraph
               (org-ml-build-paragraph!)
               (list))))
    (->> (org-ml-build-headline! :title-text title
                                 :level level
                                 :todo-keyword org-x-kw-todo
                                 :section-children sec)
         (org-x-dag-headline-add-id)
         (org-x-dag-headline-set-parent-links ids))))

(defun org-x-dag-build-qtp-headline (title paragraph ids allocation)
  (->> (org-x-dag-build-planning-id-headline title 3 paragraph ids)
       (org-ml-headline-set-node-property org-x-prop-allocate allocation)))

(defun org-x-dag-build-wkp-headline (title paragraph ids)
  (org-x-dag-build-planning-id-headline title 4 paragraph ids))

(defun org-x-dag-build-dlp-headline (title paragraph ids datetime)
  (let ((pl (org-ml-build-planning! :scheduled datetime)))
    (->> (org-x-dag-build-planning-id-headline title 4 paragraph ids)
         (org-ml-headline-set-planning pl))))

;; buffer manipulation

(defun org-x-dag-qtp-to-children (qt-plan)
  (-let* (((&plist :categories :goals) qt-plan)
          ;; TODO what happens if there are no categories?
          (sec (-some->> categories
                 (--map-indexed (org-ml-build-item!
                                 :bullet it-index
                                 :paragraph (symbol-name it)))
                 (apply #'org-ml-build-plain-list)
                 (org-ml-build-drawer org-x-drwr-categories)
                 (org-ml-build-section)))
          (subtrees (--map (apply #'org-ml-build-headline!
                                  :level 3
                                  :title-text (plist-get (cdr it) :desc)
                                  :tags `(,(plist-get (cdr it) :tag))
                                  (alist-get (car it) goals))
                           org-x-life-categories)))
    (if sec (cons sec goals) subtrees)))

(defun org-x-dag-qtp-from-children (children)
  ;; ignore properties, planning, etc
  (-let* (((sec subtrees) (if (org-ml-is-type 'section (car children))
                              `(,(car children) ,(cdr children))
                            `(nil ,children)))
          (cats (-some->> sec
                  (--find (org-x--is-drawer-with-name org-x-drwr-categories it))
                  (org-x-qtp-drawer-to-categories)))
          (goals (--map (let* ((tag (car (org-ml-get-property :tags it)))
                               (key (car (--find (equal tag (plist-get (cdr it) :tag))
                                                 org-x-life-categories))))
                          (cons key (org-ml-headline-get-subheadlines it)))
                        subtrees)))
    (list :categories cats :goals goals)))

(defun org-x-dag-qtp-get (quarter)
  (org-x-with-file (org-x-qtp-get-file)
    (-let (((year qnum) quarter))
      (->> (org-ml-parse-subtrees 'all)
           (org-x-dag-headlines-find-year year)
           (org-ml-headline-get-subheadlines)
           (org-x-dag-headlines-find-quarter qnum)
           (org-ml-get-children)
           (org-x-dag-qtp-from-children)))))

(defun org-x-dag-qtp-set (quarter qt-plan)
  (cl-flet
      ((build-yr-headline
        (year qnum children)
        (->> (org-x-dag-build-quarter-headline qnum children)
             (list)
             (org-x-dag-build-year-headline year))))
    (org-x-with-file (org-x-qtp-get-file)
      (-let* (((year qnum) quarter)
              (sts (org-ml-parse-subtrees 'all))
              (children (org-x-dag-qtp-to-children qt-plan)))
        (-if-let (st-yr (org-x-dag-headlines-find-year year sts))
            (-if-let (st-qt (->> (org-ml-headline-get-subheadlines st-yr)
                                 (org-x-dag-headlines-find-quarter qnum)))
                (org-ml-update* (org-ml-set-children children it) st-qt)
              (org-ml-update*
                (-snoc it (org-x-dag-build-quarter-headline qnum children))
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

(defun org-x-qtp-add-goal-ids (quarter ids title allocation)
  (->> (org-x-dag-build-qtp-headline title nil ids allocation)
       (org-x-qtp-add-goal quarter)))

(defun org-x-dag-weekly-headlines-to-alist (headlines)
  (->> (-map #'car org-x-dag-weekly-tags)
       (--map (->> (org-x-dag-headlines-find-day-of-week it headlines)
                   (org-ml-headline-get-subheadlines)
                   (cons it)))))

(defun org-x-dag-weekly-alist-to-headlines (plan)
  (--map (-let (((daynum . hls) it))
           (org-x-dag-build-day-of-week-headline daynum hls))
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
      ((build-yr-headline
        (year weeknum children)
        (->> (org-x-dag-build-week-headline year weeknum children)
             (list)
             (org-x-dag-build-year-headline year))))
    (org-x-with-file (org-x-get-weekly-plan-file)
      (-let* (((year weeknum) week)
              (sts (org-ml-parse-subtrees 'all))
              (children (org-x-dag-weekly-alist-to-headlines plan)))
        (-if-let (st-yr (org-x-dag-headlines-find-year year sts))
            (-if-let (st-wk (->> (org-ml-headline-get-subheadlines st-yr)
                                 (org-x-dag-headlines-find-week weeknum)))
                (org-ml-update* (org-ml-set-children children it) st-wk)
              (org-ml-update*
                (-snoc it (org-x-dag-build-week-headline year weeknum children))
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
  (->> (org-x-dag-build-wkp-headline title desc ids)
       (org-x-dag-wkp-day-add week daynum)))

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
      ((build-mo-headline
        (date headlines)
        (-let (((_ m _) date))
          (->> (org-x-dag-build-day-headline date headlines)
               (list)
               (org-x-dag-build-month-headline m))))
       (build-yr-headline
        (date headlines)
        (-let* (((y _ _) date))
          (->> (build-mo-headline date headlines)
               (list)
               (org-x-dag-build-year-headline y)))))
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
                    (-snoc it (org-x-dag-build-day-headline date headlines))
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
    (->> (org-x-dag-build-dlp-headline title nil ids datetime)
         (org-x-dag-dlp-add date))))

;;; BUFFER SCANNING

(defun org-x-dag-get-local-property (beg end prop-re)
  (save-excursion
    (goto-char beg)
    (when (re-search-forward prop-re end t)
      (match-string-no-properties 3))))

(defun org-x-dag-get-local-properties (beg end prop-pairs)
  (save-excursion
    (let (acc cur)
      (while prop-pairs
        (goto-char beg)
        (setq cur (car prop-pairs))
        (when (re-search-forward (cdr cur) end t)
          (!cons (cons (car cur) (match-string-no-properties 3)) acc))
        (!cdr prop-pairs))
      acc)))

(defconst org-x-dag-parent-link-drawer-re 
  (concat
   "^[ \t]*:X_PARENT_LINKS:[ \t]*\n"
   "\\(\\(?:^- .*?\n\\)+\\)"
   "[ \t]*:END:[ \t]*$"))

(defun org-x-dag-next-headline ()
  (save-excursion (outline-next-heading)))

(defun org-x-dag-get-parent-links (start end)
  (save-excursion
    (when start
      (goto-char start))
    (when (re-search-forward org-x-dag-parent-link-drawer-re end t)
      (let ((ss (split-string (match-string-no-properties 1) "\n" t))
            acc)
        (while ss
          (when (string-match "id:\\([^][]\\{36\\}\\)" (car ss))
            (!cons (match-string-no-properties 1 (car ss)) acc))
          (!cdr ss))
        acc))))

(defun org-x-dag-line-regexp (kws)
  (let ((level-re "\\(\\*+\\)")
        (kw-re (format "\\(%s\\)?" (s-join "\\|" kws)))
        (title-re "\\(?:[ ]*\\([^\n]+?\\)\\)??")
        (tag-re "\\(?:[ ]*:\\([[:alnum:]_@#%%:]+\\):\\)?"))
    (format "^%s[ ]+%s%s%s[ ]*$" level-re kw-re title-re tag-re)))

(defconst org-x-dag-prop-drawer-re
  (concat
   "^[\t ]*:PROPERTIES:[\t ]*\n"
   "\\(\\(.\\|\n\\)*?\\)"
   "[\t ]*:END:[\t ]*$"))

(defun org-x-dag-property-block (end)
  "Return (DRWR-BEG BEG END DRWR-END) of the property block.

This is like `org-get-property-block' except way faster, and
assumes the point is on the first line of the headline in
question. END is the end of the search space (presumably the next
headline)."
  (save-excursion
    (when (re-search-forward org-x-dag-prop-drawer-re end t)
      (list (match-beginning 0)
            (match-beginning 1)
            (match-end 1)
            (match-end 0)))))

(defun org-x-dag-parse-this-planning (prop-beg)
  "Parse the planning element for this headline.

Assume point is somewhere on the first line of headline. Note
that it is invalid for the planning keyword to start on anything
other than the next line.

PROP-BEG is the beginning position of the property drawer and
used for optimization."
  (save-excursion
    (forward-line 1)
    (when (and (< (point) prop-beg) (looking-at org-planning-line-re))
      (org-element-planning-parser prop-beg))))

(defun org-x-dag-get-buffer-nodes (file-meta kws target-props)
  (goto-char (point-min))
  (let* ((line-re (org-x-dag-line-regexp kws))
         (first-hl (org-x-dag-next-headline))
         ;; If not on a headline, check for a property drawer with links in it
         (this-file-links (unless (= ?* (following-char))
                            (org-x-dag-get-parent-links nil first-hl)))
         (pps (--map (cons it (org-re-property it nil t)) target-props))
         (id-prop (org-re-property "ID" nil t))
         ;; stack vars
         bare-stack node-level bury-level
         ;; data vars
         this-id this-level this-todo this-tags this-links this-pblock pbeg pend
         ;; return
         acc acc-links)
    (when first-hl
      (goto-char first-hl))
    (while (looking-at line-re)
      ;; Keep track of how 'deep' we are in a given org-tree using a stack. The
      ;; stack will have members like (LEVEL KEY TAGS) where LEVEL is the level
      ;; of the headline and KEY is the node key if it has a keyword, and TAGS
      ;; is a list of tags for the headlines. Only add a node to the accumulator
      ;; if it has a keyword and an ID property, and only include its parent
      ;; headline if the parent also has a keyword.
      (setq this-point (car (match-data t))
            this-level (length (match-string 1))
            this-todo (match-string 2)
            this-title (match-string 3)
            this-tags (match-string-no-properties 4)
            next-pos (or (org-x-dag-next-headline) (point-max)))
      (unless (and bury-level (< bury-level this-level))
        ;; Adjust the stack so that the top headline is the parent of the
        ;; current headline
        (when (and node-level (<= this-level node-level))
          (setq node-level nil))
        (unless node-level
          (while (and bare-stack (<= this-level (nth 0 (car bare-stack))))
            (!cdr bare-stack)))
        ;; Add the current headline to accumulator if it is a node, but only if
        ;; its parent is a node or none of its parents are nodes
        (cond
         ((and this-todo
               (setq this-pblock (org-x-dag-property-block next-pos)
                     pbeg (nth 1 this-pblock)
                     pend (nth 2 this-pblock)
                     this-id (org-x-dag-get-local-property pbeg pend id-prop)))
          (setq bury-level nil
                this-links (or (org-x-dag-get-parent-links (nth 3 this-pblock)
                                                        next-pos)
                               (unless node-level
                                 (nth 2 (car bare-stack)))))
          (when this-tags
            (setq this-tags (split-string this-tags ":")))
          (when (and (not node-level) bare-stack)
            (setq this-tags (->> (car bare-stack)
                                 (nth 1)
                                 (append this-tags))))
          (-> (list
               :id this-id
               :parents this-links
               :node-meta
               `(,@file-meta
                 :point ,this-point
                 :level ,this-level
                 :todo ,this-todo
                 :title ,(or this-title "")
                 :tags ,this-tags
                 :planning ,(org-x-dag-parse-this-planning (nth 0 this-pblock))
                 :props ,(org-x-dag-get-local-properties pbeg pend pps)))
              (!cons acc))
          (when this-links
            (!cons (cons this-id this-links) acc-links))
          (setq node-level this-level))
         ;; Underneath a node but not on a node, therefore we are buried
         (node-level
          (setq bury-level this-level))
         ;; Anything else means we are on a bare headline above any nodes
         (t
          (setq bury-level nil
                node-level nil)
          (when this-tags
            (setq this-tags (split-string this-tags ":")))
          (-> (list this-level
                    (append this-tags (or (nth 1 (car bare-stack)) org-file-tags))
                    (or (org-x-dag-get-parent-links nil next-pos)
                        (nth 2 (car bare-stack))
                        this-file-links))
              (!cons bare-stack)))))
      (goto-char next-pos))
    (list (nreverse acc) acc-links)))

(defun org-x-dag-buffer-nodes-to-tree (nodes)
  (cl-labels
      ((get-level
        (node)
        (plist-get (plist-get node :node-meta) :level))
       (mk-tree
        (parent nodes)
        (-let* (((p . cs) parent)
                (pi (get-level p))
                stop n i res)
          (while (and (not stop) nodes)
            (setq n (car nodes)
                  i (get-level n))
            (unless (setq stop (<= i pi))
              (setq res (mk-tree `(,n) (cdr nodes))
                    nodes (cdr res))
              (!cons (car res) cs)))
          `((,p ,@cs) . ,nodes))))
    (let (acc res)
      (while nodes
        (setq res (mk-tree `(,(car nodes)) (cdr nodes))
              nodes (cdr res))
        (!cons (car res) acc))
      acc)))

;; type BS a = Either String a
(defmacro org-x-dag-bs (key data)
  (pcase key
    ((or :error :valid) `(list ,key ,data))
    (e (error "Invalid status key: %s" key))))

(defmacro org-x-dag-bs>>= (bs form)
  (declare (indent 1))
  `(pcase ,bs
     (`(:error ,_) ,bs)
     (`(:valid ,it) ,form)
     (e (error "Learn to use monads, dummy; this isn't one: %s" e))))

(defmacro org-x-dag-bs-fmap (bs form)
  (declare (indent 1))
  (let ((b (make-symbol "--bs")))
    `(let ((,b ,bs))
       (pcase ,b
         (`(:error ,_) ,b)
         (`(:valid ,it) (org-x-dag-bs :valid ,form))
         (e (error "Learn to use functors, dummy; this isn't one: %s" e))))))

(defun org-x-dag-bs-error-p (bs)
  (eq (car bs) :error))

;; [Status a] -> b -> (a -> a -> Status Bool) -> (a -> Bool) -> (a -> Status b)
;; -> Status b
(defmacro org-x-dag-bs-fold-children (bss default rank-form stop-form trans-form)
  (declare (indent 2))
  (let ((err (org-x-dag-bs :error "Child error")))
    `(-if-let ((x . xs) ,bss)
         (if (org-x-dag-bs-error-p x) ',err
           (let ((acc x) r)
             (while xs
               (setq x (car xs))
               (if (org-x-dag-bs-error-p x)
                   (setq acc ',err
                         xs nil)
                 (setq it x
                       r ,rank-form)
                 (unless r
                   (error "You forgot the difference between Maybe and Either"))
                 (if (org-x-dag-bs-error-p r)
                     (setq acc r
                           xs nil)
                   (when r
                     (setq acc x))
                   (if ,stop-form
                       (setq acc (org-x-dag-bs :valid acc)
                             xs nil)
                     (!cdr xs)))))
             (org-x-dag-bs>>= acc ,trans-form)))
       (org-x-dag-bs :valid ,default))))

(defun org-x-dag-bs-error-kw (type-name kw)
  (->> (format "%ss cannot have keyword '%s" type-name kw)
       (org-x-dag-bs :error)))

(defmacro org-x-dag-bs-action-with-closed (node-data type-name canc-bs-form
                                                     done-form open-form)
  (declare (indent 2))
  (let ((c (make-symbol "--closed")))
    `(cl-flet
         ((complete-time
           (epoch canceledp)
           (list :epoch epoch :canceledp canceledp)))
       (-let (((&plist :todo it-todo :planning it-planning) ,node-data))
         (-if-let (,c (-some->> it-planning
                        (org-ml-get-property :closed)
                        (org-ml-timestamp-get-start-time)
                        (org-ml-time-to-unixtime)))
             (cond
              ((equal it-todo org-x-kw-canc)
               (let ((it-comptime (complete-time ,c t)))
                 (org-x-dag-bs :valid ,canc-bs-form)))
              ((equal it-todo org-x-kw-done)
               (let ((it-comptime (complete-time ,c nil)))
                 ,done-form))
              (t
               (->> (format "Closed %s must be marked CANC/DONE" ,type-name)
                    (org-x-dag-bs :error))))
           (cond
            ((member it-todo org-x-done-keywords)
             (->> (format "DONE/CANC %s must be closed" ,type-name)
                  (org-x-dag-bs :error)))
            (t
             ,open-form)))))))

(defun org-x-dag-bs-action-project-inner (node-data child-bss)
  (cl-flet
      ((new-proj
        (status)
        (org-x-dag-bs :valid `(:sp-proj ,@status))))
    ;; rankings
    ;; *-active > proj-wait > proj-held > (proj-stuck == iter-empty) > *-complete
    (org-x-dag-bs-action-with-closed node-data "projects"
      `(:sp-proj :proj-complete ,it-comptime)

      ;; done form
      (org-x-dag-bs-fold-children child-bss `(:sp-task :task-complete ,it-comptime)
        ;; TODO this could be slightly more efficient if the error type is
        ;; returned in this form and not the last
        (->> (pcase `(,acc ,it)
               (`((:sp-proj :proj-complete ,_) (:sp-proj :proj-complete ,_)) nil)
               (`((:sp-iter :iter-complete ,_) (:sp-iter :iter-complete ,_)) nil)
               (`((:sp-task :task-complete ,_) (:sp-task :task-complete ,_)) nil)
               (`((:sp-proj :proj-complete ,_) ,_) t)
               (`((:sp-iter :iter-complete ,_) ,_) t)
               (`((:sp-task :task-complete ,_) ,_) t)
               (`(,_ (:sp-proj :proj-complete ,_)) nil)
               (`(,_ (:sp-iter :iter-complete ,_)) nil)
               (`(,_ (:sp-task :task-complete ,_)) nil))
             (org-x-dag-bs :valid))
        (pcase acc
          (`(:sp-proj :proj-complete ,_) nil)
          (`(:sp-iter :iter-complete ,_) nil)
          (`(:sp-task :task-complete ,_) nil)
          (_ t))
        (pcase it
          ((or `(:sp-proj :proj-complete ,_)
               `(:sp-iter :iter-complete ,_)
               `(:sp-task :task-complete ,_))
           (org-x-dag-bs :valid `(:sp-proj :proj-complete ,it-comptime)))
          (_ (org-x-dag-bs :error "Completed projects cannot have active children"))))

      ;; undone form
      (-let* (((sched dead) (-some->> it-planning
                              (org-ml-get-properties '(:scheduled :deadline))))
              (task-default `(:sp-task :task-active (,it-todo ,sched ,dead))))
        (cond
         ((equal it-todo org-x-kw-hold)
          (new-proj :proj-held))
         ((and sched child-bss)
          (org-x-dag-bs :error "Projects cannot be scheduled"))
         ((equal it-todo org-x-kw-todo)
          (org-x-dag-bs-fold-children child-bss task-default
            (->> (pcase `(,acc ,it)
                   (`((:sp-proj :proj-active) ,_) nil)
                   (`((:sp-iter :iter-active ,_) ,_) nil)
                   (`((:sp-task :task-active, _) ,_) nil)
                   (`(,_ (:sp-proj :proj-active)) t)
                   (`(,_ (:sp-iter :iter-active ,_)) t)
                   (`(,_ (:sp-task :task-active ,_)) t)

                   (`((:sp-proj :proj-wait) ,_) nil)
                   (`(,_ (:sp-proj :proj-wait)) t)

                   (`((:sp-proj :proj-held) ,_) nil)
                   (`(,_ (:sp-proj :proj-held)) t)

                   (`((:sp-proj :proj-stuck) ,_) nil)
                   (`((:sp-iter :iter-empty) ,_) nil)
                   (`(,_ (:sp-proj :proj-stuck)) t)
                   (`(,_ (:sp-iter :iter-empty)) t)

                   ;; any pair that makes it this far is completed in both, which means
                   ;; neither takes precedence, which means choose the left one
                   (`(,_ ,_) nil))
                 (org-x-dag-bs :valid))

            ;; early stop
            (pcase acc
              (`(:sp-proj :proj-active) t)
              (`(:sp-iter :iter-active ,_) t)
              (`(:sp-task :task-active ,_) t)
              (_ nil))

            ;; child -> parent translation
            (pcase it
              ((or `(:sp-proj :proj-complete ,_)
                   `(:sp-task :task-complete ,_)
                   `(:sp-iter :iter-complete ,_))
               (org-x-dag-bs :error "Active projects must have at least one active child"))
              (`(:sp-proj . ,s) (new-proj s))
              (`(:sp-iter :iter-active ,_) (new-proj :proj-active))
              (`(:sp-iter :iter-empty) (new-proj :proj-stuck))
              (`(:sp-task :task-active (,c-todo ,c-sched ,_))
               (cond
                ((equal c-todo org-x-kw-todo) (->> (if c-sched :proj-active
                                                     :proj-stuck)
                                                   (new-proj)))
                ((equal c-todo org-x-kw-next) (new-proj :proj-active))
                ((equal c-todo org-x-kw-wait) (new-proj :proj-wait))
                ((equal c-todo org-x-kw-hold) (new-proj :proj-hold))
                (t (org-x-dag-bs-error-kw "Task action" c-todo)))))))
         (child-bss
          (org-x-dag-bs-error-kw "Project action" it-todo))
         (t
          task-default))))))

(defun org-x-dag-node-data-is-iterator-p (node-data)
  (-let (((&plist :props) node-data))
    (-when-let (p (alist-get org-x-prop-parent-type props nil nil #'equal))
      (equal p org-x-prop-parent-type-iterator))))

;; TODO these next two could be made more efficient by cutting out the
;; earlystop form and returning error in the rank form (the trans form is
;; still needed in case there is only one child)
(defun org-x-dag-bs-action-subiter-complete-fold (child-bss type-name comp-key)
  (declare (indent 1))
  (org-x-dag-bs-fold-children child-bss `(,comp-key ,it-comptime)
    (->> (pcase `(,si-a ,si-b)
           (`((:si-complete ,_) (:si-complete ,_)) nil)
           (`((:si-complete ,_) ,_) t)
           (`(,_ (:si-complete ,_)) nil))
         (org-x-dag-bs :valid))
    (pcase acc
      (`(:si-complete ,_) nil)
      (_ t))
    (pcase it
      (`(:si-complete ,_)
       (org-x-dag-bs :valid `(,comp-key ,it-comptime)))
      (_
       (->> (format "Completed %s cannot have active children" type-name)
            (org-x-dag-bs :error))))))

(defun org-x-dag-bs-action-subiter-todo-fold (child-bss type-name active-key default)
  (org-x-dag-bs-fold-children child-bss default
    (pcase `(,si-a ,si-b)
      (`((:si-active (,ts-a ,dead-a)) (:si-active (,ts-b ,dead-b)))
       (let ((dt-a (org-ml-timestamp-get-start-time ts-a))
             (dt-b (org-ml-timestamp-get-start-time ts-b)))
         (cond
          ((not (eq dead-a dead-b))
           (->> "All sub-iter timestamps must be scheduled or deadlined"
                (org-x-dag-bs :error)))
          ((xor (org-ml-time-is-long dt-a) (org-ml-time-is-long dt-b))
           (->> "All sub-iter timestamps must be long or short"
                (org-x-dag-bs :error)))
          (t
           ;; ASSUME this won't fail since the datetimes are assumed to be the
           ;; same length as per rules above
           (org-x-dag-bs :valid (org-x-dag-datetime< dt-a dt-b))))))
      (`((:si-active ,_) ,_) (org-x-dag-bs :valid nil))
      (`(,_ (:si-active ,_)) (org-x-dag-bs :valid t)))
    (pcase acc
      (`(:si-active ,_) t)
      (_ nil))
    (pcase it
      (`(:si-complete ,_)
       (->> (format "Active %s must have at least one active child" type-name)
            (org-x-dag-bs :error)))
      (`(:si-active ,ts-data)
       (org-x-dag-bs :valid `(,active-key ,ts-data))))))

(defun org-x-dag-node-is-iterator-p (node)
  (org-x-dag-node-data-is-iterator-p (plist-get node :node-data)))

(defun org-x-dag-bs-action-subiter-inner (node-data child-bss)
  (org-x-dag-bs-action-with-closed node "sub-iterators"
    `(:si-complete ,it-comptime)
    (org-x-dag-bs-action-subiter-complete-fold child-bss
      "sub-iterators" :si-complete)
    (-let (((sched dead) (-some->> it-planning
                           (org-ml-get-properties '(:scheduled :deadline)))))
      (cond
       ((and sched child-bss)
        (org-x-dag-bs :error "Sub-iterators with children cannot be scheduled"))
       ((and dead child-bss)
        (org-x-dag-bs :error "Sub-iterators with children cannot be deadlined"))
       ((and (not child-bss) (not (xor sched dead)))
        (org-x-dag-bs :error "Sub-iterators must either be deadlined or scheduled"))
       ((org-x-dag-node-data-is-iterator-p node-data)
        (org-x-dag-bs :error "Iterators cannot be nested"))
       ((equal it-todo org-x-kw-todo)
        (org-x-dag-bs-action-subiter-todo-fold child-bss
          "sub-iterator" :si-active
          `(:si-active ,(or sched dead) ,(and dead t))))
       (t
        (org-x-dag-bs-error-kw "Sub-iterator" it-todo))))))

(defun org-x-dag-bs-action-iter-inner (node-data child-bss)
  (org-x-dag-bs-action-with-closed node-data "iterators"
    `(:iter-complete ,it-comptime)
    (org-x-dag-bs-action-subiter-complete-fold child-bss
      "iterators" :iter-complete)
    (cond
     ((and child-bss (-some->> it-planning (org-ml-get-property :scheduled)))
      (org-x-dag-bs :error "Iterators cannot be scheduled"))
     ;; TODO also check for timeshift and archive props
     ((equal it-todo org-x-kw-todo)
      (org-x-dag-bs-action-subiter-todo-fold child-bss
        "iterator" :iter-active
        '(:iter-empty)))
     (t
      (org-x-dag-bs-error-kw "Iterator" it-todo)))))

(defun org-x-dag-bs-epg-inner (node child-bss)
  (org-x-dag-bs-action-with-closed node "endpoint goal"
    `(:complete ,comp-time)
    (org-x-dag-bs-fold-children child-bss `(:complete ,comp-time)
      (->> (pcase `(,acc ,it)
             (`((:complete ,_) (:complete ,_)) nil)
             (`(,_ (:complete ,_)) nil)
             (`((:complete ,_) ,_) t))
           (org-x-dag-bs :valid))
      (pcase acc
        (`(:complete ,_) nil)
        (_ t))
      (pcase it
        (`(:complete ,_)
         (org-x-dag-bs :valid `(:complete ,it-comptime)))
        (_ (org-x-dag-bs :error "Completed EPGs cannot have active children"))))
    (cond
     ((-some->> it-planning (org-ml-get-property :scheduled))
      (org-x-dag-bs :error "EPGs cannot be scheduled"))
     ((equal it-todo org-x-kw-todo)
      (let ((dead (-some->> it-planning (org-ml-get-property :deadline))))
        (org-x-dag-bs-fold-children child-bss `(:active ,dead)
          (->> (pcase `(,acc ,it)
                 (`((:active ,_) (:active ,_)) nil)
                 (`(,_ (:active ,_)) t)
                 (`((:active ,_) ,_) nil))
               (org-x-dag-bs :valid))
          nil
          (pcase it
            (`(:active ,c-dead)
             ;; TODO I might want to enforce the same precision here like I do
             ;; for iterators
             (let ((c-epoch (-some->> c-dead
                              (org-ml-timestamp-get-start-time)
                              (org-ml-time-to-unixtime)))
                   (p-epoch (-some->> dead
                              (org-ml-timestamp-get-start-time)
                              (org-ml-time-to-unixtime))))
               (cond
                ((and c-epoch p-epoch (<= c-epoch p-epoch))
                 (org-x-dag-bs :valid `(:active ,dead)))
                ((not dead)
                 (org-x-dag-bs :valid `(:active ,c-dead)))
                (t
                 (org-x-dag-bs :error "Child deadlines must be before parent deadlines")))))
            (_
             (org-x-dag-bs :error "Active EPGs must have at least one active child"))))))
     (t
      (org-x-dag-bs-error-kw "Endpoint goal" it-todo)))))

(defun org-x-dag-bs-with-treetop-error (tree parent-id)
  (declare (indent 3))
  (-let* (((node . children) tree)
          ((&plist :id i :parents links :node-meta m) node)
          (ps (cons parent-id links))
          (this (->> (org-x-dag-bs :error "Children not allowed")
                     (org-x-dag-node i ps parent-id m))))
    (cons this (--mapcat (org-x-dag-bs-with-treetop-error it i) children))))

(defun org-x-dag-bs-with-treetop (tree node-fun)
  (declare (indent 3))
  (-let* ((((&plist :id i :parents links :node-meta m) . children) tree)
          (bs (if children (org-x-dag-bs :error "Children not allowed")
                (funcall node-fun m)))
          (top (org-x-dag-node i links nil m bs)))
    (cons top (--mapcat (org-x-dag-bs-with-treetop-error it i) children))))

(defun org-x-dag-node (id parents buffer-parent node-meta bs)
  (list :id id
        :parents parents
        :node-meta (list :hl-meta node-meta
                         :buffer-parent buffer-parent
                         :buffer-status bs)))

(defmacro org-x-dag-node-fmap (node form)
  (declare (indent 1))
  ;; TODO not efficient (may or may not matter)
  `(-let (((&plist :id i
                   :parents ps
                   :node-meta (&plist :hl-meta h
                                      :buffer-parent p
                                      :buffer-status it))
           ,node))
     (org-x-dag-node i ps p h ,form)))

(defun org-x-dag-bs-with-children (tree parent child-fun node-fun concat-fun)
  (declare (indent 3))
  ;; TODO this is super inefficient, make a plist mapper function
  (-let* (((node . children) tree)
          ((&plist :id i :parents links :node-meta m) node)
          ((shallow rest) (->> (--map (funcall child-fun it i) children)
                               (apply #'-zip-lists)))
          (ps (if parent (cons parent links) links)))
    (list (->> shallow
               (--map (plist-get (plist-get it :node-meta) :buffer-status))
               (funcall node-fun m)
               (org-x-dag-node i ps parent m))
          (funcall concat-fun shallow rest))))

;; Tree a -> (Tree a -> (b, [d])) -> (a -> [b] -> c) -> (c, [d])
(defun org-x-dag-bs-with-children-1 (tree parent child-fun node-fun)
  (org-x-dag-bs-with-children tree parent child-fun node-fun
    (lambda (shallow deep)
      (append shallow (-flatten-n 1 deep)))))

;; Tree a -> (Tree a -> (b, ([d], [e]))) -> (a -> [b] -> c) -> (c, ([d], [e]))
(defun org-x-dag-bs-with-children-2 (tree parent child-fun node-fun)
  (org-x-dag-bs-with-children tree parent child-fun node-fun
    (lambda (shallow deep)
      (--reduce-from (-let (((a b) acc)
                            ((as bs) it))
                       `((,@as ,@a) (,@bs ,@b)))
                     `(,shallow nil)
                     deep))))

(defun org-x-dag-bs-action-subiter (tree parent)
  (org-x-dag-bs-with-children-1
   tree
   parent
   #'org-x-dag-bs-action-subiter
   #'org-x-dag-bs-action-subiter-inner))

(defun org-x-dag-bs-action-iter (tree parent)
  (org-x-dag-bs-with-children-1
   tree
   parent
   #'org-x-dag-bs-action-subiter
   (lambda (node child-bss)
     (org-x-dag-node-fmap node
       (org-x-dag-bs-fmap (org-x-dag-bs-action-iter-inner it child-bss)
         `(:sp-proj it))))))

(defun org-x-dag-bs-action-project (tree parent)
  (if (org-x-dag-node-is-iterator-p (car tree))
      (-let (((iter subiters) (org-x-dag-bs-action-iter tree nil)))
        `(,iter (nil ,subiters)))
    (org-x-dag-bs-with-children-2
     tree
     parent
     #'org-x-dag-bs-action-project
     #'org-x-dag-bs-action-project-inner)))

;; TODO need to check for created timestamps
(defun org-x-dag-bs-action (node-tree)
  ;; TODO these types might not line up properly
  (-let (((p (ps is)) (org-x-dag-bs-action-project node-tree nil)))
    `(,p ,@ps ,@is)))

(defun org-x-dag-bs-epg-outer (tree parent)
  (org-x-dag-bs-with-children-1
   tree
   parent
   #'org-x-dag-bs-epg-outer
   #'org-x-dag-bs-epg-inner))

(defun org-x-dag-bs-prefix (key nodes)
  (--map (org-x-dag-node-fmap it (org-x-dag-bs-fmap it `(,key ,it))) nodes))

(defun org-x-dag-bs-epg (tree)
  (-let (((n ns) (org-x-dag-bs-epg-outer tree nil)))
    (org-x-dag-bs-prefix :endpoint `(,n ,@ns))))

(defun org-x-dag-bs-toplevel-goal-inner (type-name node-data child-bss)
  (-let (((&plist :planning :todo) node-data))
    (cond
     (planning
      (->> (format "%ss cannot have planning elements" type-name)
           (org-x-dag-bs :error)))
     ((-any #'org-x-dag-bs-error-p child-bss)
      (org-x-dag-bs :error "Child error"))
     ((equal todo org-x-kw-todo)
      (org-x-dag-bs :valid '(:active)))
     (t
      (org-x-dag-bs-error-kw type-name todo)))))

(defun org-x-dag-bs-toplevel-goal-outer (type-name tree parent)
  (org-x-dag-bs-with-children-1
   tree
   parent
   (lambda (tree parent)
     (org-x-dag-bs-toplevel-goal-outer type-name tree parent))
   (lambda (node-data child-bss)
     (org-x-dag-bs-toplevel-goal-inner type-name node-data child-bss))))

(defun org-x-dag-bs-toplevel-goal (type-name type-key tree)
  (-let (((n ns) (org-x-dag-bs-toplevel-goal-outer type-name tree nil)))
    (org-x-dag-bs-prefix type-key `(,n ,@ns))))

(defun org-x-dag-bs-ltg (tree)
  (org-x-dag-bs-toplevel-goal "LTG" :lifetime tree))

(defun org-x-dag-bs-svg (tree)
  (org-x-dag-bs-toplevel-goal "SVG" :survival tree))

;; planning

(defun org-x-dag-bs-qtp-inner (node-data)
  (org-x-dag-bs-action-with-closed node-data "quarterly plan"
    `(:complete ,it-comptime)
    (org-x-dag-bs :valid `(:complete ,it-comptime))
    (cond
     ((-some->> it-planning (org-ml-get-properties :scheduled))
      (org-x-dag-bs :error "QTPs cannot be scheduled"))
     ((equal it-todo org-x-kw-todo)
      (-if-let (dead (-some->> it-planning (org-ml-get-properties :deadline)))
          (-let* (((&plist :tags) node-data)
                  (tag-dt (org-x-dag-quarter-tags-to-date tags))
                  (dead-dt (->> (org-ml-timestamp-get-start-time dead)
                                (org-x-dag-datetime-split)
                                (car))))
            (if (org-x-dag-datetime< tag-date dead-dt)
                (org-x-dag-bs :valid `(:active ,dead))
              (->> "QTP deadlines must be due after the quarter starts"
                   (org-x-dag-bs :error))))
        (org-x-dag-bs :valid '(:active nil))))
     (t
      (org-x-dag-bs-error-kw "QTP" it-todo)))))

(defun org-x-dag-bs-wkp-inner (node-data)
  (org-x-dag-bs-action-with-closed node-data "weekly plan"
    `(:complete ,it-comptime)
    (org-x-dag-bs :valid `(:complete ,it-comptime))
    (cond
     ((-some->> it-planning (org-ml-get-properties :scheduled))
      (org-x-dag-bs :error "WKPs cannot be scheduled"))
     ((-some->> it-planning (org-ml-get-properties :deadline))
      (org-x-dag-bs :error "WKPs cannot be deadlined"))
     ((equal it-todo org-x-kw-todo)
      (org-x-dag-bs :valid `(:active)))
     (t
      (org-x-dag-bs-error-kw "WKP" it-todo)))))

(defun org-x-dag-bs-dlp-inner (node-data)
  (org-x-dag-bs-action-with-closed node-data "daily metablock"
    `(:complete ,it-comptime)
    (org-x-dag-bs :valid `(:complete ,it-comptime))
    (cond
     ((-some->> it-planning (org-ml-get-properties :deadline))
      (org-x-dag-bs :error "Daily metablocks cannot be deadlined"))
     ((equal it-todo org-x-kw-todo)
      (-if-let ((sched-date sched-time) (-some->> it-planning
                                          (org-ml-get-property :scheduled)
                                          (org-ml-timestamp-get-start-time)
                                          (org-x-dag-datetime-split)))
          (if (not sched-time)
              (org-x-dag-bs :error "Daily metablocks must have scheduled time")
            (-let* (((&plist :tags) node-data)
                    (tag-date (org-x-dag-daily-tags-to-date tags)))
              (if (org-x-dag-datetime= tag-date sched-date)
                  (org-x-dag-bs :valid `(:active))
                (org-x-dag-bs :error "Daily metablocks must be scheduled within their date"))))
        (org-x-dag-bs :error "Daily metablocks must be scheduled")))
     (t
      (org-x-dag-bs-error-kw "Daily metablock" it-todo)))))

(defun org-x-dag-bs-qtp (tree)
  (-let (((n ns) (org-x-dag-bs-with-treetop tree #'org-x-dag-bs-qtp-inner)))
    (org-x-dag-bs-prefix :quarterly `(,n ,@ns))))

(defun org-x-dag-bs-wkp (tree)
  (-let (((n ns) (org-x-dag-bs-with-treetop tree #'org-x-dag-bs-wkp-inner)))
    (org-x-dag-bs-prefix :weekly `(,n ,@ns))))

(defun org-x-dag-bs-dlp (tree)
  (-let (((n ns) (org-x-dag-bs-with-treetop tree #'org-x-dag-bs-dlp-inner)))
    (org-x-dag-bs-prefix :daily `(,n ,@ns))))

(defun org-x-dag-get-file-nodes (file group)
  (-let* ((meta (list :file file
                      :group group
                      :category (f-base file)))
          (def-props `(,org-x-prop-created))
          (props (->> (pcase group
                        (:action (list org-x-prop-parent-type
                                       org-x-prop-time-shift
                                       "ARCHIVE")))
                      (append def-props)))
          (bs-fun (pcase group
                    (:action #'org-x-dag-bs-action)
                    (:lifetime #'org-x-dag-bs-ltg)
                    (:survival #'org-x-dag-bs-svg)
                    (:endpoint #'org-x-dag-bs-epg)
                    (:quarterly #'org-x-dag-bs-qtp)
                    (:weekly #'org-x-dag-bs-wkp)
                    (:daily #'org-x-dag-bs-dlp)))
          ((nodes links)
           (org-x-with-file file
             (org-x-dag-get-buffer-nodes meta org-todo-keywords-1 props))))
    (list (->> (org-x-dag-buffer-nodes-to-tree nodes)
               (-mapcat bs-fun))
          links)))

;; network status

;; terminology
;; - committed: x -> goal
;; - fulfilled: action -> x
;; - planned: plan -> x
;; - scheduled: x -> plan
;; - active: x -> action

(defun org-x-dag-id-link-group (adjlist id)
  (-> (ht-get adjlist id)
      (plist-get :node-meta)
      (plist-get :hl-meta)
      (plist-get :group)))

(defun org-x-dag-plist-map (plist key fun)
  (declare (indent 2))
  (plist-put plist key (funcall fun (plist-get plist key))))

(defun org-x-dag-plist-cons (plist key x)
  (declare (indent 2))
  (org-x-dag-plist-map plist key
    (lambda (xs)
      (cons x xs))))

(defmacro org-x-dag-each-links (links form)
  (declare (indent 1))
  `(--each links
     (-let (((it . it-targets) it))
       ,form)))

(defmacro org-x-dag-each-link-parent (parent-ht links form)
  (declare (indent 2))
  `(-each ,links
     (lambda (it-parent)
       (ht-set ,parent-ht it-parent))))

(defun org-x-dag-bs-error-links (msg links)
  (->> (s-join ", " links)
       (format "%s: %s" msg)
       (org-x-dag-bs :error)))

;; ;; TODO need to make two versions of this, one to propogogate the entire
;; ;; status, and another to propogate the entire key
;; ;; both cases will need to stop when they encounted a child that is already
;; ;; 'filled' (eg either already an error or already has the key)
;; (defun org-x-dag-ht-propagate-status (adjlist htbl ids)
;;   (cl-labels
;;       ((get-children
;;         (adjlist id)
;;         (->> (plist-get (ht-get adjlist id) :childre)
;;              (--filter (-> (ht-get adjlist it)
;;                            (plist-get :node-meta)
;;                            (plist-get :buffer-parent)
;;                            (equal id)))))
;;        (propagate
;;         (adjlist htbl id to-set)
;;         (--each (get-children adjlist id)
;;           (unless (ht-get htbl id)
;;             (ht-set htbl id to-set)))))
;;     (-each ids
;;       (lambda (id)
;;         (--each (get-children adjlist id)
;;           (propagate adjlist htbl it id))))
;;     htbl))

(defun org-x-dag-id-is-buffer-leaf (adjlist id)
  (->> (plist-get (ht-get adjlist id) :children)
       (--none-p (-> (ht-get adjlist it)
                     (plist-get :node-meta)
                     (plist-get :buffer-parent)
                     (equal id)))))

(defun org-x-dag-ns-toplevel (tbl adjlist links ns)
  (let ((h (alist-get tbl ns)))
    (org-x-dag-each-links links
      (ht-set h it (org-x-dag-bs-error-links "Invalid links" it-targets)))
    ns))

(defun org-x-dag-ns-ltg (adjlist links ns)
  (org-x-dag-ns-toplevel :lifetime adjlist links ns))

(defun org-x-dag-ns-svg (adjlist links ns)
  (org-x-dag-ns-toplevel :survival adjlist links ns))

(defun org-x-dag-ht-add-links (id htbl key targets)
  (let (r)
    (--each targets
      (->> (if (setq r (ht-get htbl it))
               (org-x-dag-bs-fmap r
                 (org-x-dag-plist-cons it key id))
             (org-x-dag-bs :valid `(,key (,id))))
           (ht-set htbl it)))))

(defun org-x-dag-ns-with-valid (ns adjlist cur-key id targets keys valid-fun)
  (declare (indent 5))
  (cl-flet*
      ((key-group
        (id)
        (let ((g (org-x-dag-id-link-group adjlist id)))
          (if (member g keys) g :invalid)))
       (parent-group
        (h adjlist id)
        (cond
         ((org-x-dag-bs-error-p (ht-get h id)) :error)
         ((not (org-x-dag-id-is-buffer-leaf adjlist id)) :non-leaf)
         (t :valid)))
       (reduce-valid
        (grouped-targets acc key)
        (-let* (((acc-keyed acc-error acc-non-leaf) acc)
                (h (alist-get key ns))
                ((&alist :valid v :error e :non-leaf n)
                 (->> (alist-get key grouped-targets)
                      (--group-by (parent-group h adjlist it)))))
          `(((,key ,v) ,@acc-keyed)
            (,@e ,@acc-error)
            (,@n ,@acc-non-leaf)))))
    (let ((grouped (-group-by #'key-group targets))
          (cur-h (alist-get cur-key ns)))
      (-if-let (invalid (alist-get :invalid grouped))
          (ht-set cur-h id (org-x-dag-bs-error-links "Invalid links" invalid))
        (-let (((valid err non-leaf)
                (--reduce-from (reduce-valid grouped acc it) nil keys)))
          (cond
           (err
            (->> (org-x-dag-bs-error-links "Linked to invalid links" err)
                 (ht-set cur-h id)))
           (non-leaf
            (->> (org-x-dag-bs-error-links "Linked to non-leaf nodes" non-leaf)
                 (ht-set cur-h id)))
           (t
            (funcall valid-fun id cur-h valid))))))))

(defun org-x-dag-ns-epg (adjlist links ns)
  (-let (((&alist :lifetime ht-l) ns))
    (org-x-dag-each-links links
      (org-x-dag-ns-with-valid ns adjlist :endpoint it it-targets
        '(:lifetime)
        (lambda (id this-h res)
          (-let (((&alist :lifetime l) res))
            (ht-set this-h id (org-x-dag-bs :valid `(:committed ,l)))
            ;; this function doesn't treat the value of the nstat hash table
            ;; as an either, but at this point in the code, we have already
            ;; ruled out the fact that these are all Rights since we use that
            ;; as a test of validity
            (org-x-dag-ht-add-links id ht-l :fulfilled l)))))
    ns))

(defun org-x-dag-ht-get-maybe (htbl id key)
  (pcase (ht-get htbl id)
    (`(:error ,_) nil)
    (`(:valid ,c) (plist-get c key))))

(defun org-x-dag-ns-qtp (adjlist links ns)
  (-let (((&alist :lifetime ht-l :endpoint ht-e :quarterly ht-q) ns))
    (org-x-dag-each-links links
      (org-x-dag-ns-with-valid ns adjlist :quarterly it it-targets
        '(:lifetime :endpoint)
        (lambda (id this-h res)
          (-let (((&alist :lifetime l :endpoint e) res))
              (ht-set this-h id (org-x-dag-bs :valid `(:committed (,@e ,@l))))
              (->> (--mapcat (org-x-dag-ht-get-maybe ht-e it :committed) e)
                   (org-x-dag-ht-add-links id ht-l :planned))
              (org-x-dag-ht-add-links id ht-e :planned e)
              (org-x-dag-ht-add-links id ht-l :planned l)))))
    ns))

(defun org-x-dag-ns-wkp (adjlist links ns)
  (-let (((&alist :quarterly ht-q) ns))
    (org-x-dag-each-links links
      (org-x-dag-ns-with-valid ns adjlist :weekly it it-targets
        '(:quarterly)
        (lambda (id this-h res)
          (-let (((&alist :quarterly q) res))
            (ht-set this-h id (org-x-dag-bs :valid `(:committed ,q)))
            (org-x-dag-ht-add-links id ht-q :planned q)))))
    ns))

(defun org-x-dag-ns-action (adjlist links ns)
  (-let (((&alist :endpoint ht-e :lifetime ht-l :survival ht-s) ns))
    (org-x-dag-each-links links
      (org-x-dag-ns-with-valid ns adjlist :action it it-targets
        '(:survival :endpoint :lifetime)
        (lambda (id this-h res)
          (-let (((&alist :survival s :endpoint e :lifetime l) res))
            (->> (org-x-dag-bs :valid `(:committed (,@e ,@l ,@s)))
                 (ht-set this-h it))
            (->> (--mapcat (org-x-dag-ht-get-maybe ht-e it :committed) e)
                 (org-x-dag-ht-add-links id ht-l :fulfilled))
            (org-x-dag-ht-add-links id ht-e :fulfilled e)
            (org-x-dag-ht-add-links id ht-l :fulfilled l)
            (org-x-dag-ht-add-links id ht-s :fulfilled s)))))
    ns))

(defun org-x-dag-ns-dlp (adjlist links ns)
  (cl-flet
      ((get-committed
        (htbl ids)
        (--mapcat (org-x-dag-ht-get-maybe htbl it :committed) ids)))
    (-let (((&alist :action ht-a :quarterly ht-q :weekly ht-w) ns))
      (org-x-dag-each-links links
        (org-x-dag-ns-with-valid ns adjlist :daily it it-targets
          '(:action :weekly)
          ;; TODO this won't allow links to project actions since those aren't
          ;; leaves
          (lambda (id this-h res)
            (-let (((&alist :action a :weekly w) res))
              (let ((qgoals (->> (get-committed ht-w w)
                                 (get-commited ht-q)))
                    (agoals (get-committed ht-a a)))
                (-if-let (gs (-intesection qgoals agoals))
                    (progn
                      (->> (list :scheduled w
                                 :committed (-uniq gs)
                                 :active a)
                           (org-x-dag-bs :valid)
                           (ht-set this-h id))
                      ;; TODO add the goals to their goal links? (this might be
                      ;; useful later when displaying)
                      (org-x-dag-ht-add-links id ht-w :planned w)
                      (org-x-dag-ht-add-links id ht-a :planned a))
                  (->> (org-x-dag-bs :error "Non overlapping goals")
                       (ht-set ht-d id))))))))
      ns)))

(defun org-x-dag-ht-propagate-endpoint (adjlist ns)
  (cl-labels
      ((get-children
        (adjlist id)
        (->> (plist-get (ht-get adjlist id) :children)
             (--filter (-> (ht-get adjlist it)
                           (plist-get :node-meta)
                           (plist-get :buffer-parent)
                           (equal id)))))
       (propagate
        (adjlist htbl id to-set)
        (--each (get-children adjlist id)
          ;; TODO this needs to treat the node value like a functor
          (let ((node (ht-get htbl it)))
            (unless (org-x-dag-bs-error-p node)
              (ht-set htbl it (org-x-dag-plist-map node :committed
                                (lambda (x) (append x to-set)))))))))
    (-let (((&alist :endpoint ht-e) ns))
      (-each (ht-keys ht-e)
        (lambda (id)
          (let ((committed (plist-get (ht-get ht-e id) :committed)))
            (--each (get-children adjlist id)
              (propagate adjlist ht-e it committed)))))
      ns)))

(defun org-x-dag-get-network-status (adjlist links)
  (-let ((ns (->> '(:action :endpoint :lifetime :survival :quarterly :weekly :daily)
                  (--map (cons it (ht-create #'equal)))))
         ((&plist :action a
                  :endpoint e
                  :lifetime l
                  :survival s
                  :quarterly q
                  :weekly w
                  :daily d)
          (--reduce-from (-let (((key . link) it))
                           (org-x-dag-plist-cons acc key link))
                         nil
                         links)))
    ;; TODO add propagate step at the end
    ;;
    ;; actions: propogate down since these can be link at any point and
    ;; can have daily links pointing anywhere
    ;; TODO not sure if I want to propagate the planning field up
    ;;
    ;; endpoint goals: propagate down since these can link up to ltgs
    ;;
    ;; toplevel (ltg and svg): propagate up since only leaves can be linked
    ;; but we care if a tree is linked to anything else
    (->> (org-x-dag-ns-ltg adjlist l ns)
         (org-x-dag-ns-svg adjlist s)
         (org-x-dag-ns-epg adjlist e)
         (org-x-dag-ns-qtp adjlist q)
         (org-x-dag-ns-wkp adjlist w)
         (org-x-dag-ns-action adjlist a)
         (org-x-dag-ns-dlp adjlist d)
         (org-x-dag-ht-propagate-endpoint adjlist))))


;;; DAG SYNCHRONIZATION/CONSTRUCTION

(defun org-x-dag-get-md5 (path)
  "Get the md5 checksum of PATH."
  (org-x-with-file path (buffer-hash)))

(defun org-x-dag-get-sync-state ()
  "Return the sync state.

The returned value will be a list like (TO-REMOVE TO-INSERT
TO-UPDATE) which will contain the file paths the should be
removed from, added to, or edited within the DAG respectively."
  (cl-flet*
      ((lookup-md5
        (path)
        (alist-get path org-x-dag-sync-state nil nil #'equal))
       (get-file-md5
        (file-pair)
        (-let (((path . group) file-pair)
                (md5 ))
          (list :path path
                :group group
                :md5 (org-x-dag-get-md5 path))))
       (file-status
        (file-data)
        (-let* (((&plist :md5 new-md5 :path path) file-data)
                (old-md5 (lookup-md5 path)))
          (cond
           ((not old-md5) 'to-insert)
           ((equal old-md5 new-md5) 'no-change)
           (t 'to-update)))))
    (-let* ((file-state (org-x-dag-read-file-paths))
            (existing-files (org-x-dag-flatten-file-state file-state))
            (state-files (-map #'car org-x-dag-sync-state))
            (to-remove (->> (-map #'car existing-files)
                            (-difference state-files)))
            ((&alist 'to-insert 'to-update 'no-change)
             (->> (-map #'get-file-md5 existing-files)
                  (-group-by #'file-status))))
      (list file-state to-remove to-insert to-update no-change))))

(defun org-x-dag-read-files (files)
  (cl-flet
      ((append-results
        (acc filedata)
        (-let* (((&plist :path :group) filedata)
                ((acc-ids acc-filemaps acc-links) acc)
                ((ids links) (org-x-dag-get-file-nodes path group))
                (filemap (cons path (--map (plist-get :id it) ids))))
          `((,@ids ,@acc-ids)
            (,filemap ,@acc-filemaps)
            (,@(--map (cons group it) links) ,@acc-links)))))
    (-reduce-from #'append-results nil files)))

;; TODO what about all the nodes that don't need to be updated?
(defun org-x-dag-update-ht (to-remove to-insert ht)
  (--each to-remove
    (ht-remove ht it))
  (--each to-insert
    (ht-set ht (car it) (cdr it))))

(defun org-x-dag-update-dag (to-insert to-remove)
  (let* ((dag (org-x-dag->dag))
         (dag* (if (dag-is-empty-p dag) (dag-plist-to-dag to-insert)
                 (dag-edit-nodes to-remove to-insert dag))))
    (plist-put org-x-dag :dag dag*)))

;; (defun org-x-dag-id->parent-class (id parent)
;;   (-let* (((cfile cgroup) (org-x-dag-id->file-group id))
;;           ((pfile pgroup) (org-x-dag-id->file-group parent)))
;;     (cl-case cgroup
;;       ;; the only allowed links are local
;;       ((:lifetime :survival)
;;        (unless (equal cfile pfile)
;;          :ill-foreign))
;;       ;; should only link locally or a lifetime goal
;;       (:endpoint
;;        (unless (or (equal cfile pfile) (eq pgroup :lifetime))
;;          :ill-foreign))
;;       ;; should only link to an endpoint or lifetime goal
;;       (:quarterly
;;        (cond
;;         ((memq pgroup '(:endpoint :lifetime)) nil)
;;         ((equal pfile cfile) :ill-local)
;;         (t :ill-foreign)))
;;       ;; should only link to a quarterly plan
;;       (:weekly
;;        (cond
;;         ((eq pgroup :quarterly) nil)
;;         ((equal pfile cfile) :ill-local)
;;         (t :ill-foreign)))
;;       ;; should only link to a weekly plan or an action
;;       (:daily
;;        (cond
;;         ((memq pgroup '(nil :weekly)) nil)
;;         ((equal pfile cfile) :ill-local)
;;         (t :ill-foreign)))
;;       ;; actions can only be linked to goal files, and nothing else
;;       (t
;;        (cond
;;         ((memq pgroup '(:lifetime :endpoint :survival)) nil)
;;         ((equal pfile cfile) :ill-local)
;;         (t :ill-foreign))))))

;; ;; TODO this will also include broken links, which isn't totally wrong but these
;; ;; should be filtered out as including them is suboptimal (note: I figureed out
;; ;; they were here because the broken links dag code is wrong)
;; (defun org-x-dag-filter-links (relations)
;;   (cl-flet
;;       ((flatten-relation
;;         (rel)
;;         (-let (((c . ps) rel))
;;           (--map (list c it) ps))))
;;     (-let (((&alist :ill-foreign :ill-local)
;;             (->> (-mapcat #'flatten-relation relations)
;;                  (--group-by (apply #'org-x-dag-id->parent-class it)))))
;;       (list ill-foreign ill-local))))

(defun org-x-dag-id->illegal-parents (which id)
  (ht-get (plist-get org-x-dag which) id))

(defun org-x-dag-id->has-illegal-children-p (which id)
  (ht-find (lambda (_ v) (member id v)) (plist-get org-x-dag which)))

(defun org-x-dag-id->any-illegal-p (id)
  (or (org-x-dag-id->illegal-parents :illegal-foreign id)
      (org-x-dag-id->illegal-parents :illegal-local id)
      (org-x-dag-id->has-illegal-children-p :illegal-foreign id)
      (org-x-dag-id->has-illegal-children-p :illegal-local id)))

(defun org-x-dag-id->created-epoch (id)
  (-some->> (org-x-dag-id->node-property org-x-prop-created id)
    (org-2ft)))

(defun org-x-dag-id->created-in-past-p (id)
  (-when-let (e (org-x-dag-id->created-epoch id))
    (<= e (float-time))))

;; TODO there is a HUGE DIFFERENCE between a 'key' (the things in the hash table
;; the look things up) and a 'node' (which is a cons cell, the car of which is a
;; 'key' and the cdr of which is a 'relation'). These names suck, but the point
;; is we need to distinguish between them otherwise really strange things happen
(defun org-x-dag-update (file-state to-remove to-insert to-update)
  "Update the DAG given files to add and remove.

TO-REMOVE, TO-INSERT, and TO-UPDATE are lists of files to remove
from, add to, and update with the DAG. FILE-STATE is a nested
plist holding the files to be used in the DAG."
  (-let* (((&plist :id->meta
                   :file->ids
                   :illegal-foreign if
                   :illegal-local il
                   :id->status)
           org-x-dag)
          (files2rem (append to-update to-remove))
          (files2ins (append to-update to-insert))
          (ids2rem (org-x-dag-files->ids files2rem))
          ((ids2ins fms2ins links2ins) (org-x-dag-read-files files2ins)))
    ;; (org-x-dag-update-ht ids2rem meta2ins id->meta)
    ;; (org-x-dag-update-ht files2rem fms2ins file->ids)
    (org-x-dag-update-dag ids2ins ids2rem)
    (plist-put org-x-dag :files file-state)
    ;; TODO not sure where to put this
    (let ((adjlist (dag-get-adjacency-list (plist-get org-x-dag :dag))))
      (->> (org-x-dag-get-network-status adjlist links2ins)
           (plist-put org-x-dag :netstat)))))
    ;; update illegal links after updating the adjlist, since we need that to
    ;; figure out which links are illegal
    ;; (-let (((illegal-foreign illegal-local) (org-x-dag-filter-links links2ins)))
    ;;   (org-x-dag-update-ht files2rem illegal-foreign if)
    ;;   (org-x-dag-update-ht files2rem illegal-local il))
    ;; update node-level status after figuring out which are invalid via links
    ;; (let ((status2ins (->> (-map #'car ids2ins)
    ;;                        (--map (cons it (org-x-dag-id->0th-status it))))))
    ;;   (org-x-dag-update-ht ids2rem status2ins id->status))))
    

(defun org-x-dag-sync (&optional force)
  "Sync the DAG with files from `org-x-dag-get-files'.

If FORCE is non-nil, sync no matter what."
  (when force
    (setq org-x-dag-sync-state nil
          org-x-dag (org-x-dag-empty)))
  ;; TODO verify integrity somewhere in here
  (-let (((file-state to-remove to-insert to-update no-change)
          (org-x-dag-get-sync-state)))
    (org-x-dag-update file-state to-remove to-insert to-update)
    (->> (append to-update to-insert no-change)
         (--map (cons (plist-get it :path) (plist-get it :md5)))
         (setq org-x-dag-sync-state))
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

(defun org-x-dag-add-default-props (item id)
  (org-add-props item nil
    'x-id id
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
         (setq next (org-x-dag-datetime-shift next shift shifttype)
               pastp (org-x-dag-datetime< next sel-datetime)))
       next))
    ('restart
     ;; Next time is one repeater interval after now
     ;;
     ;; ASSUME cur needs to match the length of time
     (org-x-dag-datetime-shift sel-datetime shift shifttype))
    ('cumulate
     ;; Next time is one repeater interval after the base timestamp
     (org-x-dag-datetime-shift datetime shift shifttype))))

(defun org-x-dag-unfold-timestamp (cur datetime rep future-limit)
  "Return all timestamps associated with DATETIME.

If REP is nil, return a singleton list just containing DATETIME.
If REP is non-nil, return DATETIME and all repeaters up until
FUTURE-LIMIT in a list."
  ;; ASSUME pts and future-limit are both long or short timestamps
  (unless (org-x-dag-datetime< future-limit datetime)
    (pcase rep
      (`nil `(,datetime))
      (`(,value ,unit ,reptype)
       (->> (org-x-dag-repeater-get-next cur datetime value unit reptype)
            (--unfold (unless (org-x-dag-datetime< future-limit it)
                        (cons it (org-x-dag-datetime-shift it value unit))))
            (cons datetime))))))

(defun org-x-dag-get-scheduled-at (sel-date pts)
  (-let* (((&plist :datetime d :repeater r) pts)
          (islongp (org-ml-time-is-long d))
          (future-limit (if islongp `(,@sel-date 23 59) sel-date))
          (sel-datetime (if islongp (org-x-dag-date-at-current-time sel-date) sel-date)))
    (org-x-dag-unfold-timestamp sel-datetime d r future-limit)))

(defun org-x-dag-get-deadlines-at (sel-date pts)
  (-let* (((&plist :datetime d :repeater r :warning w) pts)
          (islongp (org-ml-time-is-long d))
          ((warn-shift warn-shifttype)
           (if w w
             (let ((f (if islongp 1440 1)))
               `(,(* f org-deadline-warning-days) submonth))))
          (sel-datetime (if islongp (org-x-dag-date-at-current-time sel-date) sel-date))
          (future-limit (org-x-dag-datetime-shift sel-datetime warn-shift warn-shifttype)))
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

(defun org-x-dag-id->agenda-timestamp (id)
  "Retrieve timestamp information of ID for sorting agenda views.
This is a rewrite of `org-agenda-entry-get-agenda-timestamp'
except it ignores inactive timestamps."
  (-let (((ts type)
          (cond ((org-em 'scheduled-up 'scheduled-down
                         org-agenda-sorting-strategy-selected)
                 `(,(org-x-dag-id->planning-timestamp :scheduled id) " scheduled"))
                ((org-em 'deadline-up 'deadline-down
                         org-agenda-sorting-strategy-selected)
                 `(,(org-x-dag-id->planning-timestamp :deadline id) " deadline"))
                ((org-em 'timestamp-up 'timestamp-down
                         org-agenda-sorting-strategy-selected)
                 `(,(or (org-x-dag-id->planning-timestamp :scheduled id)
                        (org-x-dag-id->planning-timestamp :deadline id))
                   ""))
	            (t
                 '(nil "")))))
    (cons (-some->> ts
            (org-ml-timestamp-get-start-time)
            (org-x-dag-date-to-absolute))
          type)))

(defun org-x-dag-id->marker (id &optional point)
  (let* ((f (org-x-dag-id->file id))
         (p (or point (org-x-dag-id->point id)))
         (b (or (get-file-buffer f) (find-file-noselect f))))
    (set-marker (make-marker) p b)))

(defun org-x-dag-format-tag-node (category tags id)
  ;; ASSUME I don't use subtree-level categories
  (-let* ((tags* (org-x-dag-prepare-tags tags))
          (todo-state (org-x-dag-id->todo id))
          ;; TODO the only reason this format thing is here is to satisfy
          ;; `org-agenda-format-item' (which I should probably just rewrite)
          (head (format "%s %s" todo-state (org-x-dag-id->title id)))
          (level (org-x-dag-id->formatted-level id))
          (marker (org-agenda-new-marker (org-x-dag-id->marker id)))
          ((ts . ts-type) (org-x-dag-id->agenda-timestamp id))
          (item (org-agenda-format-item "" head level category tags*))
          (priority (org-get-priority item)))
    (-> (org-x-dag-add-default-props item id)
        (org-add-props nil
            ;; face
            'face 'default
            'done-face 'org-agenda-done
            'undone-face 'default
            ;; marker
            'org-hd-marker marker
            'org-marker marker
            ;; headline stuff
            'todo-state todo-state
            'priority priority
            'ts-date ts
            ;; misc
            'type (concat "tagsmatch" ts-type)))))

(defun org-x-dag-format-item (id extra category tags time)
  (let* ((tags* (org-x-dag-prepare-tags tags))
         (level (org-x-dag-id->formatted-level id))
         (todo-state (org-x-dag-id->todo id))
         (head (format "%s %s" todo-state (org-x-dag-id->title id)))
         (time-str (-some->> time (apply #'format "%02i:%02i ")))
         (item (org-agenda-format-item extra head level category tags* time-str))
         ;; TODO why am I getting the priority after sending the headline
         ;; through some crazy formatting function?
         (priority (org-get-priority item)))
    (-> (org-x-dag-add-default-props item id)
        (org-add-props nil
            'todo-state todo-state
            'priority priority))))

(defun org-x-dag-planning-props (id face pos date ts-date type)
  (list
   ;; face
   'face (if (org-x-dag-id->is-done-p id) 'org-agenda-done face)
   'undone-face face
   'done-face 'org-agenda-done
   ;; marker
   'org-hd-marker (org-agenda-new-marker (org-x-dag-id->marker id))
   'org-marker (org-agenda-new-marker (org-x-dag-id->marker id pos))
   ;; headline stuff
   'date (org-x-dag-date-to-absolute date)
   'ts-date (org-x-dag-date-to-absolute ts-date)
   'type type))

(defun org-x-dag-format-scheduled-node (sel-date pos datetime category tags id)
  (-let* (((this-date this-time) (org-x-dag-datetime-split datetime))
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
  (-let* (((this-date this-time) (org-x-dag-datetime-split datetime))
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

(defun org-x-dag-id->planning-timestamp (which id)
  (-some->> (org-x-dag-id->metaprop id :planning)
    (org-ml-get-property which)))

(defun org-x-dag-id->planning-datetime (which id)
  (-some->> (org-x-dag-id->planning-timestamp which id)
    (org-ml-timestamp-get-start-time)))

(defun org-x-dag-id->planning-epoch (which id)
  (-some->> (org-x-dag-id->planning-datetime which id)
    (org-ml-time-to-unixtime)))

(defun org-x-dag-id->node-property (prop id)
  (alist-get prop (org-x-dag-id->metaprop id :props) nil nil #'equal))

(defun org-x-dag-id->node-property-equal-p (prop value id)
  (equal (org-x-dag-id->node-property prop id) value))

(defun org-x-dag-id->is-iterator-p (id)
  (org-x-dag-id->node-property-equal-p org-x-prop-parent-type
                                       org-x-prop-parent-type-iterator
                                       id))

;; (defun org-x-dag-is-created-p (want-time)
;;   (save-excursion
;;     (-when-let (ts (org-x-dag-get-local-property org-x-prop-created))
;;       (if want-time (org-2ft ts) t))))

;; (defun org-x-dag-headline-is-iterator-p ()
;;   (save-excursion
;;     (->> (org-x-dag-get-local-property org-x-prop-parent-type)
;;          (equal org-x-prop-parent-type-iterator))))

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
  (< (* 60 60 24 org-x-archive-delay) (- (float-time) epochtime)))

(defun org-x-dag-id->is-archivable-p (id)
  (-some->> (org-x-dag-id->planning-timestamp :closed id)
    (org-ml-timestamp-get-start-time)
    (org-ml-time-to-unixtime)
    (org-x-dag-time-is-archivable-p)))

;;; STATUS DETERMINATION

;; (defmacro org-x-dag-with-id (key &rest body)
;;   (declare (indent 1))
;;   `(progn
;;      (goto-char (org-x-dag-id->point ,key))
;;      ,@body))

;; (defmacro org-x-dag-with-id-in-file (id &rest body)
;;   (declare (indent 1))
;;   `(org-x-with-file (org-x-dag-id->file ,id)
;;      (org-x-dag-with-id ,id ,@body)))

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
           (-if-let (children (org-x-dag-id->buffer-children key))
               (let* ((tags (org-x-dag-id->tags ,parent-tags key))
                      (child-results (funcall ,callback key tags children))
                      ;; ASSUME the car of the results will be the toplevel
                      ;; key/status pair for this (sub)project
                      (top-status (plist-get (car child-results) :status))
                      (top-status* (if (member top-status ',allowed-codes)
                                       top-status
                                     (alist-get top-status ',trans-tbl))))
                 (cons top-status* child-results))
             (let ((it-kw (org-x-dag-id->todo key)))
               (-> ,task-form
                   (nth ',allowed-codes)
                   (list))))))
       (let* ((results (-map #'get-project-or-task-status ,keys))
              (status (->> (-map #'car results)
                           (org-x-dag-get-max-index ',allowed-codes))))
         (cons status (-mapcat #'cdr results))))))

(defun org-x-dag-headline-get-project-status (id tags children)
  ;; ASSUME children will always be at least 1 long
  (let ((keyword (org-x-dag-id->todo id)))
    (-let (((status . child-results)
            (cond
             ((org-x-dag-id->planning-timestamp :scheduled id)
              (list :scheduled-project))
             ((equal keyword org-x-kw-hold)
              ;; (list (if (org-x-headline-is-inert-p) :inert :held)))
              (list :held))
             ((member keyword org-x--project-invalid-todostates)
              (list :invalid-todostate))
             ((equal keyword org-x-kw-canc)
              (list (if (org-x-id->is-archivable-p id) :archivable :complete)))
             ((equal keyword org-x-kw-done)
              (org-x-dag-descend-into-project children tags
                ((:archivable)
                 (:complete)
                 (:done-incomplete :stuck :inert :held :wait :active
                                   :scheduled-project :invalid-todostate
                                   :undone-complete))
                (if (member it-kw org-x-done-keywords)
                    (if (org-x-dag-id->is-archivable-p id) 0 1)
                  2)
                #'org-x-dag-headline-get-project-status))
             ((equal keyword org-x-kw-todo)
              (org-x-dag-descend-into-project children tags
                ((:undone-complete :complete :archivable)
                 (:stuck :scheduled-project :invalid-todostate
                         :done-incomplete)
                 (:held)
                 (:wait)
                 ;; (:inert)
                 (:active))
                (cond
                 ;; ((and (not (member it-kw org-x-done-keywords))
                 ;;       (org-x-headline-is-inert-p))
                 ;;  4)
                 ((equal it-kw org-x-kw-todo)
                  (if (org-x-dag-id->planning-timestamp :scheduled id) 4 1))
                 ((equal it-kw org-x-kw-hold)
                  2)
                 ((equal it-kw org-x-kw-wait)
                  3)
                 ((equal it-kw org-x-kw-next)
                  4)
                 (t 0))
                #'org-x-dag-headline-get-project-status))
             (t (error "Invalid keyword detected: %s" keyword)))))
      (cons (list :key key :status status :tags tags) child-results))))

(defun org-x-dag-headline-get-iterator-project-status (id children)
  (let* ((kw (org-x-dag-id->todo id))
         (status
          (cond
           ((or (member kw org-x--project-invalid-todostates)
                (org-x-dag-id->planning-timestamp :scheduled id))
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
              (let ((ts (org-x-dag-id->planning-timestamp :scheduled id)))
                (cond
                 ((not ts) 0)
                 ((> org-x-iterator-active-future-offset (- ts (float-time))) 1)
                 (t 2)))
              #'org-x-dag-headline-get-iterator-project-status))
           (t (error "Invalid keyword detected: %s" kw)))))
    status))

(defun org-x-dag-headline-get-iterator-task-status (id)
  (if (org-x-dag-id->is-done-p id) :empt
    (-if-let (ts (or (org-x-dag-id->planning-timestamp :scheduled id)
                     (org-x-dag-id->planning-timestamp :deadline id)))
        (if (< org-x-iterator-active-future-offset (- ts (float-time)))
            :actv
          :empt)
      :unscheduled)))

(defun org-x-dag-headline-get-iterator-status (id)
  (cl-flet
      ((get-status
        (id)
        (-if-let (children (org-x-dag-id->buffer-children id))
            (->> children
                 (org-x-dag-headline-get-iterator-project-status id)
                 (car))
          (org-x-dag-headline-get-iterator-task-status id))))
    (->> (org-x-dag-id->buffer-children key)
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
             (org-x-with-file it-file
               (-when-let (keys ,pre-form*)
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
            (-> (org-x-dag-format-tag-node cat tags key)
                (org-add-props nil
                    'x-toplevelp (org-x-dag-id->is-toplevel-p key)
                    'x-status status
                    'x-priority priority)))))
       (format-key
        (cat key)
        (let ((tags (org-x-dag-id->tags nil key)))
          ;; TODO don't hardcode these things
          (unless (or (not (eq (cadr (org-x-dag-id->goal-status 'current key)) :planned))
                      (org-x-dag-id->is-iterator-p key))
            (-some->> (org-x-dag-id->buffer-children key)
              (org-x-dag-headline-get-project-status key tags)
              (--map (format-result cat it)))))))
    (org-x-dag-with-files (org-x-dag->action-files)
        (and (org-x-dag-id->is-toplevel-p it)
             (not (org-x-dag-id->is-done-p it)))
      (format-key it-category it))))

(defun org-x-dag--item-add-goal-ids (item ids)
  (if ids
      (--map (org-add-props (copy-seq item) nil 'x-goal-id it) ids)
    (list (org-add-props item nil 'x-goal-id nil))))

(defun org-x-dag-scan-projects-with-goals ()
  (cl-flet
      ((split-parent-goals
        (s)
        (let ((id (get-text-property 1 'x-id s)))
          (-if-let (goal-ids (org-x-dag-id->linked-parents id))
              (org-x-dag--item-add-goal-ids s goal-ids)))))
    (->> (org-x-dag-scan-projects)
         (--filter (org-x-dag-id->is-toplevel-p (get-text-property 1 'x-id it)))
         (-mapcat #'split-parent-goals))))

(defun org-x-dag-scan-iterators ()
  (cl-flet*
      ((format-result
        (tags cat key)
        (-let ((status (org-x-dag-headline-get-iterator-status key)))
          (-> (org-x-dag-format-tag-node cat tags key)
              (org-add-props nil
                  'x-status status)))))
    ;; TODO this will only scan toplevel iterators
    (org-x-dag-with-files (org-x-dag->action-files)
        (org-x-dag-id->is-toplevel-p it)
      (let ((tags (org-x-dag-id->tags nil it)))
        (when (eq (cadr (org-x-dag-id->goal-status 'current id)) :planned)
          (when (org-x-dag-id->is-iterator-p it)
            (list (format-result tags it-category it))))))))
  
(defun org-x-dag-get-task-nodes (pred id)
  (declare (indent 2))
  (cl-labels
      ((descend
        (children)
        (->> (-filter pred children)
             (--mapcat (-if-let (cs (org-x-dag-id->buffer-children it))
                           (descend cs)
                         (list it))))))
    (when (funcall pred id)
      (-some-> (org-x-dag-id->buffer-children id)
        (descend)))))

;; TODO this includes tasks underneath cancelled headlines
(defun org-x-dag-scan-tasks ()
  (cl-flet
      ((format-key
        (category is-standalone key)
        (let ((tags (org-x-dag-id->tags nil key)))
          (unless (or (member org-x-tag-incubated tags)
                      ;; (not (eq (cadr (org-x-dag-id->goal-status 'current key)) :planned))
                      (org-x-dag-id->planning-timestamp :scheduled key)
                      (org-x-dag-id->planning-timestamp :deadline key))
            (let* ((s (org-x-dag-id->task-status key))
                   (p (alist-get s org-x-headline-task-status-priorities)))
              (unless (= p -1)
                (-> (org-x-dag-format-tag-node category tags key)
                    (org-add-props nil
                        'x-is-standalone is-standalone
                        'x-status s))))))))
    (org-x-dag-with-files (org-x-get-action-files)
        (org-x-dag-id->is-toplevel-p it)
      (-if-let (project-tasks (org-x-dag-get-task-nodes
                                  (lambda (it) (not (member (org-x-dag-id->todo it)
                                                       (list org-x-kw-canc org-x-kw-hold))))
                                  it))
          (--map (format-key it-category nil it) project-tasks)
        (list (format-key it-category t it))))))

;; TODO wetter than Prince's dreams
(defun org-x-dag-scan-tasks-with-goals ()
  (cl-flet*
      ((classify-parent
        (id)
        (cond
         ((or (org-x-dag-id->is-goal-p :lifetime id)
              (org-x-dag-id->is-goal-p :endpoint id))
          :non-survival)
         ((org-x-dag-id->is-goal-p :survival id)
          :survival)
         (t
          :ignore)))
       (format-key
        (category is-standalone key)
        (-let (((goal-ids goal-status) (org-x-dag-id->goal-status 'current key)))
          (when (memq goal-status '(:planned :committed))
            (let* ((s (org-x-dag-id->task-status key))
                   (p (alist-get s org-x-headline-task-status-priorities))
                   (tags (org-x-dag-id->tags nil key)))
              (unless (= p -1)
                ;; ASSUME only ids with at least one valid goal will get this
                ;; far
                (-> (org-x-dag-format-tag-node category tags key)
                    (org-add-props nil
                        'x-is-standalone is-standalone
                        'x-status s)
                    (org-x-dag--item-add-goal-ids goal-ids))))))))
    (org-x-dag-with-files (org-x-get-action-files)
        (org-x-dag-id->is-toplevel-p it)
      (-if-let (project-tasks (org-x-dag-get-task-nodes
                                  (lambda (it) (not (member (org-x-dag-id->todo it)
                                                       (list org-x-kw-canc org-x-kw-hold))))
                                  it))
          (--mapcat (format-key it-category nil it) project-tasks)
        (format-key it-category t it)))))

(defun org-x-dag-scan-survival-tasks ()
  (cl-flet
      ((format-key
        (category is-standalone key)
        (-let (((goal-ids goal-status) (org-x-dag-id->goal-status 'current key)))
          (when (eq goal-status :survival)
            (let* ((s (org-x-dag->task-status key))
                   (p (alist-get s org-x-headline-task-status-priorities))
                   (tags (org-x-dag-id->tags nil key)))
              (unless (= p -1)
                (-> (org-x-dag-format-tag-node category tags key)
                    (org-add-props nil
                        'x-is-standalone is-standalone
                        'x-status s)
                    (org-x-dag--item-add-goal-ids goal-ids))))))))
    (org-x-dag-with-files (org-x-dag->action-files)
        (and (org-x-dag-id->is-toplevel-p it)
             (not (org-x-dag-id->is-iterator-p it)))
      (-if-let (project-tasks (org-x-dag-get-task-nodes
                                  (lambda (it) (not (member (org-x-dag-id->todo it)
                                                       (list org-x-kw-canc org-x-kw-hold))))
                                  it))
          (--mapcat (format-key it-category nil it) project-tasks)
        (format-key it-category t it)))))

(defun org-x-dag-scan-survival-projects ()
  (cl-flet*
      ((format-result
        (cat result)
        (-let* (((&plist :key :status :tags) result)
                (priority (alist-get status org-x-project-status-priorities)))
          (when (>= priority 0)
            (-let (((goal-ids goal-status) (org-x-dag-id->goal-status 'current key)))
              (when (eq goal-status :survival)
                (-> (org-x-dag-format-tag-node cat tags key)
                    (org-add-props nil
                        'x-toplevelp (org-x-dag-id->is-toplevel-p key)
                        'x-status status
                        'x-priority priority)
                    (org-x-dag--item-add-goal-ids goal-ids)))))))
       (format-key
        (cat key)
        (let ((tags (org-x-dag-id->tags nil key)))
          ;; TODO don't hardcode these things
          (-some->> (org-x-dag-id->buffer-children key)
            (org-x-dag-headline-get-project-status key tags)
            (--mapcat (format-result cat it))))))
    ;; TODO this is hella-inefficient, just get the child links from the
    ;; survival goal file and start from there
    (org-x-dag-with-files (org-x-dag->action-files)
        (and (org-x-dag-id->is-toplevel-p it)
             (not (org-x-dag-id->is-done-p it)))
      (format-key it-category it))))

;; (defun org-x-dag-scan-incubated ()
;;   (cl-flet
;;       ((format-key
;;         (category key)
;;         (let ((tags (org-x-dag-id->tags nil key)))
;;           (when (member org-x-tag-incubated tags)
;;             (org-x-dag-with-id key
;;               (let* ((sch (org-x-dag-headline-is-scheduled-p t))
;;                      (dead (org-x-dag-headline-is-deadlined-p t))
;;                      (is-project (org-x-dag-id->buffer-children key)))
;;                 (-> (org-x-dag-format-tag-node category tags key)
;;                     (org-add-props nil
;;                         'x-project-p is-project
;;                         'x-scheduled sch
;;                         'x-deadlined dead))))))))
;;     (org-x-dag-with-files (org-x-dag->action-files)
;;         (and (org-x-dag-id->is-toplevel-p it)
;;              (not (org-x-dag-id->is-done-p it)))
;;       (list (format-key it-category it)))))

(defun org-x-dag-scan-archived ()
  (cl-flet
      ((format-key
        (category key)
        (let ((tags (org-x-dag-id->tags nil key)))
          ;; TODO is this what I actually want?
          (when (memq (cadr (org-x-dag-id->goal-status 'current key)) '(:planned :committed))
            (-let (((is-archivable is-project)
                    (-if-let (children (org-x-dag-id->buffer-children key))
                        (-> (org-x-dag-headline-get-project-status key tags children)
                            (alist-get org-x-project-status-priorities)
                            (eq :archivable)
                            (list t))
                      (-> (org-x-dag-id->task-status id)
                          (alist-get org-x-headline-task-status-priorities)
                          (eq :archivable)
                          (list t)))))
              (when is-archivable
                (-> (org-x-dag-format-tag-node category tags key)
                    (org-add-props nil
                        'x-project-p is-project))))))))
    (org-x-dag-with-files (org-x-get-action-files)
        (org-x-dag-id->is-toplevel-p it)
      (if (org-x-dag->is-iterator-p it)
          (->> (org-x-dag-id->buffer-children it)
               (--map (format-key it-category it)))
        (list (format-key it-category it))))))

(defun org-x-dag--classify-goal-link (which which-goal id)
  (let ((f (org-x-dag-id->file id)))
    (cond
     ((member f (org-x-dag->action-files))
      :action)
     ((equal f (org-x-dag->goal-file which))
      :local)
     ((and which-child (equal f (org-x-dag->goal-file which-child)))
      :child-goal)
     ((equal f (org-x-dag->planning-file :quarterly))
      :plan)
     (t
      :other))))

(defun org-x-dag--add-goal-status (item which local-children action-children
                                        invalid-children &optional
                                        goal-parents invalid-parents)
  (org-add-props item nil
    'x-goal-status (list :type which
                         :local-children local-children
                         :action-children action-children
                         :invalid-children invalid-children
                         :goal-parents goal-parents
                         :invalid-parents invalid-parents)))

(defun org-x-dag-scan-toplevel-goals (which which-goal)
  (cl-flet
      ((format-id
        (category id)
        (-let* (((buffer linked) (org-x-dag-id->split-children-2 id))
                ((&alist :action :local :child-goal :plan :other)
                 (--group-by
                  (org-x-dag--classify-goal-link which which-child it)
                  linked))
                (tags (org-x-dag-id->tags nil id)))
          (-> (org-x-dag-format-tag-node category tags id)
              (org-x-dag--add-goal-status which
                                          (append buffer local)
                                          (append action child-goal)
                                          other)))))
    (org-x-dag-with-files (list (org-x-dag->goal-file which))
        nil
      (list (format-id it-category it)))))

(defun org-x-dag-scan-epgs ()
  (let ((parent-files `(,(org-x-dag->goal-file :lifetime))))
    (cl-flet
        ((format-id
          (category id)
          (-let* (((buffer-children linked-children)
                   (org-x-dag-id->split-children-2 id))
                  (linked-parents (org-x-dag-id->linked-parents id))
                  ((&alist :action :local :plan :other)
                   (--group-by (org-x-dag--classify-goal-link :endpoint it) linked-children))
                  ((goal-parents other-parents)
                   (--separate (member (org-x-dag-id->file it) parent-files)
                               linked-parents))
                  (tags (org-x-dag-id->tags nil id)))
            (-> (org-x-dag-format-tag-node category tags id)
                (org-x-dag--add-goal-status :endpoint
                                            (append buffer-children local)
                                            action
                                            other
                                            goal-parents
                                            other-parents)))))
      (org-x-dag-with-files (list (org-x-dag->goal-file :endpoint))
          nil
        (list (format-id it-category it))))))

(defun org-x-dag-scan-goals ()
  (append (org-x-dag-scan-toplevel-goals :lifetime :endpoint)
          (org-x-dag-scan-toplevel-goals :survival nil)
          (org-x-dag-scan-epgs)))

(defun org-x-dag-scan-errors ()
  (cl-flet
      ((format-id
        (category id)
        (-when-let (error-type
                    (if (org-x-dag-id->is-iterator-p id)
                        (unless (org-x-dag-id->node-property "ARCHIVE" id)
                          :missing-archive)
                      (-if-let (created (org-x-dag-id->node-property org-x-prop-created))
                          (when (<= (float-time) (org-2ft created))
                            :future-created)
                        :missing-created)))
          (-> (org-x-dag-format-tag-node category nil id)
              (org-add-props nil
                  'x-error error-type)))))
    (org-x-dag-with-files (org-x-dag->files)
        (not (org-x-dag-id->is-done-p it))
      (list (format-id it-category it)))))

(defun org-x-dag-scan-agenda (sel-date)
  (cl-flet*
      ((format-timestamps
        (todayp sel-date cat id pts get-datetimes-fun format-datetime-fun)
        (-when-let (datetimes (funcall get-datetimes-fun sel-date pts))
          (let ((tags (org-x-dag-id->tags nil id)))
            ;; TODO this will show all tasks regardless of if they have a
            ;; goal/plan or not
            (-let (((&plist :pos) pts)
                   (donep (org-x-dag-id->is-done-p id)))
              (--> datetimes
                   (--remove (and donep (not (org-x-dag-datetime= (-take 3 it) sel-date))) it)
                   (if (not todayp) (--remove (org-x-dag-datetime< (-take 3 it) sel-date) it) it)
                   (--map (funcall format-datetime-fun sel-date pos it cat tags id) it))))))
       (format-id
        (todayp cat id)
        (append
         (-when-let (dead (org-x-dag-id->planning-timestamp :deadline id))
           (format-timestamps todayp sel-date cat id dead
                              #'org-x-dag-get-deadlines-at
                              #'org-x-dag-format-deadline-node))
         (-when-let (sched(org-x-dag-id->planning-timestamp :scheduled id))
           (format-timestamps todayp sel-date cat id sched
                              #'org-x-dag-get-scheduled-at
                              #'org-x-dag-format-scheduled-node)))))
    (org-x-dag-with-files (org-x-dag->action-files)
        nil
      (let ((todayp (= (org-x-dag-date-to-absolute sel-date) (org-today))))
        (format-id todayp it-category it)))))

(defun org-x-dag-scan-quarterly-plan ()
  (let ((week-file (list (org-x-get-weekly-plan-file)))
        (current-quarter (->> (org-x-dag->current-date)
                              (org-x-dag-date-to-quarter))))
    (cl-flet
        ((format-id
          (id)
          (let ((alloc (-some->> (org-x-dag-id->node-property org-x-prop-allocate id)
                         (org-x-dag-allocation-fraction current-quarter)))
                (assignedp (org-x-dag-id->has-child-in-files-p id week-file))
                (bucket (org-x-dag-id->bucket nil id)))
            (-> (org-x-dag-format-tag-node "goal" (list bucket) id)
                (org-add-props nil
                    'x-assignedp assignedp
                    ;; override face
                    'face (if assignedp 'org-warning 'default)
                    'x-alloc (or alloc 0))))))
      (org-x-with-file (org-x-dag->planning-file :quarterly)
        (-map #'format-id (org-x-dag->qtp-ids 'current))))))

(defun org-x-dag-scan-weekly-plan ()
  (let ((daily-file (list (org-x-get-daily-plan-file))))
    (cl-flet
        ((format-id
          (id)
          ;; TODO this assigned thing needs to be limited in scope to the
          ;; the current ids of the time period in question
          (let* ((assignedp (org-x-dag-id->has-child-in-files-p id daily-file))
                 (day (-some->> (org-x-dag-id->tags nil id)
                        ;; TODO I guess this works...could be more precise
                        (--filter (s-matches-p "[A-Z]\\{3\\}" it))
                        (car)))
                 (daynum (car (rassoc day org-x-dag-weekly-tags))))
            (-> (org-x-dag-format-tag-node "goal" nil id)
                (org-add-props nil
                    'x-assignedp assignedp
                    'x-day-of-week (format "%d. %s" daynum day)
                    ;; override face
                    'face (if assignedp 'org-warning 'default))))))
      (org-x-with-file (org-x-dag->planning-file :weekly)
        (-map #'format-id (org-x-dag->wkp-ids 'current))))))

;; (cl-flet
;;     ((format-id
;;       (category id)
;;       (-> (org-x-dag-format-tag-node category nil id)
;;           (org-add-props nil))))
;;   (org-x-dag-with-files (list (org-x-qtp-get-file))
;;       nil
;;     (org-x-dag-with-id it
;;       (list (format-id it-category it))))))

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

(defun org-x-dag-build-parent-link-drawer (ids)
  (->> (-map #'org-x-dag-id->link-item ids)
       (apply #'org-ml-build-plain-list)
       (org-ml-build-drawer org-x-drwr-parent-links)))

(defun org-x-dag-section-get-parent-links (children)
  (cl-flet
      ((parse-item
        (item)
        (let ((first (car (org-ml-item-get-paragraph item))))
          (if (and (org-ml-is-type 'link first)
                   (equal (org-ml-get-property :type first) "id"))
              (org-ml-get-property :path first)
            (error "Invalid link node: %S" first)))))
    (-when-let (first (->> children
                           (--find (org-x--is-drawer-with-name org-x-drwr-parent-links it))
                           (org-ml-get-children)
                           (car)))
      (if (org-ml-is-type 'plain-list first)
          (->> (org-ml-get-children first)
               (-map #'parse-item))
        (error "Invalid parent link drawer")))))

(defun org-x-dag-section-set-parent-links (ids children)
  (-if-let (i (--find-index (org-x--is-drawer-with-name org-x-drwr-parent-links it)
                            children))
      (let ((d (nth i children)))
        (-if-let (pl (-some->> (-map #'org-x-dag-id->link-item ids)
                       (apply #'org-ml-build-plain-list)))
            (-replace-at i (org-ml-set-children (list pl) d) children)
          (-remove-at i children)))
    (if ids (cons (org-x-dag-build-parent-link-drawer ids) children) children)))

(defmacro org-x-dag-section-map-parent-links* (form children)
  (let ((c (make-symbol "--headline")))
    `(let* ((,c ,children)
            (it (org-x-dag-section-get-parent-links ,c)))
       (org-x-dag-section-set-parent-links ,form ,c))))

(defun org-x-dag-section-add-parent-link (id children)
  (org-x-dag-section-map-parent-links* (cons id it) children))

(defun org-x-dag-section-remove-parent-link (id children)
  (org-x-dag-section-map-parent-links*
   (--remove-first (equal it id) it)
   children))

(defun org-x-dag-headline-get-parent-links (headline)
  (->> headline
       (org-ml-headline-get-contents (org-x-logbook-config))
       (org-x-dag-section-get-parent-links)))

(defun org-x-dag-headline-set-parent-links (ids headline)
  (org-ml-headline-map-contents* (org-x-logbook-config)
    (org-x-dag-section-set-parent-links ids it)
    headline))

(defmacro org-x-dag-headline-map-parent-links* (form headline)
  (let ((h (make-symbol "--headline")))
    `(let* ((,h ,headline)
            (it (org-x-dag-headline-get-parent-links ,h)))
       (org-x-dag-headline-set-parent-links ,form ,h))))

;; TODO not DRY
(defun org-x-dag-headline-add-parent-link (id headline)
  (org-x-dag-headline-map-parent-links* (cons id it) headline))

(defun org-x-dag-headline-remove-parent-link (id headline)
  (org-x-dag-headline-map-parent-links*
   (--remove-first (equal it id) it)
   headline))

;;; ALLOCATION

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

;;; INTERACTIVE FUNCTIONS

(defun org-x-dag-add-id-to-this-headline (id)
  (org-ml-update-this-headline*
    (org-x-dag-headline-add-parent-link id it)))

(defun org-x-dag-read-id (ids cur-ids)
  (cl-flet
      ((make-cell
        (id type)
        (-let ((title (org-x-dag-id->title id))
               (presentp (not (eq type 'toadd)))
               (prefix (pcase type
                         (`present ?*)
                         (`noexist ?!)
                         (`toadd ?\s))))
          (list (format "%c %s" prefix title)
                :id id
                :title title
                :presentp presentp))))
    (let* ((present (--map (list it 'present) (-intersection cur-ids ids)))
           (noexist (--map (list it 'noexist) (-difference cur-ids ids)))
           (toadd (--map (list it 'toadd) (-difference ids cur-ids)))
           (mapper (->> (append present noexist toadd)
                        (--map (apply #'make-cell it))
                        (--sort (plist-get (cdr it) :presentp)))))
      (print (-map #'cdr mapper))
      (alist-get (completing-read "Node: " mapper) mapper nil nil #'equal))))

(defun org-x-dag-this-headline-choose-id (toplevel-allowed? legal-files msg ids)
  (cl-flet
      ((update-nodes
        (link-getter remover adder ids children)
        (-let* ((cur-ids (funcall link-getter children))
                ((&plist :id i :presentp p) (org-x-dag-read-id ids cur-ids)))
          (funcall (if p remover adder) i children))))
    (if (not (member (buffer-file-name) legal-files)) (message "Not in %s" msg)
      (if (org-before-first-heading-p)
          (if (not toplevel-allowed?) (message "Cannot set toplevel drawer.")
            (org-ml~update-this-section* nil
              ;; TODO org-ml shouldn't require this, just map the children
              ;; directly
              (org-ml-map-children*
                (update-nodes #'org-x-dag-section-get-parent-links
                              #'org-x-dag-section-remove-parent-link
                              #'org-x-dag-section-add-parent-link
                              ids it)
                it)))
        (org-ml~update-this-headline* nil
          (update-nodes #'org-x-dag-headline-get-parent-links
                        #'org-x-dag-headline-remove-parent-link
                        #'org-x-dag-headline-add-parent-link
                        ids it))))))

(defun org-x-dag-link-ltg-to-epg ()
  (interactive)
  (let ((ids (org-x-dag->ltg-ids))
        (legal (list (org-x-get-endpoint-goal-file))))
    (org-x-dag-this-headline-choose-id nil legal "endpoint goal file" ids)))

(defun org-x-dag-link-goal-to-qtp ()
  (interactive)
  (let ((ids (append (org-x-dag->ltg-ids) (org-x-dag->epg-ids)))
        (legal (list (org-x-qtp-get-file))))
    (org-x-dag-this-headline-choose-id nil legal "quarterly plan file" ids)))

(defun org-x-dag-link-action-to-goal ()
  (interactive)
  (let ((ids (append (org-x-dag->ltg-ids) (org-x-dag->epg-ids)))
        (legal (org-x-dag->action-files)))
    ;; TODO this won't work on the toplevel section
    (org-x-dag-this-headline-choose-id t legal "an action file" ids)))

;;; AGENDA VIEWS

(defun org-x-dag-agenda-run-series (name files cmds)
  (declare (indent 2))
  (catch 'exit
    (let ((org-agenda-buffer-name (format "*Agenda: %s*" name)))
      (org-agenda-run-series name `((,@cmds) ((org-agenda-files ',files)))))))

(defun org-x-dag-agenda-call (buffer-name header-name type match files settings)
  (declare (indent 5))
  (let* ((n (or header-name buffer-name))
         (s `((org-agenda-overriding-header ,n) ,@settings)))
    (org-x-dag-agenda-run-series buffer-name files `((,type ,match ,s)))))

;; TODO the tags in the far column are redundant
(defun org-x-dag-agenda-quarterly-plan ()
  (interactive)
  (let ((match ''org-x-dag-scan-quarterly-plan)
        (files (org-x-get-action-files))
        (header (->> (org-x-dag->current-date)
                     (org-x-dag-date-to-quarter)
                     (apply #'format "Quarterly Plan: %d Q%d"))))
    (org-x-dag-agenda-call "Quarterly Plan" nil #'org-x-dag-show-nodes match files
      `((org-agenda-todo-ignore-with-date t)
        (org-agenda-overriding-header ,header)
        (org-agenda-sorting-strategy '(user-defined-up category-keep))
        ;; TODO add allocation (somehow)
        (org-agenda-prefix-format '((tags . "  ")))
        (org-super-agenda-groups
         '((:auto-map
            (lambda (line)
              (let ((bucket (car (get-text-property 1 'tags line))))
                (--> (-map #'cdr org-x-life-categories)
                     (--find (equal (plist-get it :tag) bucket) it)
                     (plist-get it :desc)))))))))))

(defun org-x-dag-agenda-weekly-plan ()
  (interactive)
  (let* ((match ''org-x-dag-scan-weekly-plan)
         (files (org-x-get-action-files))
         (date (org-x-dag->current-date))
         (header (->> (org-x-dag-date-to-week-number date)
                      (format "Weekly Plan: %d W%02d" (car date)))))
    (org-x-dag-agenda-call "Weekly Plan" nil #'org-x-dag-show-nodes match files
      `((org-agenda-todo-ignore-with-date t)
        (org-agenda-overriding-header ,header)
        (org-agenda-sorting-strategy '(user-defined-up category-keep))
        (org-agenda-prefix-format '((tags . "  ")))
        (org-super-agenda-groups
         '((:auto-map
            (lambda (line)
              (get-text-property 1 'x-day-of-week line)))))))))

(defun org-x-dag-agenda-tasks-by-goal ()
  (interactive)
  (let ((match ''org-x-dag-scan-tasks-with-goals)
        (files (org-x-get-action-files)))
    (nd/org-agenda-call "Tasks by Goal" nil #'org-x-dag-show-nodes match files
      `((org-agenda-todo-ignore-with-date t)
        (org-agenda-sorting-strategy '(user-defined-up category-keep))
        (org-super-agenda-groups
         '((:auto-map
            (lambda (line)
              (-if-let (i (get-text-property 1 'x-goal-id line))
                  (->> (org-x-dag-id->title i)
                       (substring-no-properties))
                "0. Unlinked")))))))))

(defun org-x-dag-agenda-survival-tasks ()
  (interactive)
  (let ((match ''org-x-dag-scan-survival-tasks)
        (files (org-x-get-action-files)))
    (nd/org-agenda-call "Survival Tasks" nil #'org-x-dag-show-nodes match files
      `((org-agenda-todo-ignore-with-date t)
        (org-agenda-sorting-strategy '(user-defined-up category-keep))
        (org-super-agenda-groups
         '((:auto-map
            (lambda (line)
              (-if-let (i (get-text-property 1 'x-goal-id line))
                  (->> (org-x-dag-id->title i)
                       (substring-no-properties))
                "0. Unlinked")))))))))

;; TODO this is just toplevel projects (for now)
;; TODO wetter than Seattle
(defun org-x-dag-agenda-projects-by-goal ()
  (interactive)
  (let ((match ''org-x-dag-scan-projects-with-goals)
        (files (org-x-get-action-files)))
    (nd/org-agenda-call "Projects by Goal" nil #'org-x-dag-show-nodes match files
      `((org-agenda-todo-ignore-with-date t)
        (org-agenda-sorting-strategy '(user-defined-up category-keep))
        (org-super-agenda-groups
         '((:auto-map
            (lambda (line)
              (-if-let (i (get-text-property 1 'x-goal-id line))
                  (->> (org-x-dag-id->title i)
                       (substring-no-properties))
                "0. Unlinked")))))))))

;; TODO this is just toplevel projects (for now)
;; TODO wetter than Seattle
(defun org-x-dag-agenda-survival-projects ()
  (interactive)
  (let ((match ''org-x-dag-scan-survival-projects)
        (files (org-x-get-action-files)))
    (nd/org-agenda-call "Survival Projects" nil #'org-x-dag-show-nodes match files
      `((org-agenda-todo-ignore-with-date t)
        (org-agenda-sorting-strategy '(user-defined-up category-keep))
        (org-super-agenda-groups
         '((:auto-map
            (lambda (line)
              (-if-let (i (get-text-property 1 'x-goal-id line))
                  (->> (org-x-dag-id->title i)
                       (substring-no-properties))
                "0. Unlinked")))))))))

(defun org-x-dag-agenda-goals ()
  (interactive)
  (let ((match ''org-x-dag-scan-goals))
    (nd/org-agenda-call "Goals-0" nil #'org-x-dag-show-nodes match nil
      `((org-agenda-sorting-strategy '(user-defined-up category-keep))
        (org-super-agenda-groups
         '((:auto-map
            (lambda (line)
              (-let* (((&plist :type y
                               :local-children lc
                               :action-children ac
                               :invalid-children ic
                               :goal-parents gp
                               :invalid-parents ip)
                       (get-text-property 1 'x-goal-status line))
                      (type (cl-case y
                              (:endpoint "0. Endpoint")
                              (:lifetime "1. Lifetime")
                              (:survival "2. Survival")))
                      (subtext (cl-case y
                                 (:endpoint
                                  (cond
                                   (ip "Invalid parent links")
                                   ((not gp) "Missing toplevel goal")
                                   (ic "Invalid child links")
                                   ((and (not lc) (not ac) "Missing action"))
                                   ((and lc (not ac)) "Branch")))
                                 ((:lifetime :survival)
                                  (cond
                                   (ic "Invalid child links")
                                   ((and (not lc) (not ac) "Missing goal/action"))
                                   ((and lc (not ac)) "Branch"))))))
                (if subtext (format "%s (%s)" type subtext) type))))))))))

(provide 'org-x-dag)
;;; org-x-dag.el ends here
