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

(require 'org)
(require 'org-agenda)
(require 'org-ml)
(require 'dash)
(require 'dag)
(require 'either)
(require 'ht)

(require 'org-x-files)
(require 'org-x-const)

(eval-when-compile
  (require 'org-x-macs))

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

(defun org-x-dag-time-is-archivable-p (epochtime)
  (< (* 60 60 24 org-x-archive-delay) (- (float-time) epochtime)))

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

;; date <-> epoch

(defun org-x-dag-date-to-epoch (date)
  (float-time (encode-time `(0 0 0 ,@(reverse date) nil -1 nil))))

;; date <-> week

(defun org-x-dag-date-to-day-of-week (date)
  (->> (org-x-dag-date-to-gregorian date)
       (calendar-day-of-week)))

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

(defun org-x-dag-date-to-day-number (date)
  (->> (org-x-dag-date-to-gregorian date)
       (calendar-day-number)))

;; date <-> quarter

(defun org-x-dag-quarter-to-date (quarter)
  (-let (((y q) quarter))
    (list y (1+ (* (1- q) 3)) 1)))

(defun org-x-dag-date-to-quarter (date)
  (-let (((y m _) date))
    (list y (1+ (/ (1- m) 3)))))

(defun org-x-dag-date-to-quarter-start (date)
  (->> (org-x-dag-date-to-quarter date)
       (org-x-dag-quarter-to-date)))

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

;; tags <-> date

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

(defun org-x-dag-quarter-tags-to-date (tags)
  (-let (((y q) (reverse tags)))
    (org-x-dag-quarter-to-date (list (org-x-dag-tag-to-year y)
                                     (org-x-dag-tag-to-quarter q)))))

(defun org-x-dag-weekly-tags-to-date (tags)
  (-let (((y w) (reverse tags)))
    (org-x-dag-week-number-to-date (org-x-dag-tag-to-year y)
                                   (org-x-dag-tag-to-week w))))

(defun org-x-dag-daily-tags-to-date (tags)
  (-let (((y m d) (reverse tags)))
    (list (org-x-dag-tag-to-year y)
          (org-x-dag-tag-to-month m)
          (org-x-dag-tag-to-day d))))

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

;; allocation

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

;;; Org-DAG Pipeline

;; global state

(defun org-x-dag-create (d fis fls c s fs)
  (list :dag d
        :file->ids fis
        :file->links fls
        :current-date c
        :selected-date s
        :files fs))

(defun org-x-dag-empty ()
  (org-x-dag-create (dag-empty)
                    (ht-create #'equal)
                    (ht-create #'equal)
                    (org-x-dag-current-date)
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

;; buffer -> node tree

(defconst org-x-dag-parent-link-drawer-re
  (concat
   "^[ \t]*:X_PARENT_LINKS:[ \t]*\n"
   "\\(\\(?:^- .*?\n\\)+\\)"
   "[ \t]*:END:[ \t]*$"))

(defconst org-x-dag-prop-drawer-re
  (concat
   "^[\t ]*:PROPERTIES:[\t ]*\n"
   "\\(\\(.\\|\n\\)*?\\)"
   "[\t ]*:END:[\t ]*$"))

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

(defun org-x-dag-nreverse-tree (tree)
  (--each tree
    (setcdr it (org-x-dag-nreverse-tree (cdr it))))
  (nreverse tree))

(defun org-x-dag-get-buffer-nodes (file-meta kws target-props)
  (goto-char (point-min))
  (let* ((line-re (org-x-dag-line-regexp kws))
         (pps (--map (cons it (org-re-property it nil t)) target-props))
         (id-prop (org-re-property "ID" nil t))
         (next-pos (unless (= ?* (following-char))
                     (org-x-dag-next-headline)))
         ;; If not on a headline, check for a property drawer with links in it
         (this-file-links (when next-pos
                            (org-x-dag-get-parent-links nil next-pos)))
         ;; stack vars
         bare-stack node-stack bury-level
         ;; data vars
         this-id this-level this-todo this-tags this-links this-pblock
         this-parent this-buffer-parent this-point this-title this-node
         pbeg pend
         ;; return
         acc acc-links)
    (when next-pos
      (goto-char next-pos))
    (while (looking-at line-re)
      ;; Keep track of how 'deep' we are in a given org-tree using a stack. The
      ;; stack will have members like (LEVEL KEY TAGS) where LEVEL is the level
      ;; of the headline and KEY is the node key if it has a keyword, and TAGS
      ;; is a list of tags for the headlines. Only add a node to the accumulator
      ;; if it has a keyword and an ID property, and only include its parent
      ;; headline if the parent also has a keyword.
      (setq this-point (car (match-data t))
            this-level (length (match-string 1))
            this-todo (match-string-no-properties 2)
            this-title (match-string 3)
            this-tags (match-string-no-properties 4)
            next-pos (or (org-x-dag-next-headline) (point-max)))
      (unless (and bury-level (< bury-level this-level))
        ;; Adjust the stack so that the top headline is the parent of the
        ;; current headline
        (while (and node-stack (<= this-level (nth 0 (car node-stack))))
          (!cdr node-stack))
        (unless node-stack
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
                this-buffer-parent (nth 2 (car node-stack))
                this-links (or (org-x-dag-get-parent-links (nth 3 this-pblock)
                                                        next-pos)
                               (unless node-stack
                                 (nth 2 (car bare-stack)))))
          (when this-tags
            (setq this-tags (split-string this-tags ":")))
          (when (and (not node-stack) bare-stack)
            (setq this-tags (->> (car bare-stack)
                                 (nth 1)
                                 (append this-tags))))
          (->> (list :point this-point
                     :buffer-parent this-buffer-parent
                     :effort (when this-title
                               (get-text-property 0 'effort this-title))
                     :level this-level
                     :todo this-todo
                     :title (if this-title (substring-no-properties this-title) "")
                     :tags this-tags
                     :planning (org-x-dag-parse-this-planning (nth 0 this-pblock))
                     :props (org-x-dag-get-local-properties pbeg pend pps))
               (append file-meta)
               (list :id this-id
                     :parents (if this-buffer-parent
                                  `(,this-buffer-parent ,@this-links)
                                this-links)
                     :node-meta)
               (list)
               (setq this-node))
          (if node-stack
              (-> (setq this-parent (nth 1 (car node-stack)))
                  (setcdr (cons this-node (cdr this-parent))))
            (!cons this-node acc))
          (!cons (list this-level this-node this-id) node-stack)
          (when this-links
            (!cons (cons this-id this-links) acc-links)))
         ;; Underneath a node but not on a node, therefore we are buried
         (node-stack
          (setq bury-level this-level))
         ;; Anything else means we are on a bare headline above any nodes
         (t
          (setq bury-level nil
                node-stack nil)
          (when this-tags
            (setq this-tags (split-string this-tags ":")))
          (-> (list this-level
                    (append this-tags (or (nth 1 (car bare-stack)) org-file-tags))
                    (or (org-x-dag-get-parent-links nil next-pos)
                        (nth 2 (car bare-stack))
                        this-file-links))
              (!cons bare-stack)))))
      (goto-char next-pos))
    (list (org-x-dag-nreverse-tree acc) acc-links)))

;; buffer status

;; TODO need to check for created timestamps

;; [Status a] -> b -> (a -> a -> Status Bool) -> (a -> Bool) -> (a -> Status b)
;; -> Status b
(defmacro org-x-dag-bs-fold-children (bss default rank-form stop-form trans-form)
  (declare (indent 2))
  (let ((err (either :left "Child error")))
    `(if ,bss
         (-let (((x . xs) ,bss))
           (if (either-is-left-p x) ',err
             (let ((acc (cadr x)) r final it)
               (while (and (not final) xs)
                 (setq x (car xs))
                 (if (either-is-left-p x)
                     (setq final ',err)
                   (setq it (cadr x)
                         r ,rank-form)
                   (unless r
                     (error "You forgot the difference between Maybe and Either"))
                   (if (either-is-left-p r)
                       (setq final r)
                     (when (cadr r)
                       (setq acc (cadr x)))
                     (if ,stop-form
                         (setq final (either :right acc))
                       (!cdr xs)))))
               (when (not final)
                 (setq final (either :right acc)))
               (either>>= final ,trans-form))))
       (either :right ,default))))

(defun org-x-dag-bs-error-kw (type-name kw)
  (either :left (format "%ss cannot have keyword '%s" type-name kw)))

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
                 (either :right ,canc-bs-form)))
              ((equal it-todo org-x-kw-done)
               (let ((it-comptime (complete-time ,c nil)))
                 ,done-form))
              (t
               (->> (format "Closed %s must be marked CANC/DONE" ,type-name)
                    (either :left))))
           (cond
            ((member it-todo org-x-done-keywords)
             (either :left (format "DONE/CANC %s must be closed" ,type-name)))
            (t
             ,open-form)))))))

(defun org-x-dag-bs-action-project-inner (node-data child-bss)
  (cl-flet
      ((new-proj
        (status)
        (either :right `(:sp-proj ,@status)))
       (is-next
        (task-data)
        (-let (((&plist :todo :sched) task-data))
          (or sched (equal todo org-x-kw-next)))))
    ;; rankings
    ;; *-active > proj-wait > proj-held > (proj-stuck == iter-empty) > *-complete
    (org-x-dag-bs-action-with-closed node-data "projects"
      (if child-bss
          `(:sp-proj :proj-complete ,it-comptime)
        `(:sp-task :task-complete ,it-comptime))

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
               (`(,_ (:sp-task :task-complete ,_)) nil)
               (e (error "Unmatched pattern: %S" e)))
             (either :right))
        (pcase acc
          (`(:sp-proj :proj-complete ,_) nil)
          (`(:sp-iter :iter-complete ,_) nil)
          (`(:sp-task :task-complete ,_) nil)
          (_ t))
        (pcase it
          ((or `(:sp-proj :proj-complete ,_)
               `(:sp-iter :iter-complete ,_)
               `(:sp-task :task-complete ,_))
           (either :right `(:sp-proj :proj-complete ,it-comptime)))
          (_ (either :left "Completed projects cannot have active children"))))

      ;; undone form
      (-let* (((sched dead) (-some->> it-planning
                              (org-ml-get-properties '(:scheduled :deadline))))
              (task-default (->> (list :todo it-todo
                                       :sched sched
                                       :dead dead)
                                 (list :sp-task :task-active))))
        (cond
         ((and child-bss (equal it-todo org-x-kw-hold))
          (new-proj '(:proj-held)))
         ((and child-bss sched)
          (either :left "Projects cannot be scheduled"))
         ((equal it-todo org-x-kw-todo)
          (org-x-dag-bs-fold-children child-bss task-default
            (->> (pcase `(,acc ,it)
                   (`((:sp-task :task-active ,a) (:sp-task :task-active ,b))
                    (and (not (is-next a)) (is-next b)))

                   (`(,(or `(:sp-proj :proj-active)
                           `(:sp-proj :proj-wait)
                           `(:sp-proj :proj-held)
                           `(:sp-proj :proj-stuck)
                           `(:sp-iter :iter-active)
                           `(:sp-iter :iter-empty))
                      (:sp-task :task-active ,d))
                    (is-next d))

                   (`((:sp-task :task-active ,d)
                      ,(or `(:sp-proj :proj-active)
                           `(:sp-proj :proj-wait)
                           `(:sp-proj :proj-held)
                           `(:sp-proj :proj-stuck)
                           `(:sp-iter :iter-active)
                           `(:sp-iter :iter-empty)))
                    (not (is-next d)))

                   (`((:sp-iter :iter-active ,_) ,_) nil)
                   (`((:sp-proj :proj-active) ,_) nil)
                   (`(,_ (:sp-proj :proj-active)) t)
                   (`(,_ (:sp-iter :iter-active ,_)) t)

                   (`((:sp-proj :proj-wait) ,_) nil)
                   (`(,_ (:sp-proj :proj-wait)) t)

                   (`((:sp-proj :proj-held) ,_) nil)
                   (`(,_ (:sp-proj :proj-held)) t)

                   (`((:sp-proj :proj-stuck) ,_) nil)
                   (`((:sp-iter :iter-empty) ,_) nil)
                   (`((:sp-task :task-active ,_) ,_) nil)
                   (`(,_ (:sp-proj :proj-stuck)) t)
                   (`(,_ (:sp-iter :iter-empty)) t)
                   (`(,_ (:sp-task :task-active ,_)) t)

                   ;; any pair that makes it this far is completed in both, which means
                   ;; neither takes precedence, which means choose the left one
                   (`(,_ ,_) nil))
                 (either :right))

            ;; early stop
            (pcase acc
              (`(:sp-proj :proj-active) t)
              (`(:sp-iter :iter-active ,_) t)
              (`(:sp-task :task-active ,d) (is-next d))
              (_ nil))

            ;; child -> parent translation
            (pcase it
              ((or `(:sp-proj :proj-complete ,_)
                   `(:sp-task :task-complete ,_)
                   `(:sp-iter :iter-complete ,_))
               (either :left "Active projects must have at least one active child"))
              (`(:sp-proj . ,s) (new-proj s))
              (`(:sp-iter :iter-active ,_) (new-proj '(:proj-active)))
              (`(:sp-iter :iter-empty) (new-proj '(:proj-stuck)))
              (`(:sp-task :task-active ,d)
               (-let (((&plist :todo o :sched s) d))
                 (cond
                  ((equal o org-x-kw-todo) (->> (if s :proj-active
                                                  :proj-stuck)
                                                (list)
                                                (new-proj)))
                  ((equal o org-x-kw-next) (new-proj '(:proj-active)))
                  ((equal o org-x-kw-wait) (new-proj '(:proj-wait)))
                  ((equal o org-x-kw-hold) (new-proj '(:proj-hold)))
                  (t (org-x-dag-bs-error-kw "Task action" o))))))))
         (child-bss
          (org-x-dag-bs-error-kw "Project action" it-todo))
         (t
          (either :right task-default)))))))

(defun org-x-dag-node-data-is-iterator-p (node-data)
  (-let (((&plist :props) node-data))
    (-when-let (p (alist-get org-x-prop-parent-type props nil nil #'equal))
      (equal p org-x-prop-parent-type-iterator))))

;; TODO these next two could be made more efficient by cutting out the
;; earlystop form and returning error in the rank form (the trans form is
;; still needed in case there is only one child)
(defun org-x-dag-bs-action-subiter-complete-fold (child-bss comptime type-name comp-key)
  (declare (indent 2))
  (org-x-dag-bs-fold-children child-bss `(,comp-key ,comptime)
    (->> (pcase `(,acc ,it)
           (`((:si-complete ,_) (:si-complete ,_)) nil)
           (`((:si-complete ,_) ,_) t)
           (`(,_ (:si-complete ,_)) nil)
           (e (error "Unmatched pattern: %S" e)))
         (either :right))
    (pcase acc
      (`(:si-complete ,_) nil)
      (_ t))
    (pcase it
      (`(:si-complete ,_)
       (either :right `(,comp-key ,comptime)))
      (_
       (->> (format "Completed %s cannot have active children" type-name)
            (either :left))))))

(defun org-x-dag-bs-action-subiter-todo-fold (child-bss type-name active-key default)
  (org-x-dag-bs-fold-children child-bss default
    (pcase `(,acc ,it)
      (`((:si-active ,a) (:si-active ,b))
       (-let (((&plist :sched as :dead ad) a)
              ((&plist :sched bs :dead bd) b))
         (cond
          ((or (xor as bs) (xor ad bd))
           (->> "All sub-iters must have the same planning configuration"
                (either :left)))
          ((and as bs (xor (org-ml-time-is-long as) (org-ml-time-is-long bs)))
           (->> "Sub-iters must have scheduled timestamp with same length"
                (either :left)))
          ((and ad bd (xor (org-ml-time-is-long ad) (org-ml-time-is-long bd)))
           (->> "Sub-iters must have deadline timestamp with same length"
                (either :left)))
          ;; ASSUME this won't fail since the datetimes are assumed to be the
          ;; same length as per rules above
          ((and ad bd)
           (->> (org-x-dag-datetime< (org-ml-timestamp-get-start-time ad)
                                     (org-ml-timestamp-get-start-time bd))
                (either :right)))
          (t
           (->> (org-x-dag-datetime< (org-ml-timestamp-get-start-time as)
                                     (org-ml-timestamp-get-start-time bs))
                (either :right))))))
      (`((:si-active ,_) ,_) (either :right nil))
      (`(,_ (:si-active ,_)) (either :right t))
      (`(,_ ,_) (either :right nil)))
    (pcase acc
      (`(:si-active ,_) t)
      (_ nil))
    (pcase it
      (`(:si-complete ,_)
       (->> (format "Active %s must have at least one active child" type-name)
            (either :left)))
      (`(:si-active ,ts-data)
       (either :right `(,active-key ,ts-data))))))

(defun org-x-dag-node-is-iterator-p (node)
  (org-x-dag-node-data-is-iterator-p (plist-get node :node-meta)))

(defun org-x-dag-bs-action-subiter-inner (node-data child-bss)
  (org-x-dag-bs-action-with-closed node-data "sub-iterators"
    `(:si-complete ,it-comptime)
    (org-x-dag-bs-action-subiter-complete-fold child-bss it-comptime
      "sub-iterators" :si-complete)
    (-let (((sched dead) (-some->> it-planning
                           (org-ml-get-properties '(:scheduled :deadline)))))
      (cond
       ((and sched child-bss)
        (either :left "Sub-iterators with children cannot be scheduled"))
       ((and dead child-bss)
        (either :left "Sub-iterators with children cannot be deadlined"))
       ;; ((and (not child-bss) (not (xor sched dead)))
       ;;  (either :left "Sub-iterators must either be deadlined or scheduled"))
       ((org-x-dag-node-data-is-iterator-p node-data)
        (either :left "Iterators cannot be nested"))
       ((equal it-todo org-x-kw-todo)
        (org-x-dag-bs-action-subiter-todo-fold child-bss
          "sub-iterator" :si-active
          `(:si-active (:sched ,sched :dead ,dead))))
       (t
        (org-x-dag-bs-error-kw "Sub-iterator" it-todo))))))

(defun org-x-dag-bs-action-iter-inner (node-data child-bss)
  (org-x-dag-bs-action-with-closed node-data "iterators"
    `(:iter-complete ,it-comptime)
    (org-x-dag-bs-action-subiter-complete-fold child-bss it-comptime
      "iterators" :iter-complete)
    (cond
     ((and child-bss (-some->> it-planning (org-ml-get-property :scheduled)))
      (either :left "Iterators cannot be scheduled"))
     ;; TODO also check for timeshift and archive props
     ((equal it-todo org-x-kw-todo)
      (org-x-dag-bs-action-subiter-todo-fold child-bss
        "iterator" :iter-active
        '(:iter-empty)))
     (t
      (org-x-dag-bs-error-kw "Iterator" it-todo)))))

(defun org-x-dag-bs-epg-inner (node child-bss)
  (org-x-dag-bs-action-with-closed node "endpoint goal"
    `(:complete ,it-comptime)
    (org-x-dag-bs-fold-children child-bss `(:complete ,it-comptime)
      (->> (pcase `(,acc ,it)
             (`((:complete ,_) (:complete ,_)) nil)
             (`(,_ (:complete ,_)) nil)
             (`((:complete ,_) ,_) t))
           (either :right))
      (pcase acc
        (`(:complete ,_) nil)
        (_ t))
      (pcase it
        (`(:complete ,_)
         (either :right `(:complete ,it-comptime)))
        (_ (either :left "Completed EPGs cannot have active children"))))
    (cond
     ((-some->> it-planning (org-ml-get-property :scheduled))
      (either :left "EPGs cannot be scheduled"))
     ((equal it-todo org-x-kw-todo)
      (let ((dead (-some->> it-planning (org-ml-get-property :deadline))))
        (org-x-dag-bs-fold-children child-bss `(:active ,dead)
          (->> (pcase `(,acc ,it)
                 (`((:active ,_) (:active ,_)) nil)
                 (`(,_ (:active ,_)) t)
                 (`((:active ,_) ,_) nil))
               (either :right))
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
                 (either :right `(:active ,dead)))
                ((not dead)
                 (either :right `(:active ,c-dead)))
                (t
                 (either :left "Child deadlines must be before parent deadlines")))))
            (_
             (either :left "Active EPGs must have at least one active child"))))))
     (t
      (org-x-dag-bs-error-kw "Endpoint goal" it-todo)))))

(defun org-x-dag-bs-with-treetop-error (tree)
  (declare (indent 3))
  (-let* (((node . children) tree)
          ((&plist :id i :parents ps :node-meta m) node)
          (this (->> (either :left "Children not allowed")
                     (org-x-dag-node i ps m))))
    (cons this (--mapcat (org-x-dag-bs-with-treetop-error it) children))))

(defun org-x-dag-bs-with-treetop (tree node-fun)
  (declare (indent 3))
  (-let* ((((&plist :id i :parents ps :node-meta m) . children) tree)
          (bs (if children (either :left "Children not allowed")
                (funcall node-fun m)))
          (top (org-x-dag-node i ps m bs)))
    (cons top (--mapcat (org-x-dag-bs-with-treetop-error it) children))))

(defun org-x-dag-node (id parents node-meta bs)
  (list :id id
        :parents parents
        :node-meta (list :hl-meta node-meta
                         :buffer-status bs)))

(defmacro org-x-dag-node-fmap (node form)
  (declare (indent 1))
  ;; TODO not efficient (may or may not matter)
  `(-let (((&plist :id i
                   :parents ps
                   :node-meta (&plist :hl-meta h
                                      :buffer-status it))
           ,node))
     (org-x-dag-node i ps h ,form)))

(defun org-x-dag-bs-with-children (tree child-fun node-fun concat-fun)
  (declare (indent 3))
  ;; TODO this is super inefficient, make a plist mapper function
  (-let* (((node . children) tree)
          ((&plist :id i :parents ps :node-meta m) node)
          ((shallow rest) (->> (--map (funcall child-fun it) children)
                               (apply #'-zip-lists))))
    (list (->> shallow
               (--map (plist-get (plist-get it :node-meta) :buffer-status))
               (funcall node-fun m)
               (org-x-dag-node i ps m))
          (funcall concat-fun shallow rest))))

;; Tree a -> (Tree a -> (b, [d])) -> (a -> [b] -> c) -> (c, [d])
(defun org-x-dag-bs-with-children-1 (tree child-fun node-fun)
  (org-x-dag-bs-with-children tree child-fun node-fun
    (lambda (shallow deep)
      (append shallow (-flatten-n 1 deep)))))

;; Tree a -> (Tree a -> (b, ([d], [e]))) -> (a -> [b] -> c) -> (c, ([d], [e]))
(defun org-x-dag-bs-with-children-2 (tree child-fun node-fun)
  (org-x-dag-bs-with-children tree child-fun node-fun
    (lambda (shallow deep)
      (--reduce-from (-let (((a b) acc)
                            ((as bs) it))
                       `((,@as ,@a) (,@bs ,@b)))
                     `(,shallow nil)
                     deep))))

(defun org-x-dag-bs-action-subiter (tree)
  (org-x-dag-bs-with-children-1
   tree
   #'org-x-dag-bs-action-subiter
   #'org-x-dag-bs-action-subiter-inner))

(defun org-x-dag-bs-action-iter (tree)
  (org-x-dag-bs-with-children-1
   tree
   #'org-x-dag-bs-action-subiter
   (lambda (node-data child-bss)
     (either<$> (org-x-dag-bs-action-iter-inner node-data child-bss)
       (cons :sp-iter it)))))

(defun org-x-dag-bs-action-project (tree)
  (if (org-x-dag-node-is-iterator-p (car tree))
      (-let (((iter subiters) (org-x-dag-bs-action-iter tree)))
        `(,iter (nil ,subiters)))
    (org-x-dag-bs-with-children-2
     tree
     #'org-x-dag-bs-action-project
     #'org-x-dag-bs-action-project-inner)))

(defun org-x-dag-bs-action (node-tree)
  (cl-flet
      ((lift-subiter
        (node)
        (org-x-dag-node-fmap node
          (either<$> it (cons :sp-subiter it)))))
    (-let (((p (ps is)) (org-x-dag-bs-action-project node-tree)))
      `(,p ,@ps ,@(-map #'lift-subiter is)))))

(defun org-x-dag-bs-epg-outer (tree)
  (org-x-dag-bs-with-children-1
   tree
   #'org-x-dag-bs-epg-outer
   #'org-x-dag-bs-epg-inner))

(defun org-x-dag-bs-prefix (key nodes)
  (--map (org-x-dag-node-fmap it (either<$> it `(,key ,@it))) nodes))

(defun org-x-dag-bs-epg (tree)
  (-let (((n ns) (org-x-dag-bs-epg-outer tree)))
    (org-x-dag-bs-prefix :endpoint `(,n ,@ns))))

(defun org-x-dag-bs-toplevel-goal-inner (type-name node-data child-bss)
  (-let (((&plist :planning :todo) node-data))
    (cond
     (planning
      (either :left (format "%ss cannot have planning elements" type-name)))
     ((either-lefts child-bss)
      (either :left "Child error"))
     ((equal todo org-x-kw-todo)
      (either :right '(:active)))
     (t
      (org-x-dag-bs-error-kw type-name todo)))))

(defun org-x-dag-bs-toplevel-goal-outer (type-name tree)
  (org-x-dag-bs-with-children-1
   tree
   (lambda (tree)
     (org-x-dag-bs-toplevel-goal-outer type-name tree))
   (lambda (node-data child-bss)
     (org-x-dag-bs-toplevel-goal-inner type-name node-data child-bss))))

(defun org-x-dag-bs-toplevel-goal (type-name type-key tree)
  (-let (((n ns) (org-x-dag-bs-toplevel-goal-outer type-name tree)))
    (org-x-dag-bs-prefix type-key `(,n ,@ns))))

(defun org-x-dag-bs-ltg (tree)
  (org-x-dag-bs-toplevel-goal "LTG" :lifetime tree))

(defun org-x-dag-bs-svg (tree)
  (org-x-dag-bs-toplevel-goal "SVG" :survival tree))

(defun org-x-dag-bs-qtp-inner (node-data)
  (org-x-dag-bs-action-with-closed node-data "quarterly plan"
    `(:complete ,it-comptime)
    (either :right `(:complete ,it-comptime))
    (cond
     ((-some->> it-planning (org-ml-get-properties :scheduled))
      (either :left "QTPs cannot be scheduled"))
     ((equal it-todo org-x-kw-todo)
      (-if-let (dead (-some->> it-planning (org-ml-get-properties :deadline)))
          (-let* (((&plist :tags) node-data)
                  (tag-dt (org-x-dag-quarter-tags-to-date tags))
                  (dead-dt (->> (org-ml-timestamp-get-start-time dead)
                                (org-x-dag-datetime-split)
                                (car))))
            (if (org-x-dag-datetime< tag-dt dead-dt)
                (either :right `(:active ,dead))
              (->> "QTP deadlines must be due after the quarter starts"
                   (either :left))))
        (either :right '(:active nil))))
     (t
      (org-x-dag-bs-error-kw "QTP" it-todo)))))

(defun org-x-dag-bs-wkp-inner (node-data)
  (org-x-dag-bs-action-with-closed node-data "weekly plan"
    `(:complete ,it-comptime)
    (either :right `(:complete ,it-comptime))
    (cond
     ((-some->> it-planning (org-ml-get-properties :scheduled))
      (either :left "WKPs cannot be scheduled"))
     ((-some->> it-planning (org-ml-get-properties :deadline))
      (either :left "WKPs cannot be deadlined"))
     ((equal it-todo org-x-kw-todo)
      (either :right `(:active)))
     (t
      (org-x-dag-bs-error-kw "WKP" it-todo)))))

(defun org-x-dag-bs-dlp-inner (node-data)
  (org-x-dag-bs-action-with-closed node-data "daily metablock"
    `(:complete ,it-comptime)
    (either :right `(:complete ,it-comptime))
    (cond
     ((-some->> it-planning (org-ml-get-property :deadline))
      (either :left "Daily metablocks cannot be deadlined"))
     ((equal it-todo org-x-kw-todo)
      (-if-let (sched (-some->> it-planning
                        (org-ml-get-property :scheduled)))
          (-let (((sched-date sched-time) (->> sched
                                               (org-ml-timestamp-get-start-time)
                                               (org-x-dag-datetime-split))))
            (if (not sched-time)
                (either :left "Daily metablocks must have scheduled time")
              (-let* (((&plist :tags) node-data)
                      (tag-date (org-x-dag-daily-tags-to-date tags)))
                (if (org-x-dag-datetime= tag-date sched-date)
                    (either :right `(:active (:sched ,sched)))
                  (either :left "Daily metablocks must be scheduled within their date")))))
        (either :left "Daily metablocks must be scheduled")))
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

(defmacro org-x-dag-each-links (links &rest body)
  (declare (indent 1))
  `(let (it it-targets)
     (while ,links
       (setq it (car (car ,links))
             it-targets (cdr (car ,links)))
       ,@body
       (!cdr ,links))))

(defun org-x-dag-bs-error-links (msg links)
  (->> (s-join ", " links)
       (format "%s: %s" msg)
       (either :left)))

(defun org-x-dag-ns-toplevel (tbl links ns)
  (let ((h (alist-get tbl ns)))
    (org-x-dag-each-links links
      (ht-set h it (org-x-dag-bs-error-links "Invalid links" it-targets)))
    ns))

(defun org-x-dag-ns-ltg (links ns)
  (org-x-dag-ns-toplevel :lifetime links ns))

(defun org-x-dag-ns-svg (links ns)
  (org-x-dag-ns-toplevel :survival links ns))

(defun org-x-dag-ht-add-links (id htbl key targets)
  (let (r)
    (--each targets
      (->> (if (setq r (ht-get htbl it))
               (either<$> r
                 (org-x-dag-plist-cons it key id))
             (either :right `(,key (,id))))
           (ht-set htbl it)))))

(defun org-x-dag-ns-with-valid (ns adjlist cur-key links keypairs valid-fun)
  (declare (indent 4))
  (cl-flet*
      ((key-group
        (keys id)
        (let ((g (org-x-dag-id-link-group adjlist id)))
          (if (member g keys) g :invalid)))
       (parent-group
        (h permitleafp adjlist id)
        (cond
         ((either-is-left-p (ht-get h id))
          :error)
         ((and (not permitleafp) (org-x-dag-get-children adjlist id))
          :non-leaf)
         (t :valid)))
       (reduce-valid
        (grouped-targets acc keypair)
        (-let* (((key . permitleafp) keypair)
                ((acc-keyed acc-error acc-non-leaf) acc)
                (h (alist-get key ns))
                ((&alist :valid v :error e :non-leaf n)
                 (->> (alist-get key grouped-targets)
                      (--group-by (parent-group h permitleafp adjlist it)))))
          `(((,key ,@v) ,@acc-keyed)
            (,@e ,@acc-error)
            (,@n ,@acc-non-leaf)))))
    (org-x-dag-each-links links
      (let* ((keys (-map #'car keypairs))
             (grouped (--group-by (key-group keys it) it-targets))
             (cur-h (alist-get cur-key ns)))
        (-if-let (invalid (alist-get :invalid grouped))
            (ht-set cur-h it (org-x-dag-bs-error-links "Invalid links" invalid))
          (-let (((valid err non-leaf)
                  (--reduce-from (reduce-valid grouped acc it) nil keypairs)))
            (cond
             (err
              (->> (org-x-dag-bs-error-links "Linked to invalid links" err)
                   (ht-set cur-h it)))
             (non-leaf
              (->> (org-x-dag-bs-error-links "Linked to non-leaf nodes" non-leaf)
                   (ht-set cur-h it)))
             (t
              (funcall valid-fun it cur-h valid)))))))))

;; TODO this needs to eventually propagate deadlines; I want to be able to
;; link epgs to other epgs, which means I won't be able to check deadline
;; fidelity without links being established (which they are here)
(defun org-x-dag-ns-epg (adjlist links ns)
  (-let (((&alist :lifetime ht-l) ns))
    (org-x-dag-ns-with-valid ns adjlist :endpoint links
      '((:lifetime))
      (lambda (id this-h res)
        (-let (((&alist :lifetime l) res))
          (ht-set this-h id (either :right `(:committed ,l)))
          (org-x-dag-ht-add-links id ht-l :fulfilled l))))
    ns))

(defun org-x-dag-ht-get-maybe (htbl id key)
  (-when-let (x (ht-get htbl id))
    (either-from* x nil (plist-get it key))))

(defun org-x-dag-ns-qtp (adjlist links ns)
  (-let (((&alist :lifetime ht-l :endpoint ht-e) ns))
    (org-x-dag-ns-with-valid ns adjlist :quarterly links
      '((:lifetime) (:endpoint))
      (lambda (id this-h res)
        (-let (((&alist :lifetime l :endpoint e) res))
          (ht-set this-h id (either :right `(:committed (,@e ,@l))))
          (->> (--mapcat (org-x-dag-ht-get-maybe ht-e it :committed) e)
               (org-x-dag-ht-add-links id ht-l :planned))
          (org-x-dag-ht-add-links id ht-e :planned e)
          (org-x-dag-ht-add-links id ht-l :planned l))))
    ns))

(defun org-x-dag-ns-wkp (adjlist links ns)
  (-let (((&alist :quarterly ht-q) ns))
    (org-x-dag-ns-with-valid ns adjlist :weekly links
      '((:quarterly))
      (lambda (id this-h res)
        (-let (((&alist :quarterly q) res))
          (ht-set this-h id (either :right `(:committed ,q)))
          (org-x-dag-ht-add-links id ht-q :planned q))))
    ns))

(defun org-x-dag-ns-action (adjlist links ns)
  (-let (((&alist :endpoint ht-e :lifetime ht-l :survival ht-s) ns))
    (org-x-dag-ns-with-valid ns adjlist :action links
      '((:survival) (:endpoint) (:lifetime))
      (lambda (id this-h res)
        (-let* (((&alist :survival s :endpoint e :lifetime l) res)
                (this-ns
                 (cond
                  ((and s (or e l))
                   (->> "Action has both survival and endpoint/lifetime links"
                        (either :left)))
                  (s
                   (either :right `(:committed ,s :survivalp t)))
                  (t
                   (either :right `(:committed (,@e ,@l) :survivalp nil))))))
          (ht-set this-h id this-ns)
          (org-x-dag-ht-add-links id ht-l :fulfilled l)
          (org-x-dag-ht-add-links id ht-s :fulfilled s)
          (org-x-dag-ht-add-links id ht-e :fulfilled e)
          (->> (--mapcat (org-x-dag-ht-get-maybe ht-e it :committed) e)
               (org-x-dag-ht-add-links id ht-l :fulfilled)))))
    ns))

(defun org-x-dag-ns-dlp (adjlist links ns)
  (cl-flet
      ((get-committed
        (htbl ids)
        (--mapcat (org-x-dag-ht-get-maybe htbl it :committed) ids)))
    (-let (((&alist :action ht-a :quarterly ht-q :weekly ht-w) ns))
      (org-x-dag-ns-with-valid ns adjlist :daily links
        '((:action t) (:weekly))
        (lambda (id this-h res)
          (-let (((&alist :action a :weekly w) res))
            (let ((qgoals (->> (get-committed ht-w w)
                               (get-committed ht-q)))
                  (agoals (get-committed ht-a a)))
              ;; TODO this check doesn't matter for survival goals since
              ;; those won't be on the quarterly plan
              ;; (-if-let (gs (-intersection qgoals agoals))
                  (progn
                    (->> (list :scheduled w
                               ;; :committed (-uniq gs)
                               :committed (-uniq (append qgoals agoals))
                               :active a)
                         (either :right)
                         (ht-set this-h id))
                    ;; TODO add the goals to their goal links? (this might be
                    ;; useful later when displaying)
                    (org-x-dag-ht-add-links id ht-w :planned w)
                    (org-x-dag-ht-add-links id ht-a :planned a))))))
                ;; (->> (either :left "Non overlapping goals")
                ;;      (ht-set this-h id)))))))
      ns)))

(defun org-x-dag-get-children (adjlist id)
  (->> (plist-get (ht-get adjlist id) :children)
       (--filter (-> (ht-get adjlist it)
                     (plist-get :node-meta)
                     (plist-get :hl-meta)
                     (plist-get :buffer-parent)
                     (equal id)))))

(defun org-x-dag-ht-map-down (adjlist h-key ns get-fun set-fun def-fun)
  (declare (indent 3))
  (cl-labels
      ((propagate
        (adjlist htbl id to-set)
        (->> (-if-let (node (ht-get htbl id))
                 (either<$> node (funcall set-fun it to-set))
               (either :right (funcall def-fun to-set)))
             (ht-set htbl id))
        (--each (org-x-dag-get-children adjlist id)
          (propagate adjlist htbl it to-set))))
    (let ((h (alist-get h-key ns)))
      (-each (ht-keys h)
        (lambda (id)
          (-when-let (xs (funcall get-fun h id))
            (--each (org-x-dag-get-children adjlist id)
              (propagate adjlist h it xs)))))
      ns)))

(defun org-x-dag-ht-propagate-down (adjlist h-key s-key ns)
  (org-x-dag-ht-map-down adjlist h-key ns
    (lambda (h id)
      (org-x-dag-ht-get-maybe h id s-key))
    (lambda (plist to-set)
      (org-x-dag-plist-map (-copy plist) s-key
        (lambda (x) (append x to-set))))
    (lambda (to-set)
      (list s-key (-copy to-set)))))

(defun org-x-dag-ht-propagate-up (adjlist h-key s-key ns)
  (cl-labels
      ((propagate
        (htbl id)
        (-let* ((cs (org-x-dag-get-children adjlist id))
                (rs (--map (propagate htbl it) cs))
                ;; TODO there isn't a better way to do this? (seems like I'm
                ;; accessing either/maybe types too many times)
                ((n* rs*) (-if-let (n (ht-get htbl id))
                              (either-from n
                                (lambda ()
                                  `(,n ,rs))
                                (lambda (it)
                                  (let ((p (org-x-dag-plist-map it s-key
                                             (lambda (x) (append x rs)))))
                                    `(,(either :right p) ,(plist-get s-key p)))))
                            (list (either :right `(,s-key ,rs)) rs))))
          (ht-set htbl id n*)
          rs*)))
    (let ((h (alist-get h-key ns)))
      (--each (ht-keys h)
        (propagate h it )))
    ns))

(defun org-x-dag-get-network-status (adjlist links)
  (-let ((ns (->> (list :action
                        :endpoint
                        :lifetime
                        :survival
                        :quarterly
                        :weekly
                        :daily)
                  (--map (cons it (ht-create #'equal)))))
         ((&plist :action a
                  :endpoint e
                  :lifetime l
                  :survival s
                  :quarterly q
                  :weekly w
                  :daily d)
          (--reduce-from (-let* (((group . links) it)
                                 (acc-links (plist-get acc group)))
                           (plist-put acc group (append acc-links links)))
                         nil
                         links)))
    ;; add all links to the network status object (ew side effects)
    (org-x-dag-ns-ltg l ns)
    (org-x-dag-ns-svg s ns)
    (org-x-dag-ns-epg adjlist e ns)
    (org-x-dag-ns-qtp adjlist q ns)
    (org-x-dag-ns-wkp adjlist w ns)
    (org-x-dag-ns-action adjlist a ns)
    (org-x-dag-ns-dlp adjlist d ns)
    ;; propagate network statuses across each buffer tree as needed
    (org-x-dag-ht-propagate-down adjlist :action :planned ns)
    (org-x-dag-ht-map-down adjlist :action ns
      (lambda (h id)
        (either-from* (ht-get h id)
          nil
          (-when-let (committed (plist-get it :committed))
            `(,committed ,(plist-get it :survivalp)))))
      (lambda (plist to-set)
        (-let (((committed survivalp) to-set))
          ;; copy is needed here for some reason, otherwise other parts of the
          ;; hash table are affected
          (-> (-copy plist)
              (plist-put :survivalp survivalp)
              (org-x-dag-plist-map :committed
                  (lambda (x) (append x committed))))))
      (lambda (to-set)
        (-let (((committed survivalp) to-set))
          `(:committed ,committed :survivalp ,survivalp))))
    (org-x-dag-ht-propagate-down adjlist :endpoint :committed ns)
    (org-x-dag-ht-propagate-up adjlist :lifetime :fulfilled ns)
    (org-x-dag-ht-propagate-up adjlist :lifetime :planned ns)
    (org-x-dag-ht-propagate-up adjlist :survival :fulfilled ns)
    (org-x-dag-ht-propagate-up adjlist :survival :planned ns)))

;; global pipeline control

(defun org-x-dag-get-md5 (path)
  "Get the md5 checksum of PATH."
  (org-x-with-file path (buffer-hash)))

(defun org-x-dag-group-code (group)
  (pcase group
    (:lifetime "LTG")
    (:survival "SVG")
    (:endpoint "EPG")
    (:action "ACT")
    (:quarterly "QTP")
    (:weekly "WKP")
    (:daily "DLP")
    (_ "???")))

(defun org-x-dag-read-file-paths ()
  (list :goal-files (list :lifetime (org-x-get-lifetime-goal-file)
                          :endpoint (org-x-get-endpoint-goal-file)
                          :survival (org-x-get-survival-goal-file))
        :plan-files (list :daily (org-x-get-daily-plan-file)
                          :weekly (org-x-get-weekly-plan-file)
                          :quarterly (org-x-qtp-get-file))
        :action-files (append (org-x-get-action-files)
                              (org-x-get-incubator-files))))

(defun org-x-dag-flatten-file-state (state)
  (cl-flet
      ((flat-flip
        (plist)
        (->> (-partition-all 2 plist)
             (--map (cons (cadr it) (car it))))))
  (-let (((&plist :goal-files g :plan-files p :action-files a) state))
    (append (flat-flip g) (flat-flip p) (--map (cons it :action) a)))))

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
        (-let (((path . group) file-pair))
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
    `(,(-mapcat bs-fun nodes) ,links)))

(defun org-x-dag-read-files (files)
  (cl-flet
      ((append-results
        (acc filedata)
        (-let* (((&plist :path :group) filedata)
                ((acc-ids acc-filemaps acc-links) acc)
                ((ids links) (org-x-dag-get-file-nodes path group))
                (filemap (cons path (--map (plist-get it :id) ids))))
          `((,@ids ,@acc-ids)
            (,filemap ,@acc-filemaps)
            ((,path ,group ,@links) ,@acc-links)))))
    (-reduce-from #'append-results nil files)))

;; TODO what about all the nodes that don't need to be updated?
(defun org-x-dag-update-ht (to-remove to-insert ht)
  (--each to-remove
    (ht-remove ht it))
  (--each to-insert
    (ht-set ht (car it) (cdr it))))

;; TODO there is a HUGE DIFFERENCE between a 'key' (the things in the hash table
;; the look things up) and a 'node' (which is a cons cell, the car of which is a
;; 'key' and the cdr of which is a 'relation'). These names suck, but the point
;; is we need to distinguish between them otherwise really strange things happen
(defun org-x-dag-update (file-state to-remove to-insert to-update)
  "Update the DAG given files to add and remove.

TO-REMOVE, TO-INSERT, and TO-UPDATE are lists of files to remove
from, add to, and update with the DAG. FILE-STATE is a nested
plist holding the files to be used in the DAG."
  (-let* (((&plist :dag :file->ids :file->links) org-x-dag)
          (files2rem (append to-update to-remove))
          (files2ins (append to-update to-insert))
          (ids2rem (org-x-dag-files->ids files2rem))
          ((ids2ins fms2ins links2ins) (org-x-dag-read-files files2ins)))
    (->> (if (dag-is-empty-p dag) (dag-plist-to-dag ids2ins)
           (dag-edit-nodes ids2rem ids2ins dag))
         (plist-put org-x-dag :dag))
    (org-x-dag-update-ht files2rem fms2ins file->ids)
    (org-x-dag-update-ht files2rem links2ins file->links)
    (plist-put org-x-dag :files file-state)
    (let ((adjlist (dag-get-adjacency-list (plist-get org-x-dag :dag))))
      (->> (plist-get org-x-dag :file->links)
           (ht-values)
           (org-x-dag-get-network-status adjlist)
           (plist-put org-x-dag :netstat)))))

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

;; GLOBAL LOOKUP FUNCTIONS

;; all functions with `org-x-dag->' or `org-x-dag-id->' depend on the value of
;; `org-x-dag'

;; global state slot lookup

(defun org-x-dag->dag ()
  (plist-get org-x-dag :dag))

(defun org-x-dag->adjacency-list ()
  (dag-get-adjacency-list (org-x-dag->dag)))

(defun org-x-dag->current-date ()
  (plist-get org-x-dag :current-date))

(defun org-x-dag->selected-date ()
  (plist-get org-x-dag :selected-date))

(defun org-x-dag->file-state ()
  (plist-get org-x-dag :files))

;; state files

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

(defun org-x-dag-id->node-meta (id)
  (-> (org-x-dag->adjacency-list)
      (ht-get id)
      (plist-get :node-meta)))

(defun org-x-dag-id->hl-meta (id)
  (-> (org-x-dag-id->node-meta id)
      (plist-get :hl-meta)))

(defun org-x-dag-id->bs (id)
  (-> (org-x-dag-id->node-meta id)
      (plist-get :buffer-status)))

(defun org-x-dag-id->hl-meta-prop (id prop)
  (-> (org-x-dag-id->hl-meta id)
      (plist-get prop)))

(defun org-x-dag-id->buffer-parent (id)
  (org-x-dag-id->hl-meta-prop id :buffer-parent))

(defun org-x-dag-id->file (id)
  "Return file for ID."
  (org-x-dag-id->hl-meta-prop id :file))

(defun org-x-dag-id->category (id)
  "Return file for ID."
  (org-x-dag-id->hl-meta-prop id :category))

(defun org-x-dag-id->group (id)
  "Return file group for ID.
Return one of seven values: :lifetime, :survival, :endpoint,
:quarterly, :weekly, :daily, or nil (which means action files)."
  (org-x-dag-id->hl-meta-prop id :group))

(defun org-x-dag-id->point (id)
  "Return point for ID."
  (org-x-dag-id->hl-meta-prop id :point))

(defun org-x-dag-id->todo (id)
  "Return todo keyword for ID."
  (org-x-dag-id->hl-meta-prop id :todo))

(defun org-x-dag-id->title (id)
  "Return title for ID."
  (org-x-dag-id->hl-meta-prop id :title))

(defun org-x-dag-id->local-tags (id)
  "Return local tags for ID."
  (org-x-dag-id->hl-meta-prop id :tags))

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
        (-if-let (parent (org-x-dag-id->buffer-parent id))
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

(defun org-x-dag-id->ns (id)
  (let ((nst (plist-get org-x-dag :netstat)))
    (-> (org-x-dag-id->group id)
        (alist-get nst)
        (ht-get id))))

(defun org-x-dag-id->ns-key (key id)
  (-when-let (n (org-x-dag-id->ns id))
    (plist-get (either-from-right n nil) key)))

(defun org-x-dag-id->planning-timestamp (which id)
  (-some->> (org-x-dag-id->hl-meta-prop id :planning)
    (org-ml-get-property which)))

(defun org-x-dag-id->planning-datetime (which id)
  (-some->> (org-x-dag-id->planning-timestamp which id)
    (org-ml-timestamp-get-start-time)))

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

;; id relationships

(defun org-x-dag-id->parents (id)
  "Return parent nodes of ID."
  (->> (org-x-dag->dag)
       (dag-get-parents id)))

(defun org-x-dag-id->children (id)
  "Return child nodes of ID."
  (->> (org-x-dag->dag)
       (dag-get-children id)))

(defun org-x-dag-id->split-parents-2 (id)
  "Return the buffer and non-buffer parents of ID.

Return value is a list like (BUFFER NON-BUFFER)."
  (let ((parents (org-x-dag-id->parents id)))
    (-if-let (buffer-parent (org-x-dag-id->buffer-parent id))
        (cons buffer-parent (-remove-item buffer-parent parents))
      (cons nil parents))))

(defun org-x-dag-id->linked-parents (id)
  "Return non-buffer (foreign) parents of ID."
  (cdr (org-x-dag-id->split-parents-2 id)))

(defun org-x-dag-id->split-children-2 (id)
  "Return buffer and non-buffer children of ID.

Return value is a list like (BUFFER NON-BUFFER)."
  (->> (org-x-dag-id->children id)
       (--separate (equal (org-x-dag-id->buffer-parent it) id))))

(defun org-x-dag-id->buffer-children (id)
  "Return children of ID that are not linked."
  (car (org-x-dag-id->split-children-2 id)))

(defun org-x-dag-id->all-buffer-children (id)
  "Return nested children of ID that are in the same buffer."
  (->> (org-x-dag-id->buffer-children id)
       (-mapcat #'org-x-dag-id->all-buffer-children)
       (cons id)))

(defun org-x-dag-id->buffer-lineage (id)
  (cl-labels
      ((get-parents
        (acc id)
        (-if-let (p (org-x-dag-id->buffer-parent id))
            (get-parents (cons id acc) p)
          (cons id acc))))
    (get-parents nil id)))

(defun org-x-dag-id->path (category? id)
  (let ((path (->> (org-x-dag-id->buffer-lineage id)
                   (-map #'org-x-dag-id->title)
                   (s-join "/")
                   (s-prepend "/"))))
    (if category?
        (format "%s:%s" (org-x-dag-id->category id) path)
      path)))

(defun org-x-dag-id->formatted-level (id)
  (-> (org-x-dag-id->hl-meta-prop id :level)
      (org-reduced-level)
      (make-string ?\s)))

;; id predicates/identities

(defun org-x-dag-id->is-done-p (id)
  "Return t if ID has done keywords."
  (member (org-x-dag-id->todo id) org-x-done-keywords))

(defun org-x-dag-id->is-toplevel-p (id)
  "Return t if ID is at the top of its buffer."
  (not (org-x-dag-id->buffer-parent id)))

(defun org-x-dag-id->is-buffer-leaf-p (id)
  "Return t if ID has no buffer children."
  (not (org-x-dag-id->buffer-children id)))

(defun org-x-dag-id->is-active-iterator-child-p (id)
  (-> (org-x-dag-id->buffer-parent id)
      (org-x-dag-id->bs)
      (either-from-right nil)
      (cadr)
      (eq :iter-active)))

;; files to ids

(defun org-x-dag-file->ids (file)
  (ht-get (plist-get org-x-dag :file->ids) file))

(defun org-x-dag-files->ids (files)
  (-mapcat #'org-x-dag-file->ids files))

(defun org-x-dag->goal-ids (which)
  (->> (org-x-dag->goal-file which)
       (org-x-dag-file->ids)))

(defun org-x-dag->planning-ids (which)
  (->> (org-x-dag->planning-file which)
       (org-x-dag-file->ids)))

(defun org-x-dag->epg-ids ()
  (org-x-dag->goal-ids :endpoint))

(defun org-x-dag->ltg-ids ()
  (org-x-dag->goal-ids :lifetime))

(defun org-x-dag->svg-ids ()
  (org-x-dag->goal-ids :survival))

(defun org-x-dag->qtp-ids ()
  (org-x-dag->planning-ids :quarterly))

(defun org-x-dag->wkp-ids ()
  (org-x-dag->planning-ids :weekly))

(defun org-x-dag->dlp-ids ()
  (org-x-dag->planning-ids :daily))

(defun org-x-dag->action-ids ()
  (->> (org-x-dag->action-files)
       (org-x-dag-files->ids)))

(defun org-x-dag-filter-ids-tags (tags ids)
  (--filter (-intersection (org-x-dag-id->tags nil it) tags) ids))

(defun org-x-dag-date->tagged-ids (ids tag-getter date)
  (--filter (equal date (funcall tag-getter (org-x-dag-id->tags nil it))) ids))

(defun org-x-dag-date->qtp-ids (date)
  (org-x-dag-date->tagged-ids (org-x-dag->qtp-ids)
                              #'org-x-dag-quarter-tags-to-date
                              date))

(defun org-x-dag-date->wkp-ids (date)
  (org-x-dag-date->tagged-ids (org-x-dag->wkp-ids)
                              #'org-x-dag-weekly-tags-to-date
                              date))

(defun org-x-dag-date->dlp-ids (date)
  (org-x-dag-date->tagged-ids
   (org-x-dag->dlp-ids)
   #'org-x-dag-daily-tags-to-date
   date))

(defun org-x-dag->current-qtp-ids ()
  (-> (org-x-dag->selected-date)
      (org-x-dag-date-to-quarter-start)
      (org-x-dag-date->qtp-ids)))

(defun org-x-dag->current-wkp-ids ()
  (-> (org-x-dag->selected-date)
      (org-x-dag-date-to-week-start)
      (org-x-dag-date->wkp-ids)))

(defun org-x-dag->current-dlp-ids ()
  (-> (org-x-dag->selected-date)
      (org-x-dag-date->dlp-ids)))

(defun org-x-dag-goal-count-tasks (id)
  (->> (org-x-dag-id->children id)
       (-mapcat #'org-x-dag-id->all-buffer-children)
       ;; TODO this isn't very efficient, looking up children twice
       (-remove #'org-x-dag-id->buffer-children)
       (length)))

;; AGENDA LINE FORMATTING

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
                   (e (error "Invalid unit for long datetime: %s" e)))
               (pcase unit
                 ('week (* 7 value))
                 ('day value)
                 ((or 'hour 'minute) (message "WARNING: ..."))
                 (e (error "Invalid unit for short datetime: %s" e)))))))
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


(defun org-x-dag-id->marker (id &optional point)
  (let* ((f (org-x-dag-id->file id))
         (p (or point (org-x-dag-id->point id)))
         (b (or (get-file-buffer f) (find-file-noselect f))))
    (set-marker (make-marker) p b)))

(defun org-x-dag-format-item (id extra tags dotime)
  (let* ((tags* (org-x-dag-prepare-tags tags))
         (category (org-x-dag-id->category id))
         (level (org-x-dag-id->formatted-level id))
         (todo-state (org-x-dag-id->todo id))
         (effort (org-x-dag-id->hl-meta-prop id :effort))
         ;; (head (format "%s %s" todo-state (org-x-dag-id->title id)))
         (head (-> (format "%s %s" todo-state (org-x-dag-id->title id))
                   (org-add-props nil 'effort effort)))
         ;; (time-str (-some->> time (apply #'format "%02i:%02i ")))
         ;; (item (org-agenda-format-item extra head level category tags* time-str))
         ;; NOTE this depends on the buffer position only when using
         ;; breadcrumbs (which I never do)
         (item (org-agenda-format-item extra head level category tags* dotime))
         ;; TODO why am I getting the priority after sending the headline
         ;; through some crazy formatting function?
         (priority (org-get-priority item)))
    (-> (org-x-dag-add-default-props item id)
        (org-add-props nil
            'todo-state todo-state
            'priority priority))))

(defun org-x-dag-format-tag-node (tags id)
  (-let* ((marker (org-agenda-new-marker (org-x-dag-id->marker id)))
          ((ts . ts-type) (org-x-dag-id->agenda-timestamp id))
          (item (org-x-dag-format-item id "" tags nil))
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
            'ts-date ts
            ;; misc
            'type (concat "tagsmatch" ts-type)))))

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

(defun org-x-dag-format-timestamp-node (sel-date pos datetime tags id
                                                 extra-fun face-fun dt-fun)
  (declare (indent 5))
  (-let* (((this-date this-time) (org-x-dag-datetime-split datetime))
          (time-str (-some->> this-time (apply #'format "%02i:%02i ")))
          ;; negative diff -> past and vice versa
          (diff (org-x-dag-date-diff this-date sel-date))
          (extra (funcall extra-fun diff))
          (face (funcall face-fun diff))
          ((date type) (funcall dt-fun diff this-date))
          (props (org-x-dag-planning-props id face pos date this-date type)))
    (-> (org-x-dag-format-item id extra tags time-str)
        (org-add-props props))))

(defun org-x-dag-format-scheduled-node (sel-date pos datetime tags id)
  (org-x-dag-format-timestamp-node sel-date pos datetime tags id
    (lambda (diff)
      ;; hopefully this is right...if it is this seems silly
      (-let (((today past) org-agenda-scheduled-leaders))
        (cond
         ((= 0 diff) today)
         ((< diff 0) (format past (- diff)))
         (t ""))))
    (lambda (diff)
      (cond
       ((< diff 0) 'org-scheduled-previously)
       ((= diff 0) 'org-scheduled-today)
       (t 'org-scheduled)))
    (lambda (diff this-date)
      (if (< diff 0) `(,this-date "past-scheduled")
        `(,sel-date "scheduled")))))

(defun org-x-dag-format-deadline-node (sel-date pos datetime tags id)
  (org-x-dag-format-timestamp-node sel-date pos datetime tags id
    (lambda (diff)
      (-let* (((now future past) org-agenda-deadline-leaders))
        (cond
         ((< 0 diff) (format future diff))
         ((< diff 0) (format past diff))
         (t now))))
    ;; TODO the stock deadline formatter uses the warning time to determine this
    ;; based on percentage; I'm lazy and don't feel like doing that (now) but I
    ;; might in the future
    (lambda (diff)
      (cond
       ((< 5 diff) 'org-upcoming-distant-deadline)
       ((< 1 diff) 'org-upcoming-deadline)
       (t 'org-warning)))
    (lambda (diff this-date)
      (if (< 0 diff) `(,sel-date "upcoming-deadline")
        `(,this-date "deadline")))))

;;; ID FUNCTIONS

;; ranking

;; TODO not sure if these are useful?

;; (defmacro org-x-dag-ids-rank (form ids)
;;   (declare (indent 1))
;;   `(cl-labels
;;        ((compare
;;          (a b)
;;          (cond
;;           ((not (or a b)) t)
;;           ((= (car a) (car b)) (compare (cdr a) (cdr b)))
;;           (t (> (car a) (car b))))))
;;      (->> (--map (cons it ,form) ,ids)
;;           (--sort (compare (cdr it) (cdr other))))))
  
;; (defmacro org-x-dag-ids-rank-by-children (form ids)
;;   `(org-x-dag-ids-rank
;;        (let ((it (org-x-dag-id->children it)))
;;          ,form)
;;      ,ids))

;; (defmacro org-x-dag-ids-rank-by-parents (form ids)
;;   `(org-x-dag-ids-rank
;;        (let ((it (org-x-dag-id->parents it)))
;;          ,form)
;;      ,ids))

;; (defun org-x-dag-rank-leaf-goals (quarter ids)
;;   (cl-flet
;;       ((score
;;         (buckets id)
;;         ;; TODO what happens when I don't have a bucket?
;;         (let ((idx (-elem-index (org-x-dag-id->bucket t id) (reverse buckets)))
;;               (ntasks (org-x-dag-goal-count-tasks id)))
;;           (list idx ntasks))))
;;     (let ((bs (org-x-qtp-get-buckets quarter)))
;;       (org-x-dag-ids-rank (score bs it) ids))))

;; ;; reductions

;; ;; TODO this is a naive approach that will effectively expand the dag into
;; ;; a tree for nodes that share common children/parents. I might want to handle
;; ;; these special cases in a better way (example, 'summation' could count nodes
;; ;; multiple times, which may or may not make sense)
;; (defmacro org-x-dag--id-reduce (id-getter branch-form leaf-form init id)
;;   (declare (indent 1))
;;   (let ((cs (make-symbol "--children")))
;;     `(cl-labels
;;          ((reduce
;;            (acc it)
;;            (-if-let (,cs (,id-getter ,id))
;;                (--reduce-from (reduce acc it) ,branch-form ,cs)
;;              ,leaf-form)))
;;        (reduce ,init ,id))))

;; (defmacro org-x-dag-id-reduce-down (branch-form leaf-form init id)
;;   `(org-x-dag--id-reduce org-x-dag-id->children
;;      ,branch-form ,leaf-form ,init ,id))

;; (defmacro org-x-dag-id-reduce-up (branch-form leaf-form init id)
;;   `(org-x-dag--id-reduce org-x-dag-id->parents
;;      ,branch-form ,leaf-form ,init ,id))

;;; ITEM GENERATORS

;; auxiliary macros

(defmacro org-x-dag-with-ids (files id-form)
  (declare (indent 1))
  `(with-temp-buffer
    ;; TODO this is silly and it adds 0.1 seconds to this function's runtime;
    ;; it is only needed to get the todo keyword the right color
    (org-mode)
    (--mapcat ,id-form (org-x-dag-files->ids ,files))))

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

;; tasks/projects

;; TODO this includes tasks underneath cancelled headlines
(defun org-x-dag-itemize-tasks (files)
  (org-x-dag-with-ids files
    (pcase (either-from-right (org-x-dag-id->bs it) nil)
      (`(:sp-task :task-active ,s)
       (-let (((&plist :sched :dead) s))
         (-let (((&plist :committed c) (-when-let (ns (org-x-dag-id->ns it))
                                         (either-from-right ns nil))))
           (when (and (not sched) (not dead) c)
             (let ((tags (org-x-dag-id->tags nil it))
                   (bp (org-x-dag-id->buffer-parent it)))
               (-> (org-x-dag-format-tag-node tags it)
                   (org-add-props nil
                       'x-is-standalone (not bp)
                       'x-status :active)
                   (list))))))))))

(defun org-x-dag-itemize-projects (files)
  (org-x-dag-with-ids files
    (pcase (either-from-right (org-x-dag-id->bs it) nil)
      (`(:sp-proj . ,status-data)
       ;; NOTE in the future there might be more than just the car to this
       (let ((status (car status-data)))
         (-when-let (priority (cl-case status
                                (:proj-active 4)
                                (:proj-wait 3)
                                (:proj-hold 2)
                                (:proj-stuck 1)))
           (-when-let ((&plist :committed) (-when-let (ns (org-x-dag-id->ns it))
                                             (either-from-right ns nil)))
             (let ((tags (org-x-dag-id->tags nil it)))
               (-> (org-x-dag-format-tag-node tags it)
                   (org-add-props nil
                       'x-toplevelp (org-x-dag-id->is-toplevel-p it)
                       'x-status status
                       'x-priority priority)
                   (list))))))))))

(defun org-x-dag-itemize-iterators (files)
  (org-x-dag-with-ids files
    (pcase (either-from-right (org-x-dag-id->bs it) nil)
      (`(:sp-proj . ,status-data)
       (let ((status (car status-data)))
         (when (memq status '(:iter-empty :iter-active))
           (let ((tags (org-x-dag-id->tags nil it)))
             (-> (org-x-dag-format-tag-node tags it)
                 (org-add-props nil
                     'x-status status)
                 (list)))))))))

(defun org-x-dag-itemize-incubated (files)
  (org-x-dag-with-ids files
    (-when-let (type (pcase (either-from-right (org-x-dag-id->bs it) nil)
                       (`(:sp-proj :proj-complete ,_) nil)
                       (`(:sp-task :task-complete ,_) nil)
                       (`(:sp-iter :iter-complete ,_) nil)
                       (`(:sp-subiter :si-complete ,_) nil)
                       (`(:sp-proj . ,_) :proj)
                       (`(:sp-task . ,_ ) :task)
                       (`(:sp-iter . ,_) :iter)
                       (`(:sp-subiter . ,_) :subiter)))
      (-let (((&plist :committed c :planned p :survivalp s)
              (-some-> (org-x-dag-id->ns it)
                (either-from-right  nil))))
        (when (not p)
          (let ((tags (org-x-dag-id->tags nil it))
                (toplevelp (pcase type
                             ((or :proj :task)
                              (org-x-dag-id->is-toplevel-p it))
                             (:subiter
                              (org-x-dag-id->is-active-iterator-child-p it)))))
            (-> (org-x-dag-format-tag-node tags it)
                (org-add-props nil
                    'x-type type
                    'x-toplevelp toplevelp
                    'x-survivalp s
                    'x-committedp (and c t))
                (list))))))))

(defun org-x-dag-itemize-tl-goals (files)
  (let ((plan-ids (org-x-dag->current-qtp-ids)))
    (cl-flet
        ((mk-item
          (id type plannedp fulfilledp committedp)
          (let ((tags (org-x-dag-id->tags nil id))
                (leafp (org-x-dag-id->is-buffer-leaf-p id)))
            (-> (org-x-dag-format-tag-node tags id)
                (org-add-props nil
                    'x-type type
                    'x-leafp leafp
                    'x-plannedp (-intersection plannedp plan-ids)
                    'x-fulfilledp fulfilledp
                    'x-committedp committedp)
                (list)))))
      (org-x-dag-with-ids files
        (pcase (either-from-right (org-x-dag-id->bs it) nil)
          (`(:lifetime :active)
           (-when-let (ns (org-x-dag-id->ns it))
             (-let (((&plist :planned p :fulfilled f)
                     (either-from-right ns nil)))
               (mk-item it :lifetime p f nil))))
          ;; TODO not sure how I want to handle deadlines yet here
          (`(:endpoint :active ,_)
           (-when-let (ns (org-x-dag-id->ns it))
             (-let (((&plist :planned p :fulfilled f :committed c)
                     (either-from-right ns nil)))
               (mk-item it :endpoint p f c)))))))))

(defun org-x-dag-itemize-qtp (files)
  (let ((wkp-ids (org-x-dag->current-wkp-ids))
        (sel-date (->> (org-x-dag->selected-date)
                       (org-x-dag-date-to-quarter-start))))
    (org-x-dag-with-ids files
      (pcase (either-from-right (org-x-dag-id->bs it) nil)
        (`(:quarterly :active ,dead)
         (let* ((tags (org-x-dag-id->tags nil it))
                (date (org-x-dag-quarter-tags-to-date tags)))
           (when (org-x-dag-datetime= sel-date date)
             (-when-let (ns (org-x-dag-id->ns it))
               (-let (((&plist :planned p :committed c)
                       (either-from-right ns nil)))
                 ;; TODO actually handle deadlines
                 (-> (org-x-dag-format-tag-node tags it)
                     (org-add-props nil
                         'x-deadline dead
                         'x-plannedp (-intersection p wkp-ids)
                         'x-committedp c)
                     (list)))))))))))

;; TODO not DRY
(defun org-x-dag-itemize-wkp (files)
  (let ((sel-date (->> (org-x-dag->selected-date)
                       (org-x-dag-date-to-week-start))))
    (org-x-dag-with-ids files
      (pcase (either-from-right (org-x-dag-id->bs it) nil)
        (`(:weekly :active)
         (let* ((tags (org-x-dag-id->tags nil it))
                (date (org-x-dag-weekly-tags-to-date tags))
                (day (nth 2 (reverse tags))))
           (when (org-x-dag-datetime= sel-date date)
             (-when-let (ns (org-x-dag-id->ns it))
               (-let (((&plist :planned p :committed c)
                       (either-from-right ns nil)))
                 (-> (org-x-dag-format-tag-node tags it)
                     (org-add-props nil
                         'x-day day
                         'x-plannedp p
                         'x-committedp c)
                     (list)))))))))))

(defun org-x-dag--item-add-goal-ids (item ids)
  (if ids
      (--map (org-add-props (copy-seq item) nil 'x-goal-id it) ids)
    (list (org-add-props item nil 'x-goal-id nil))))

(defun org-x-dag-itemize-tasks-with-goals (files)
  (org-x-dag-with-ids files
    (pcase (either-from-right (org-x-dag-id->bs it) nil)
      (`(:sp-task :task-active ,_)
       (-let ((goal-ids (-when-let (ns (org-x-dag-id->ns it))
                          (either-from* ns
                            nil
                            (unless (plist-get it :survivalp)
                              (plist-get it :committed)))))
              (tags (org-x-dag-id->tags nil it))
              (bp (org-x-dag-id->buffer-parent it)))
         (-> (org-x-dag-format-tag-node tags it)
             (org-add-props nil
                 'x-is-standalone (not bp)
                 'x-status :active)
             (org-x-dag--item-add-goal-ids goal-ids)))))))

(defun org-x-dag-itemize-projects-with-goals (files)
  (org-x-dag-with-ids files
    (pcase (either-from-right (org-x-dag-id->bs it) nil)
      (`(:sp-proj . ,s)
       (unless (eq (car s) :proj-complete)
         (let ((goal-ids (-when-let (ns (org-x-dag-id->ns it))
                           (either-from* ns
                             nil
                             (unless (plist-get it :survivalp)
                               (plist-get it :committed)))))
               (tags (org-x-dag-id->tags nil it)))
           (-> (org-x-dag-format-tag-node tags it)
               (org-x-dag--item-add-goal-ids goal-ids))))))))

(defun org-x-dag-itemize-archived (files)
  (org-x-dag-with-ids files
    (-let (((comptime type)
            (pcase (either-from-right (org-x-dag-id->bs it) nil)
              (`(:sp-proj :proj-complete ,c) `(,c :proj))
              (`(:sp-task :task-complete ,c) `(,c :task))
              (`(:sp-iter :iter-complete ,c) `(,c :iter))
              (`(:sp-subiter :si-complete ,c) `(,c :subiter)))))
      (when (and comptime
                 (or (and (memq type '(:proj :task))
                          (org-x-dag-id->is-toplevel-p it))
                     (eq type :iter)
                     (and (eq type :subiter)
                          (org-x-dag-id->is-active-iterator-child-p it))))
        (-let ((epoch (plist-get comptime :epoch)))
          (when (org-x-dag-time-is-archivable-p epoch)
            (let ((tags (org-x-dag-id->tags nil it)))
              (-> (org-x-dag-format-tag-node tags it)
                  (org-add-props nil
                      'x-type type)
                  (list)))))))))

(defun org-x-dag-itemize-errors (files)
  (cl-flet
      ((format-id
        (id msg)
        (-> (org-x-dag-format-tag-node nil id)
            (org-add-props nil
                'x-error msg))))
    (with-temp-buffer
      (org-mode)
      (->> (org-x-dag-files->ids files)
           (--map (pcase (org-x-dag-id->bs it)
                    (`(:error ,msg) (format-id it msg))))
           (-non-nil)))))

;; agenda/calendar

(defun org-x-dag-itemize-agenda (files sel-date)
  (cl-flet*
      ((format-timestamps
        (todayp sel-date id ts get-datetimes-fun format-datetime-fun)
        (let ((pts (org-x-dag-partition-timestamp ts)))
          (-when-let (datetimes (funcall get-datetimes-fun sel-date pts))
            ;; TODO this will show all tasks regardless of if they have a
            ;; goal/plan or not
            (-let ((tags (org-x-dag-id->tags nil id))
                   ((&plist :pos) pts)
                   (donep (org-x-dag-id->is-done-p id)))
              (--> datetimes
                   (--remove (and donep (not (org-x-dag-datetime= (-take 3 it) sel-date))) it)
                   (if (not todayp) (--remove (org-x-dag-datetime< (-take 3 it) sel-date) it) it)
                   (--map (funcall format-datetime-fun sel-date pos it tags id) it))))))
       (format-scheduleds
        (todayp sel-date id ts)
        (format-timestamps todayp sel-date id ts
                           #'org-x-dag-get-scheduled-at
                           #'org-x-dag-format-scheduled-node))
       (format-deadlines
        (todayp sel-date id ts)
        (format-timestamps todayp sel-date id ts
                           #'org-x-dag-get-deadlines-at
                           #'org-x-dag-format-deadline-node))
       (with-task-ish
        (id todayp)
        (append
         (-when-let (dead (org-x-dag-id->planning-timestamp :deadline id))
           (format-deadlines todayp sel-date id dead))
         (-when-let (sched (org-x-dag-id->planning-timestamp :scheduled id))
           (format-scheduleds todayp sel-date id sched)))))
    (let ((todayp (= (org-x-dag-date-to-absolute sel-date) (org-today))))
      (org-x-dag-with-ids files
        (pcase (either-from-right (org-x-dag-id->bs it) nil)
          (`(:daily :active (:sched ,sched))
           (-when-let (datetime (org-ml-timestamp-get-start-time sched))
             (when (org-x-dag-datetime= sel-date (-take 3 datetime))
               (format-scheduleds todayp sel-date it sched))))
          (`(:daily :complete ,_)
           (-when-let (sched (org-x-dag-id->planning-timestamp :scheduled it))
             (when (org-x-dag-datetime= sel-date (-take 3 (org-ml-timestamp-get-start-time sched)))
               (format-scheduleds todayp sel-date it sched))))
          (`(:sp-task :task-active ,_)
           (with-task-ish it todayp))
          (`(:sp-task :task-complete ,_)
           (with-task-ish it todayp))
          (`(:sp-subiter :si-active ,_)
           (with-task-ish it todayp))
          (`(:sp-subiter :si-complete ,_)
           (with-task-ish it todayp)))))))

;;; BUFFER MANIPULATION

;;; org-ml node functions

;; parent link drawers

(defun org-x-dag-build-parent-link-drawer (ids)
  (->> (-map #'org-x-dag-id->link-item ids)
       (apply #'org-ml-build-plain-list)
       (org-ml-build-drawer org-x-drwr-parent-links)))

(defun org-x-dag-drawer-get-parent-links (drawer)
  (cl-flet
      ((parse-item
        (item)
        (let ((first (car (org-ml-item-get-paragraph item))))
          (if (and (org-ml-is-type 'link first)
                   (equal (org-ml-get-property :type first) "id"))
              (org-ml-get-property :path first)
            (error "Invalid link node: %S" first)))))
    (-when-let (first (car (org-ml-get-children drawer)))
      (if (org-ml-is-type 'plain-list first)
          (->> (org-ml-get-children first)
               (-map #'parse-item))
        (error "Invalid parent link drawer")))))

(defun org-x-dag-drawer-set-parent-links (ids drawer)
  (-when-let (pl (-some->> (-map #'org-x-dag-id->link-item ids)
                   (apply #'org-ml-build-plain-list)))
    (org-ml-set-children (list pl) drawer)))

;; headline parent links

(defun org-x-dag-section-get-parent-links (children)
  (->> (--find (org-x--is-drawer-with-name org-x-drwr-parent-links it) children)
       (org-x-dag-drawer-get-parent-links)))

(defun org-x-dag-section-set-parent-links (ids children)
  (-if-let (i (--find-index (org-x--is-drawer-with-name org-x-drwr-parent-links it)
                            children))
      (-if-let (d (org-x-dag-drawer-set-parent-links ids (nth i children)))
          (-replace-at i d children)
        (-remove-at i children))
    (if ids (cons (org-x-dag-build-parent-link-drawer ids) children) children)))

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

;; toplevel section parent links

(defun org-x-dag-tl-section-get-parent-links (section)
  (->> (org-ml-get-children section)
       (org-x-dag-section-get-parent-links)))

(defun org-x-dag-tl-section-set-parent-links (ids section)
  (org-ml-map-children*
    (org-x-dag-section-set-parent-links ids it)
    section))

(defmacro org-x-dag-tl-section-map-parent-links* (form children)
  (let ((s (make-symbol "--section")))
    `(let* ((,s ,children)
            (it (org-x-dag-tl-section-get-parent-links ,s)))
       (org-x-dag-tl-section-set-parent-links ,form ,s))))

(defun org-x-dag-tl-section-add-parent-link (id section)
  (org-x-dag-tl-section-map-parent-links* (cons id it) section))

(defun org-x-dag-tl-section-remove-parent-link (id section)
  (org-x-dag-tl-section-map-parent-links*
   (--remove-first (equal it id) it)
   section))

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

(defun org-x-dag-build-planning-headline (title tag level section subheadlines)
  (apply #'org-ml-build-headline!
         :title-text title
         :tags (list tag)
         :level level
         :section-children section
         subheadlines))

(defun org-x-dag-build-year-headline (year subheadlines)
  (let ((title (number-to-string year))
        (tag (org-x-dag-format-year-tag year)))
    (org-x-dag-build-planning-headline title tag 1 nil subheadlines)))

(defun org-x-dag-build-quarter-headline (quarter section subheadlines)
  (let ((title (format "Quarter %d" quarter))
        (tag (org-x-dag-format-quarter-tag quarter)))
    (org-x-dag-build-planning-headline title tag 2 section subheadlines)))

(defun org-x-dag-build-week-headline (year weeknum subheadlines)
  (-let* (((_ m d) (org-x-dag-week-number-to-date year weeknum))
          (m* (calendar-month-name m))
          (title (format "%s %s" m* d))
          (tag (org-x-dag-format-week-tag weeknum)))
    (org-x-dag-build-planning-headline title tag 2 nil subheadlines)))

(defun org-x-dag-build-month-headline (month subheadlines)
  (let ((title (calendar-month-name month))
        (tag (org-x-dag-format-month-tag month)))
    (org-x-dag-build-planning-headline title tag 2 nil subheadlines)))

(defun org-x-dag-build-day-headline (date subheadlines)
  (-let* (((y m d) date)
          (title (format "%d-%02d-%02d" y m d))
          (tag (org-x-dag-format-day-tag d)))
    (org-x-dag-build-planning-headline title tag 3 nil subheadlines)))

(defun org-x-dag-build-day-of-week-headline (daynum subheadlines)
  (let ((title (elt calendar-day-name-array daynum))
        (tag (alist-get daynum org-x-dag-weekly-tags)))
    (org-x-dag-build-planning-headline title tag 3 nil subheadlines)))

;; headline ids

(defun org-x-dag-headline-get-id (headline)
  (org-ml-headline-get-node-property "ID" headline))

(defun org-x-dag-headline-add-id (headline)
  (org-ml-headline-set-node-property "ID" (org-id-new) headline))

;; planning headline builders

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

;; empty plans

(defun org-x-dag-qtp-empty ()
  (->> (-map #'cdr org-x-life-categories)
       (--map (org-ml-build-headline! :level 3
                                      :title-text (plist-get it :desc)
                                      :tags `(,(plist-get it :tag))))))

(defun org-x-dag-wkp-empty ()
  (->> (-map #'car org-x-dag-weekly-tags)
       (--map (org-x-dag-build-day-of-week-headline it nil))))

;;; stateful buffer function

;; nested headline manipulation

(defun org-x-dag-headline-get-nested (path find-funs)
  (declare (indent 1))
  (cl-labels
      ((get-subheadlines
        (find-funs hls)
        (-let* (((next . rest) find-funs))
          (-when-let (found (funcall next hls))
            (if (not rest) found
              (->> (org-ml-headline-get-subheadlines found)
                   (get-subheadlines rest)))))))
    (org-x-with-file path
      (->> (org-ml-parse-subtrees 'all)
           (get-subheadlines find-funs)))))

(defun org-x-dag-headline-set-nested (path to-set hierachy-funs)
  (declare (indent 2))
  (cl-labels
      ((update-subheadlines
        (funs hls)
        (-let ((((find-fun _) . rest) funs))
          (-if-let (hl (funcall find-fun hls))
              (if rest (->> (org-ml-headline-get-subheadlines hl)
                            (update-subheadlines rest))
                (org-ml-update* (org-ml-set-children to-set it) hl)
                (org-ml-get-property :begin hl))
            (let ((end (1+ (org-ml-get-property :end (-last-item hls)))))
              (->> (-map #'cadr funs)
                   (reverse)
                   (--reduce-from (list (funcall it acc)) to-set)
                   (org-ml-insert end))
              end)))))
    (org-x-with-file path
      (->> (org-ml-parse-subtrees 'all)
           (update-subheadlines hierachy-funs)))))

;; quarterly plan

(defun org-x-dag-qtp-to-children (qt-plan)
  (-let* (((&plist :categories :goals) qt-plan)
          ;; TODO what happens if there are no categories?
          (sec (-some->> categories
                 (--map-indexed (org-ml-build-item!
                                 :bullet it-index
                                 :paragraph (symbol-name it)))
                 (apply #'org-ml-build-plain-list)
                 (org-ml-build-drawer org-x-drwr-categories)
                 (list)))
          (subtrees (--map (apply #'org-ml-build-headline!
                                  :level 3
                                  :title-text (plist-get (cdr it) :desc)
                                  :tags `(,(plist-get (cdr it) :tag))
                                  (alist-get (car it) goals))
                           org-x-life-categories)))
    (list sec subtrees)))

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

(defun org-x-dag-qtp-get-headline (date)
  (-let* (((y q) (org-x-dag-date-to-quarter date)))
    (org-x-dag-headline-get-nested (org-x-dag->planning-file :quarterly)
      (list (-partial #'org-x-dag-headlines-find-year y)
            (-partial #'org-x-dag-headlines-find-quarter q)))))

(defun org-x-dag-qtp-get (quarter)
  (org-x-with-file (org-x-qtp-get-file)
    (-let (((year qnum) quarter))
      (->> (org-ml-parse-subtrees 'all)
           (org-x-dag-headlines-find-year year)
           (org-ml-headline-get-subheadlines)
           (org-x-dag-headlines-find-quarter qnum)
           (org-ml-get-children)
           (org-x-dag-qtp-from-children)))))

(defun org-x-dag-qtp-set-headlines (date headlines)
  (-let* (((y q) (org-x-dag-date-to-quarter date))
          (path (org-x-dag->planning-file :quarterly))
          (find-year (-partial #'org-x-dag-headlines-find-year y))
          (find-quarter (-partial #'org-x-dag-headlines-find-quarter q))
          (build-year (-partial #'org-x-dag-build-year-headline y))
          (build-quarter (-partial #'org-x-dag-build-quarter-headline q nil)))
    (org-x-dag-headline-set-nested path headlines
      `((,find-year ,build-year)
        (,find-quarter ,build-quarter)))))

;; (defun org-x-dag-qtp-set (quarter qt-plan)
;;   (cl-flet
;;       ((build-yr-headline
;;         (year qnum section children)
;;         (->> (org-x-dag-build-quarter-headline qnum section children)
;;              (list)
;;              (org-x-dag-build-year-headline year))))
;;     (org-x-with-file (org-x-dag->planning-file :quarterly)
;;       (-let* (((year qnum) quarter)
;;               (sts (org-ml-parse-subtrees 'all))
;;               ((section subhls) (org-x-dag-qtp-to-children qt-plan)))
;;         (-if-let (st-yr (org-x-dag-headlines-find-year year sts))
;;             (-if-let (st-qt (->> (org-ml-headline-get-subheadlines st-yr)
;;                                  (org-x-dag-headlines-find-quarter qnum)))
;;                 (org-ml-update* (org-ml-set-children subhls it) st-qt)
;;               (org-ml-update*
;;                 (->> (org-x-dag-build-quarter-headline qnum section subhls)
;;                      (-snoc it))
;;                 st-yr))
;;           (let ((end (1+ (org-ml-get-property :end (-last-item sts)))))
;;             (org-ml-insert end (build-yr-headline year qnum section subhls))))))))

;; (defmacro org-x-dag-qtp-map (quarter form)
;;   (declare (indent 1))
;;   `(let ((it (org-x-dag-qtp-get ,quarter)))
;;      (org-x-dag-qtp-set ,quarter ,form)))

;; (defun org-x-dag-qtp-get-key (key quarter)
;;   (plist-get (org-x-dag-qtp-get quarter) key))

;; (defun org-x-dag-qtp-set-key (quarter key xs)
;;   (org-x-dag-qtp-map quarter
;;     (plist-put it key xs)))

;; (defun org-x-qtp-get-buckets (quarter)
;;   (org-x-dag-qtp-get-key :categories quarter))

;; (defun org-x-qtp-get-goals (quarter)
;;   (org-x-dag-qtp-get-key :goals quarter))

;; (defun org-x-qtp-get-goal-ids (quarter)
;;   (->> (org-x-qtp-get-goals quarter)
;;        (--map (org-ml-headline-get-node-property "ID" it))))

;; (defun org-x-qtp-get-goal-parent-ids (quarter)
;;   (->> (org-x-qtp-get-goals quarter)
;;        (-mapcat #'org-x-dag-headline-get-parent-links)))

;; (defun org-x-qtp-set-categories (quarter categories)
;;   (org-x-dag-qtp-set-key quarter :categories categories))

;; (defun org-x-qtp-set-goals (quarter goals)
;;   (org-x-dag-qtp-set-key quarter :goals goals))

;; (defmacro org-x-qtp-map-categories (quarter form)
;;   `(let ((it (org-x-qtp-get-buckets ,quarter)))
;;      (org-x-qtp-set-categories ,quarter ,form)))

;; (defmacro org-x-qtp-map-goals (quarter form)
;;   `(let ((it (org-x-qtp-get-goals ,quarter)))
;;      (org-x-qtp-set-goals ,quarter ,form)))

;; (defun org-x-qtp-add-goal (quarter headline)
;;   (org-x-qtp-map-goals quarter (cons headline it)))

;; (defun org-x-qtp-add-goal-ids (quarter ids title allocation)
;;   (->> (org-x-dag-build-qtp-headline title nil ids allocation)
;;        (org-x-qtp-add-goal quarter)))

;; weekly plan

(defun org-x-dag-weekly-headlines-to-alist (headlines)
  (->> (-map #'car org-x-dag-weekly-tags)
       (--map (->> (org-x-dag-headlines-find-day-of-week it headlines)
                   (org-ml-headline-get-subheadlines)
                   (cons it)))))

(defun org-x-dag-weekly-alist-to-headlines (plan)
  (--map (-let (((daynum . hls) it))
           (org-x-dag-build-day-of-week-headline daynum hls))
         plan))

(defun org-x-dag-wkp-get-week-headline (date)
  (-let (((y _ _) date)
         (w (org-x-dag-date-to-week-number date)))
    (org-x-dag-headline-get-nested (org-x-dag->planning-file :weekly)
      (list (-partial #'org-x-dag-headlines-find-year y)
            (-partial #'org-x-dag-headlines-find-week w)))))

(defun org-x-dag-wkp-get-day-headline (date)
  (-let ((n (org-x-dag-date-to-day-number date)))
    (->> (org-x-dag-wkp-get-week-headline date)
         (org-ml-headline-get-subheadlines)
         (org-x-dag-headlines-find-day-of-week n))))

(defun org-x-dag-wkp-set-headlines (date headlines)
  (-let* (((y _ _) date)
          (w (org-x-dag-date-to-week-number date))
          (path (org-x-dag->planning-file :weekly))
          (find-year (-partial #'org-x-dag-headlines-find-year y))
          (find-week (-partial #'org-x-dag-headlines-find-week w))
          (build-year (-partial #'org-x-dag-build-year-headline y))
          (build-week (-partial #'org-x-dag-build-week-headline y w)))
    (org-x-dag-headline-set-nested path headlines
      `((,find-year ,build-year)
        (,find-week ,build-week)))))

;; TODO these functions need to take dates and not 'week's (whatever those are)
(defun org-x-dag-wkp-get (date)
  (->> (org-x-dag-wkp-get-day-headline date)
       (org-ml-headline-get-subheadlines)
       (org-x-dag-weekly-headlines-to-alist)))

;; (defun org-x-dag-wkp-set (week plan)
;;   (cl-flet*
;;       ((build-yr-headline
;;         (year weeknum children)
;;         (->> (org-x-dag-build-week-headline year weeknum children)
;;              (list)
;;              (org-x-dag-build-year-headline year))))
;;     (org-x-with-file (org-x-get-weekly-plan-file)
;;       (-let* (((year weeknum) week)
;;               (sts (org-ml-parse-subtrees 'all))
;;               (children (org-x-dag-weekly-alist-to-headlines plan)))
;;         (-if-let (st-yr (org-x-dag-headlines-find-year year sts))
;;             (-if-let (st-wk (->> (org-ml-headline-get-subheadlines st-yr)
;;                                  (org-x-dag-headlines-find-week weeknum)))
;;                 (org-ml-update* (org-ml-set-children children it) st-wk)
;;               (org-ml-update*
;;                 (-snoc it (org-x-dag-build-week-headline year weeknum children))
;;                 st-yr))
;;           (let ((end (1+ (org-ml-get-property :end (-last-item sts)))))
;;             (org-ml-insert end (build-yr-headline year weeknum children))))))))

;; (defmacro org-x-dag-wkp-map (week form)
;;   (declare (indent 1))
;;   (let ((w (make-symbol "--week")))
;;     `(let* ((,w ,week)
;;             (it (org-x-dag-wkp-get ,w)))
;;        (org-x-dag-wkp-set ,w ,form))))

;; (defun org-x-dag-wkp-day-get (week daynum)
;;   (alist-get daynum (org-x-dag-wkp-get week)))

;; (defun org-x-dag-wkp-day-set (week daynum headlines)
;;   (org-x-dag-wkp-map week
;;     (--replace-where (= daynum (car it)) (cons daynum headlines) it)))

;; (defmacro org-x-dag-wkp-day-map (week daynum form)
;;   (declare (indent 2))
;;   (let ((w (make-symbol "--week"))
;;         (d (make-symbol "--daynum")))
;;     `(let* ((,w ,week)
;;             (,d ,daynum)
;;             (it (org-x-dag-wkp-day-get ,w ,d)))
;;        (org-x-dag-wkp-day-set ,w ,d ,form))))

;; (defun org-x-dag-wkp-day-add (week daynum headline)
;;   (org-x-dag-wkp-day-map week daynum (cons headline it)))

;; (defun org-x-dag-wkp-add-goal (week daynum title ids desc)
;;   (->> (org-x-dag-build-wkp-headline title desc ids)
;;        (org-x-dag-wkp-day-add week daynum)))

;; daily plan

(defun org-x-dag-dlp-get-headline (date)
  (org-x-with-file (org-x-dag->planning-file :daily)
    (-let (((y m d) date))
      (->> (org-ml-parse-subtrees 'all)
           (org-x-dag-headlines-find-year y)
           (org-ml-headline-get-subheadlines)
           (org-x-dag-headlines-find-month m)
           (org-ml-headline-get-subheadlines)
           (org-x-dag-headlines-find-day d)))))

(defun org-x-dag-dlp-get (date)
  (->> (org-x-dag-dlp-get-headline date)
       (org-ml-headline-get-subheadlines)))

(defun org-x-dag-dlp-set (date headlines)
  (-let* (((y m d) date)
          (path (org-x-dag->planning-file :daily))
          (find-year (-partial #'org-x-dag-headlines-find-year y))
          (find-month (-partial #'org-x-dag-headlines-find-month m))
          (find-day (-partial #'org-x-dag-headlines-find-day d))
          (build-year (-partial #'org-x-dag-build-year-headline y))
          (build-month (-partial #'org-x-dag-build-month-headline m))
          (build-day (-partial #'org-x-dag-build-day-headline date)))
    (org-x-dag-headline-set-nested path headlines
      `((,find-year ,build-year)
        (,find-month ,build-month)
        (,find-day ,build-day)))))

(defmacro org-x-dag-dlp-map (date form)
  (declare (indent 1))
  (let ((d (make-symbol "--date")))
    `(let* ((,d ,date)
            (it (org-x-dag-dlp-get ,d)))
       (org-x-dag-dlp-set ,d ,form))))

(defun org-x-dag-dlp-add (date headline)
  (org-x-dag-dlp-map date (-snoc it headline)))

(defun org-x-dag-dlp-add-task (date title ids time effort)
  (let ((datetime `(,@date ,@time)))
    (->> (org-x-dag-build-dlp-headline title nil ids datetime)
         (org-ml-headline-set-node-property org-effort-property effort)
         (org-x-dag-dlp-add date))))

;;; INTERACTIVE FUNCTIONS

;;; misc/navigation

(defun org-x-dag-set-date ()
  (interactive)
  (let ((date (->> (org-read-date nil t)
                   (decode-time)
                   (-drop 3)
                   (-take 3)
                   (reverse))))
    (plist-put org-x-dag :selected-date date)
    (apply #'message "Org-DAG date set to %d-%02d-%02d" date)))

(defun org-x-dag-show-date ()
  (interactive)
  (->> (plist-get org-x-dag :selected-date)
       (apply #'message "Org-DAG date is %d-%02d-%02d")))

(defun org-x-dag--goto-current (what file-key hl-fun &optional create-fun)
  (declare (indent 2))
  (cl-flet
      ((goto
        (point)
        (find-file (org-x-dag->planning-file file-key))
        (goto-char point)
        (org-reveal)))
    (let ((d (org-x-dag->selected-date)))
      (-if-let (p (-some->> (funcall hl-fun d)
                    (org-ml-get-property :begin)))
          (goto p)
        (let ((msg (format "%s does not exist for current date." what)))
          (if create-fun
              (when (yes-or-no-p (format "%s Make new?" msg))
                (->> (funcall create-fun d)
                     (goto)))
            (message msg)))))))

(defun org-x-dag-goto-current-quarterly-plan ()
  (interactive)
  (org-x-dag--goto-current "Quarterly plan" :quarterly
    #'org-x-dag-qtp-get-headline
    #'org-x-dag--new-qtp))

(defun org-x-dag-goto-current-weekly-plan ()
  (interactive)
  (org-x-dag--goto-current "Weekly plan (week)" :weekly
    #'org-x-dag-wkp-get-week-headline
    #'org-x-dag--new-wkp))

(defun org-x-dag-goto-current-weekly-plan-day ()
  (interactive)
  (org-x-dag--goto-current "Weekly plan (day)" :weekly
    #'org-x-dag-wkp-get-day-headline))

(defun org-x-dag-goto-current-daily-plan ()
  (interactive)
  (org-x-dag--goto-current "Daily plan" :daily
    #'org-x-dag-dlp-get-headline))

;;; DAG manipulation

;; add blank plans

(defmacro org-x-dag--new-plan-maybe (get-form new-form)
  (declare (indent 0))
  `(let ((it (org-x-dag->selected-date)))
     (unless ,get-form
       ,new-form)))

(defun org-x-dag--new-qtp (date)
  (org-x-dag-qtp-set-headlines date (org-x-dag-qtp-empty)))

(defun org-x-dag--new-wkp (date)
  (org-x-dag-wkp-set-headlines date (org-x-dag-wkp-empty)))

(defun org-x-dag-new-qtp ()
  (interactive)
  (org-x-dag--new-plan-maybe
    (org-x-dag-qtp-get-headline it)
    (org-x-dag--new-qtp it)))

(defun org-x-dag-new-wkp ()
  (interactive)
  (org-x-dag--new-plan-maybe
    (org-x-dag-wkp-get-week-headline it)
    (org-x-dag--new-wkp it)))

;; parent -> child linkers
;;
;; functions to set the current headline as a parent link for a child headline

(defun org-x-dag--format-link-menu-line (id title-fun)
  (declare (indent 1))
  (let* ((group (org-x-dag-id->group id))
         (s (funcall title-fun id group))
         (g (org-x-dag-group-code group)))
    (format "%s | %s" g s)))

(defun org-x-dag--format-link-menu-line-expanded (id)
  (org-x-dag--format-link-menu-line id
    (lambda (id group)
      (if (memq group '(:weekly :daily :quarterly))
          (org-x-dag-id->title id)
        (org-x-dag-id->path (eq group :action) id)))))

(defun org-x-dag--link-parent-to-child (parent-id-fun child-id-fun fmt-fun)
  (cl-flet*
      (;; (is-valid-node
       ;;  (id)
       ;;  (-some-> (org-x-dag-id->ns id)
       ;;    (either-is-right-p)))
       (to-menu-line
        (this-id id)
        (let* ((presentp (member this-id (org-x-dag-id->linked-parents id)))
               (title (funcall fmt-fun id))
               (line (format "%c %s" (if presentp ?* ?\s) title)))
          `(,line :id ,id :presentp ,presentp)))
       (choose-child-id
        (this-id)
        (-if-let (collection (->> (funcall child-id-fun)
                                  ;; (-filter #'is-valid-node)
                                  (--map (to-menu-line this-id it))))
            (-let* (((&plist :id :presentp)
                     (-> (completing-read "Child: " collection nil t)
                         (alist-get collection nil nil #'equal)))
                    ((update-fun fmt)
                     (if presentp
                         (list #'org-x-dag-headline-remove-parent-link
                               "Successfully removed '%s' from '%s'")
                       (list #'org-x-dag-headline-add-parent-link
                             "Successfully added '%s' to '%s'"))))
              (org-x-with-file (org-x-dag-id->file id)
                (goto-char (org-x-dag-id->point id))
                (org-ml-update-this-headline*
                  (funcall update-fun this-id it)))
              (->> (org-x-dag-id->title id)
                   (message fmt (org-x-dag-id->title this-id))))
          (message "No children available"))))
    (either-from* (funcall parent-id-fun)
      (message it)
      (choose-child-id it))))

(defun org-x-dag-link-parent-to-child ()
  (interactive)
  (cl-flet*
      ;; parent id functions
      ;;
      ;; TODO might make sense to check for validity here so I don't link
      ;; poisoned nodes together
      ((id-getter
        ()
        (-if-let (id (org-id-get))
            (either :right id)
          (either :left "Not on a valid node")))
       (leaf-id-getter
        ()
        (either>>= (id-getter)
          (if (org-x-dag-id->is-buffer-leaf-p it)
              (either :right it)
            (either :left "Not on a leaf node"))))
       (action-id-getter
        ()
        (either>>= (id-getter)
          (cond
           ((-some->> (org-x-dag-id->planning-datetime :scheduled it)
              (org-x-dag-datetime-split)
              (nth 1))
            (either :left "Action has scheduled time"))
           (t
            (either :right it)))))

       ;; child id functions
       (action-qtp-getter
        ()
        (->> (org-x-dag->action-ids)
             ;; TODO could also remove DONE/CANC and things
             ;; underneath these
             (--remove (org-x-dag-id->ns-key :survivalp it))
             (append (org-x-dag->current-qtp-ids))))
       (svg-action-getter
        ()
        (->> (org-x-dag->action-ids)
             ;; TODO could also remove DONE/CANC and things
             ;; underneath these
             (--remove (and (org-x-dag-id->ns-key :committed it)
                            (not (org-x-dag-id->ns-key :survivalp it))))))
       (epg-action-qtp-getter
        ()
        `(,@(org-x-dag->epg-ids) ,@(action-qtp-getter))))

    (org-x-dag-sync)
    (let ((f (buffer-file-name)))
      (cond
       ((equal f (org-x-dag->goal-file :lifetime))
        (org-x-dag--link-parent-to-child
         #'leaf-id-getter
         #'epg-action-qtp-getter
         #'org-x-dag--format-link-menu-line-expanded))
       ((equal f (org-x-dag->goal-file :endpoint))
        (org-x-dag--link-parent-to-child
         #'leaf-id-getter
         #'action-qtp-getter
         #'org-x-dag--format-link-menu-line-expanded))
       ((equal f (org-x-dag->goal-file :survival))
        (org-x-dag--link-parent-to-child
         #'leaf-id-getter
         #'svg-action-getter
         (-partial #'org-x-dag-id->path t)))
       ((member f (org-x-dag->action-files))
        (org-x-dag--link-parent-to-child
         #'action-id-getter
         #'org-x-dag->current-dlp-ids
         #'org-x-dag-id->title))
       ((equal f (org-x-dag->planning-file :quarterly))
        (org-x-dag--link-parent-to-child
         #'id-getter
         #'org-x-dag->current-wkp-ids
         #'org-x-dag-id->title))
       ((equal f (org-x-dag->planning-file :weekly))
        (org-x-dag--link-parent-to-child
         #'id-getter
         #'org-x-dag->current-dlp-ids
         #'org-x-dag-id->title))
       (t
        (message "Cannot link child from parent in current file"))))))

(defun org-x-dag-agenda-link-parent-to-child ()
  (interactive)
  (org-x-agenda-cmd-wrapper nil
    (org-x-dag-link-parent-to-child)))

;; child -> parent linkers
;;
;; functions to retrieve a parent headline id and add it to the current
;; headline's (or file's) parent links

(defun org-x-dag--link-child-to-parent (parse-fun parent-id-fun fmt-fun)
  (cl-flet*
      (;; (is-valid-node
       ;;  (id)
       ;;  (-some-> (org-x-dag-id->ns id)
       ;;    (either-is-right-p)))
       (to-menu-line
        (child-ids id)
        (let* ((presentp (member id child-ids))
               (title (funcall fmt-fun id))
               (line (format "%c %s" (if presentp ?* ?\s) title)))
          `(,line :id ,id :presentp ,presentp)))
       (update
        (add-fun rem-fun child-id-fun what node)
        (let ((child-ids (funcall child-id-fun node)))
          (-if-let (collection (->> (funcall parent-id-fun)
                                    ;; (-filter #'is-valid-node)
                                    (--map (to-menu-line child-ids it))))
              (-let* (((&plist :id :presentp)
                       (-> (completing-read "Parent: " collection nil t)
                           (alist-get collection nil nil #'equal)))
                      (title (org-x-dag-id->title id))
                      ((verb fun) (if presentp
                                      `("removed" ,rem-fun)
                                    `("added" ,add-fun))))
                (org-ml~update* nil (funcall fun id it) node)
                (message "Successfully %s '%s' from %s" verb title what))
            (message "No parents available"))))
       (update-headline
        (hl)
        (update #'org-x-dag-headline-add-parent-link
                #'org-x-dag-headline-remove-parent-link
                #'org-x-dag-headline-get-parent-links
                "current headline"
                hl))
       (update-tl-section
        (sec)
        (update #'org-x-dag-tl-section-add-parent-link
                #'org-x-dag-tl-section-remove-parent-link
                #'org-x-dag-tl-section-get-parent-links
                "toplevel section"
                sec)))
    (either-from* (funcall parse-fun)
      (message it)
      (if (org-ml-is-type 'headline it)
          (update-headline it)
        (update-tl-section it)))))

(defun org-x-dag-link-child-to-parent ()
  (interactive)
  (cl-flet*
      ((parse-hl
        ()
        ;; TODO could also test for DONE/CANC nodes since those are useless
        (-if-let (hl (org-ml-parse-this-headline))
            (if (->> (org-x-dag-headline-get-id hl)
                     (org-x-dag-id->todo))
                (either :right hl)
              (either :left "Headline is not a node"))
          (either :left "Not on headline")))
       (parse-hl-sec
        ()
        (if (org-before-first-heading-p)
            (->> (org-ml-parse-this-toplevel-section)
                 (either :right))
          (parse-hl)))

       ;; parent id getters
       (tlg-getter
        ()
        (append (org-x-dag->epg-ids) (org-x-dag->ltg-ids)))
       (goal-getter
        ()
        (append (org-x-dag->svg-ids) (tlg-getter)))
       (dlp-getter
        ()
        (append (org-x-dag->current-wkp-ids) (org-x-dag->action-ids)))
       
       ;; formatters
       (goal-formatter
        (id)
        (org-x-dag--format-link-menu-line id
          (lambda (id _)
            (org-x-dag-id->path nil id)))))

    (org-x-dag-sync)
    (let ((f (buffer-file-name)))
      (cond
       ((equal f (org-x-dag->goal-file :endpoint))
        (org-x-dag--link-child-to-parent
         #'parse-hl
         #'org-x-dag->ltg-ids
         (-partial #'org-x-dag-id->path nil)))
       ((member f (org-x-dag->action-files))
        (org-x-dag--link-child-to-parent
         #'parse-hl-sec
         #'goal-getter
         #'goal-formatter))
       ((equal f (org-x-dag->planning-file :quarterly))
        (org-x-dag--link-child-to-parent
         #'parse-hl
         #'tlg-getter
         #'goal-formatter))
       ((equal f (org-x-dag->planning-file :weekly))
        (org-x-dag--link-child-to-parent
         #'parse-hl
         #'org-x-dag->current-qtp-ids
         #'org-x-dag-id->title))
       ((equal f (org-x-dag->planning-file :daily))
        (org-x-dag--link-child-to-parent
         #'parse-hl
         #'dlp-getter
         #'org-x-dag--format-link-menu-line-expanded))
       (t
        (message "Cannot link parent from child in current file"))))))

(defun org-x-dag-agenda-link-child-to-parent ()
  (interactive)
  (org-x-agenda-cmd-wrapper nil
    (org-x-dag-link-child-to-parent)))

;; add nodes

(defun org-x-dag-read-until (read-fun pred msg)
  (declare (indent 1))
  (let (ret)
    (while (not (setq ret (funcall pred (funcall read-fun))))
      (message msg)
      (sleep-for 0.5))
    ret))

(defun org-x-dag-read-string-until (prompt pred msg)
  (declare (indent 1))
  (org-x-dag-read-until (-partial #'read-string prompt) pred msg))

(defun org-x-dag-read-datetime (date)
  (declare (indent 1))
  (let* ((re (concat "\\([0-9]\\{4\\}\\)-\\([0-1][0-9]\\)-\\([0-3][0-9]\\)"
                     " "
                     "\\([0-1][0-9]\\|2[0-3]\\):\\([0-6][0-9]\\)"))
         (pred
          (lambda (s)
            (-some->> (s-match re s)
              (cdr)
              (-map #'string-to-number))))
         (msg "Must be datetime like YYYY-MM-DD HH:MM")
         (epoch (org-x-dag-date-to-epoch date))
         (read-fun (-partial #'org-read-date nil nil nil nil epoch)))
    (org-x-dag-read-until read-fun pred msg)))

(defun org-x-dag-read-effort ()
  (declare (indent 1))
  (let* ((pred
          (lambda (s)
            (condition-case nil
                (when (org-duration-to-minutes s) s)
              (error nil))))
         (msg "Must be valid effort string")
         (allowed (org-property-get-allowed-values nil org-effort-property t))
         (read-fun (-partial #'completing-read "Effort: " allowed)))
    (org-x-dag-read-until read-fun pred msg)))

(defun org-x-dag-add-daily-metablock ()
  (interactive)
  (-let* ((title (org-x-dag-read-string-until "Metablock title: "
                   (lambda (it) (when (< 0 (length it)) it))
                   "Title cannot be blank"))
          ((date time) (->> (org-x-dag->selected-date)
                            (org-x-dag-read-datetime)
                            (org-x-dag-datetime-split)))
          (effort (org-x-dag-read-effort)))
    (org-x-dag-dlp-add-task date title nil time effort)))

;; make blank plans

;; (defun org-x-dag-qtp-new ()
;;   (interactive)
;;   (-let* ((cur-q (->> (plist-get org-x-dag :selected-date)
;;                       (org-x-dag-date-to-quarter)))
;;           ((last-plan last-q)
;;            (->> cur-q
;;                 (--unfold
;;                  (if (not it) nil
;;                    (let ((plan (org-x-dag-qtp-get it)))
;;                      `((,plan ,it) .
;;                        ,(unless (or (plist-get plan :categories)
;;                                     (plist-get plan :goals))
;;                           (org-x-dag-shift-quarter it -1 'quarter))))))
;;                 (-last-item))))
;;     (if (equal cur-q last-q)
;;         (apply #'message "Quarterly plan already initialized for %d-Q%d" cur-q)
;;       (let ((c (plist-get last-plan :categories)))
;;         (org-x-dag-qtp-set cur-q `(:categories ,c :goals nil))
;;         (apply #'message "Created new quaterly plan for %d-Q%d" cur-q)))))

;;; agenda views

;; agenda builders

(defun org-x-dag-show-nodes (get-nodes)
  (let* ((org-tags-match-list-sublevels org-tags-match-list-sublevels)
         (completion-ignore-case t))
    (catch 'exit
      ;; this should be run before `org-x-dag-sync' as it refreshes properties
      ;; like effort and statistics
      (org-agenda-prepare (concat "DAG-TAG"))
      (org-compile-prefix-format 'tags)
      (org-set-sorting-strategy 'tags)
      (org-x-dag-sync)
      (let ((org-agenda-redo-command `(org-x-dag-show-nodes ',get-nodes))
            (rtnall (funcall get-nodes org-agenda-files)))
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
  (-let ((completion-ignore-case t)
         ;; TODO not sure if this if thing is actually necessary
         ((arg start-day span with-hour) (or org-agenda-overriding-arguments
                                             (list nil start-day 'day nil))))
    (catch 'exit
      (org-agenda-prepare "DAG-DAILY")
      (org-compile-prefix-format 'agenda)
      (org-set-sorting-strategy 'agenda)
      (org-x-dag-sync)
      (-let* ((today (org-today))
              (sd (or start-day today))
              (org-agenda-redo-command
               `(org-x-dag-show-daily-nodes 'nil ,start-day ',span ,with-hour))
              ((m d y) (calendar-gregorian-from-absolute sd))
              (rtnall (org-x-dag-itemize-agenda org-agenda-files `(,y ,m ,d))))
        (setq-local org-starting-day sd)
        (setq-local org-arg-loc arg)
        ;; TODO just day (for now)
        (setq-local org-agenda-current-span span)
        (org-agenda--insert-overriding-header
          (with-temp-buffer
            (insert (format "Agenda for %d-%02d-%02d\n" y m d))
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

;; agenda helper functions

(defun org-x-dag-agenda-run-series (name files cmds)
  (declare (indent 2))
  (catch 'exit
    (let ((org-agenda-buffer-name (format "*Agenda: %s*" name)))
      ;; files are actually needed (I think) for `org-agenda-prepare' to run
      (org-agenda-run-series name `((,@cmds) ((org-agenda-files ',files)))))))

(defun org-x-dag-agenda-call-inner (buffer-name type match files settings)
  (declare (indent 4))
  (org-x-dag-agenda-run-series buffer-name files `((,type ,match ,settings))))

(defun org-x-dag-agenda-call (buffer-name header-name type match files settings)
  (declare (indent 5))
  (let* ((n (or header-name buffer-name))
         (s `((org-agenda-overriding-header ,n) ,@settings)))
    (org-x-dag-agenda-run-series buffer-name files `((,type ,match ,s)))))

(defun org-x-dag-agenda-show-nodes (buffer-name itemizer files settings)
  (declare (indent 3))
  (org-x-dag-agenda-call buffer-name nil #'org-x-dag-show-nodes
                         `(quote ,itemizer) files
    settings))

(defun org-x-dag-org-mapper-title (level1 level2 status subtitle)
  "Make an auto-mapper title.
The title will have the form 'LEVEL1.LEVEL2 STATUS (SUBTITLE)'."
  (let ((status* (->> (symbol-name status)
                   (s-chop-prefix ":")
                   (s-replace "-" " ")
                   (s-titleize))))
    (format "%s.%s %s (%s)" level1 level2 status* subtitle)))

(defmacro org-x-dag-with-advice (adlist &rest body)
  "Execute BODY with temporary advice in ADLIST.

Each element of ADLIST should be a list of the form
  (SYMBOL WHERE FUNCTION [PROPS])
suitable for passing to `advice-add'.  The BODY is wrapped in an
`unwind-protect' form, so the advice will be removed even in the
event of an error or nonlocal exit."
  (declare (debug ((&rest (&rest form)) body))
           (indent 1))
  `(progn
     ,@(mapcar (lambda (adform)
                 (cons 'advice-add adform))
               adlist)
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (adform)
                   `(advice-remove ,(car adform) ,(nth 2 adform)))
                 adlist))))

(defun org-x-dag-run-series-advice (fun name settings)
  (org-x-dag-with-advice
      ((#'org-agenda-list :override #'org-x-dag-show-daily-nodes))
    ;; TODO why the if?
    (-if-let (org-agenda-files (->> (nth 1 settings)
                                    (alist-get 'org-agenda-files)
                                    (car)
                                    (eval)))
        (funcall fun name settings)
      (funcall fun name settings))))

(defun org-x-dag-set-series-advice (on?)
  (let ((f #'org-agenda-run-series)
        (a #'org-x-dag-run-series-advice))
    (if on? (advice-add f :around a) (advice-remove f a))))

(defun org-x-dag-format-header (s)
  (org-add-props s '(face org-agenda-structure)))

;; agenda views

;; TODO these functions can't bootstrap themselves in the sense that files won't
;; be known until sync (which happens after `org-agenda-prepare'. The best
;; (maybe?) way around this is to advice `org-agenda-files' (the function) to
;; understand `org-agenda-files' (the variable) as pointing to a function which
;; references files in the dag after the dag is initialized

(defun org-x-dag-agenda-timeblock ()
  "Show the timeblock agenda view.

In the order of display
1. morning tasks (to do immediately after waking)
2. daily calendar (for thing that begin today at a specific time)
3. evening tasks (to do immediately before sleeping)"
  (interactive)
  (let ((files (cons (org-x-dag->planning-file :daily)
                     (org-x-dag->action-files))))
    (org-x-dag-agenda-call-inner "Timeblock" 'agenda "" files
      `((org-agenda-sorting-strategy '(time-up deadline-up scheduled-up category-keep))
        (org-super-agenda-groups
         `(,(nd/org-def-super-agenda-pred "Morning routine"
              (org-x-headline-has-property org-x-prop-routine
                                           org-x-prop-routine-morning)
              :order 0)
           ,(nd/org-def-super-agenda-pred "Evening routine"
              (org-x-headline-has-property org-x-prop-routine
                                           org-x-prop-routine-evening)
              :order 2)
           (:name "Calendar" :order 1 :time-grid t
                  :transformer (if (equal (get-text-property 1 'org-category it) "daily")
                                   (propertize it 'face 'org-todo)
                                 it))
           (:name "Deadlined" :order 3 :deadline t)
           (:name "Scheduled" :order 4 :scheduled t)))))))

(defun org-x-dag-agenda-goals ()
  (interactive)
  (let ((files (->> (list :lifetime :endpoint :survival)
                    (-map #'org-x-dag->goal-file))))
    (org-x-dag-agenda-show-nodes "Goals" #'org-x-dag-itemize-tl-goals files
      `((org-agenda-sorting-strategy '(user-defined-up category-keep))
        (org-super-agenda-groups
         '((:auto-map
            (lambda (line)
              (-let* ((c (get-text-property 1 'x-committedp line))
                      (f (get-text-property 1 'x-fulfilledp line))
                      (p (get-text-property 1 'x-plannedp line))
                      (l (get-text-property 1 'x-leafp line))
                      (type (get-text-property 1 'x-type line))
                      ((krank key) (pcase type
                                     (:lifetime '(0 "Lifetime"))
                                     (:endpoint '(1 "Endpoint"))))
                      ((srank subtext)
                       (cond
                        ((and (eq type :endpoint) (not c) p f)
                         '(0 "Uncommitted"))
                        ((and p f)
                         '(4 "Fulfilled | Planned"))
                        ((and (not p) f)
                         '(3 "Fulfilled | Unplanned"))
                        ((and p (not f))
                         '(2 "Unfulfilled | Planned"))
                        (t
                         '(1 "Unfulfilled | Unplanned")))))
                (format "%d.%d %s (%s)" krank srank key subtext))))))))))

(defun org-x-dag-agenda-quarterly-plan ()
  (interactive)
  (let ((files (list (org-x-dag->planning-file :quarterly)))
        (quarter-header (lambda ()
                          (-let (((y q) (->> (org-x-dag->selected-date)
                                             (org-x-dag-date-to-quarter))))
                            (-> (format "Quarter %d - %d\n" q y)
                                (org-x-dag-format-header))))))
    (org-x-dag-agenda-show-nodes "Quarterly Plan" #'org-x-dag-itemize-qtp files
      `((org-agenda-sorting-strategy '(user-defined-up category-keep))
        (org-agenda-overriding-header ',quarter-header)
        (org-super-agenda-groups
         '((:auto-map
            (lambda (line)
              (-let* ((c (get-text-property 1 'x-committedp line))
                      (p (get-text-property 1 'x-plannedp line))
                      ((rank text)
                       (cond
                        ((and p c)
                         '(4 "Committed | Planned"))
                        ((and (not p) c)
                         '(3 "Committed | Unplanned"))
                        ((and p (not c))
                         '(2 "Uncommitted | Planned"))
                        (t
                         '(1 "Unfulfilled | Unplanned")))))
                (format "%d. %s" rank text))))))))))

(defun org-x-dag-agenda-weekly-plan ()
  (interactive)
  (let ((files (list (org-x-dag->planning-file :weekly)))
        (weekly-header (lambda ()
                         (-let* (((date &as y m d) (org-x-dag->selected-date))
                                 (n (org-x-dag-date-to-week-number date)))
                           (-> (format "Week %d - %d-%d-%d\n" n y m d)
                               (org-x-dag-format-header))))))
    (org-x-dag-agenda-show-nodes "Weekly Plan" #'org-x-dag-itemize-wkp files
      `((org-agenda-sorting-strategy '(user-defined-up category-keep))
        (org-agenda-overriding-header ',weekly-header)
        (org-super-agenda-groups
         '((:auto-map
            (lambda (line)
              (-let* ((c (get-text-property 1 'x-committedp line))
                      (p (get-text-property 1 'x-plannedp line))
                      (day (get-text-property 1 'x-day line))
                      (n (car (rassoc day org-x-dag-weekly-tags)))
                      ((rank text)
                       (cond
                        ((and p c)
                         '(4 "Committed | Planned"))
                        ((and (not p) c)
                         '(3 "Committed | Unplanned"))
                        ((and p (not c))
                         '(2 "Uncommitted | Planned"))
                        (t
                         '(1 "Unfulfilled | Unplanned")))))
                (format "%d.%d %s (%s)" n rank day text))))))))))

(defun org-x-dag-agenda-tasks ()
  "Show the tasks agenda view.

Distinguish between independent and project tasks, as well as
tasks that are inert (which I may move to the incubator during a
review phase)"
  (interactive)
  (let ((files (org-x-dag->action-files)))
    (org-x-dag-agenda-show-nodes "Tasks" #'org-x-dag-itemize-tasks files
      `((org-agenda-sorting-strategy '(user-defined-up category-keep))
        (org-super-agenda-groups
         '((:auto-map
            (lambda (line)
              (-let* ((i (get-text-property 1 'x-is-standalone line))
                      (s (get-text-property 1 'x-status line))
                      (s* (if (and (not i) (eq s :inert)) :active s))
                      ((level1 subtitle) (if i '(1 "α") '(0 "σ")))
                      (p (alist-get s* nd/org-headline-task-status-priorities)))
                (org-x-dag-org-mapper-title level1 p s* subtitle))))))))))

(defun org-x-dag-agenda-projects ()
  "Show the projects agenda view."
  (interactive)
  (let ((files (org-x-dag->action-files)))
    (org-x-dag-agenda-show-nodes "Projects" #'org-x-dag-itemize-projects files
      `((org-agenda-sorting-strategy '(category-keep))
        (org-super-agenda-groups
         '((:auto-map
            (lambda (line)
              (-let* ((i (get-text-property 1 'x-toplevelp line))
                      (s (get-text-property 1 'x-status line))
                      (p (get-text-property 1 'x-priority line))
                      ((level1 subtitle) (if i '(0 "τ") '(1 "σ"))))
                (org-x-dag-org-mapper-title level1 p s subtitle))))))))))

(defun org-x-dag-agenda-incubator ()
  "Show the incubator agenda view."
  (interactive)
  (let ((files (org-x-dag->action-files)))
    (org-x-dag-agenda-show-nodes "Incubator" #'org-x-dag-itemize-incubated files
      `((org-agenda-sorting-strategy '(category-keep))
        (org-super-agenda-groups
         '((:auto-map
            (lambda (line)
              (let ((p (get-text-property 1 'x-project-p line))
                    (s (get-text-property 1 'x-scheduled line))
                    (d (get-text-property 1 'x-deadlined line)))
                (cond
                 ((and s (not p))
                  (if (< (float-time) s) "Future Scheduled" "Past Scheduled"))
                 ((and d (not p))
                  (if (< (float-time) d) "Future Deadline" "Past Deadline"))
                 (p "Toplevel Projects")
                 (t "Standalone Tasks")))))))))))

(defun org-x-dag-agenda-iterators ()
  "Show the iterator agenda view."
  (interactive)
  (let ((files (org-x-dag->action-files)))
    (org-x-dag-agenda-show-nodes "Iterators-0" #'org-x-dag-itemize-iterators files
      `((org-agenda-sorting-strategy '(category-keep))
        (org-super-agenda-groups
         ',(nd/org-def-super-agenda-automap
             (cl-case (org-x-headline-get-iterator-status)
               (:uninit "0. Uninitialized")
               (:project-error "0. Project Error")
               (:unscheduled "0. Unscheduled")
               (:empt "1. Empty")
               (:actv "2. Active")
               (t "3. Other"))))))))

(defun org-x-dag-agenda-errors ()
  "Show the critical errors agenda view."
  (interactive)
  (let ((files (org-x-dag->action-files)))
    (org-x-dag-agenda-show-nodes "Errors" #'org-x-dag-itemize-errors files
      `((org-super-agenda-groups
         '((:auto-map
            (lambda (line)
              (get-text-property 1 'x-error line)))))))))

(defun org-x-dag-agenda-archive ()
  "Show the archive agenda view."
  (interactive)
  (let ((files (org-x-dag->action-files)))
    (org-x-dag-agenda-show-nodes "Archive-0" #'org-x-dag-itemize-archived files
    `((org-agenda-sorting-strategy '(category-keep))
      (org-super-agenda-groups
         '((:auto-map
            (lambda (line)
              (cl-case (get-text-property 1 'x-type line)
                (:proj "Toplevel Projects")
                (:task "Standalone Tasks")
                (:iter "Closed Iterators")
                (:subiter "Toplevel Subiterators"))))))))))

;; ;; TODO the tags in the far column are redundant
;; (defun org-x-dag-agenda-quarterly-plan ()
;;   (interactive)
;;   (let ((match #'org-x-dag-scan-quarterly-plan)
;;         (files (org-x-dag->action-files))
;;         (header (->> (org-x-dag->current-date)
;;                      (org-x-dag-date-to-quarter)
;;                      (apply #'format "Quarterly Plan: %d Q%d"))))
;;     (org-x-dag-agenda-show-nodes "Quarterly Plan" match files
;;       `((org-agenda-overriding-header ,header)
;;         (org-agenda-sorting-strategy '(user-defined-up category-keep))
;;         ;; TODO add allocation (somehow)
;;         (org-agenda-prefix-format '((tags . "  ")))
;;         (org-super-agenda-groups
;;          '((:auto-map
;;             (lambda (line)
;;               (let ((bucket (car (get-text-property 1 'tags line))))
;;                 (--> (-map #'cdr org-x-life-categories)
;;                      (--find (equal (plist-get it :tag) bucket) it)
;;                      (plist-get it :desc)))))))))))

;; (defun org-x-dag-agenda-weekly-plan ()
;;   (interactive)
;;   (let* ((match #'org-x-dag-scan-weekly-plan)
;;          (files (org-x-dag->action-files))
;;          (date (org-x-dag->current-date))
;;          (header (->> (org-x-dag-date-to-week-number date)
;;                       (format "Weekly Plan: %d W%02d" (car date)))))
;;     (org-x-dag-agenda-show-nodes "Weekly Plan" match files
;;       `((org-agenda-overriding-header ,header)
;;         (org-agenda-sorting-strategy '(user-defined-up category-keep))
;;         (org-agenda-prefix-format '((tags . "  ")))
;;         (org-super-agenda-groups
;;          '((:auto-map
;;             (lambda (line)
;;               (get-text-property 1 'x-day-of-week line)))))))))

(defun org-x-dag-agenda-tasks-by-goal ()
  (interactive)
  (let ((match #'org-x-dag-itemize-tasks-with-goals)
        (files (org-x-dag->action-files)))
    (org-x-dag-agenda-show-nodes "Tasks by Goal" match files
      `((org-agenda-sorting-strategy '(user-defined-up category-keep))
        (org-super-agenda-groups
         '((:auto-map
            (lambda (line)
              (-if-let (i (get-text-property 1 'x-goal-id line))
                  (->> (org-x-dag-id->title i)
                       (substring-no-properties))
                "0. Unlinked")))))))))

;; (defun org-x-dag-agenda-survival-tasks ()
;;   (interactive)
;;   (let ((match #'org-x-dag-scan-survival-tasks)
;;         (files (org-x-dag->action-files)))
;;     (org-x-dag-agenda-show-nodes "Survival Tasks" match files
;;       `((org-agenda-sorting-strategy '(user-defined-up category-keep))
;;         (org-super-agenda-groups
;;          '((:auto-map
;;             (lambda (line)
;;               (-if-let (i (get-text-property 1 'x-goal-id line))
;;                   (->> (org-x-dag-id->title i)
;;                        (substring-no-properties))
;;                 "0. Unlinked")))))))))

;; TODO this is just toplevel projects (for now)
;; TODO wetter than Seattle
(defun org-x-dag-agenda-projects-by-goal ()
  (interactive)
  (let ((match #'org-x-dag-itemize-projects-with-goals)
        (files (org-x-dag->action-files)))
    (org-x-dag-agenda-show-nodes "Projects by Goal" match files
      `((org-agenda-sorting-strategy '(user-defined-up category-keep))
        (org-super-agenda-groups
         '((:auto-map
            (lambda (line)
              (-if-let (i (get-text-property 1 'x-goal-id line))
                  (->> (org-x-dag-id->title i)
                       (substring-no-properties))
                "0. Unlinked")))))))))

;; ;; TODO this is just toplevel projects (for now)
;; ;; TODO wetter than Seattle
;; (defun org-x-dag-agenda-survival-projects ()
;;   (interactive)
;;   (let ((match ''org-x-dag-scan-survival-projects)
;;         (files (org-x-get-action-files)))
;;     (org-x-dag-agenda-call "Survival Projects" nil #'org-x-dag-show-nodes match files
;;       `((org-agenda-todo-ignore-with-date t)
;;         (org-agenda-sorting-strategy '(user-defined-up category-keep))
;;         (org-super-agenda-groups
;;          '((:auto-map
;;             (lambda (line)
;;               (-if-let (i (get-text-property 1 'x-goal-id line))
;;                   (->> (org-x-dag-id->title i)
;;                        (substring-no-properties))
;;                 "0. Unlinked")))))))))

;; (defun org-x-dag-agenda-goals ()
;;   (interactive)
;;   (let ((match #'org-x-dag-scan-goals))
;;     (org-x-dag-agenda-show-nodes "Goals-0" match nil
;;       `((org-agenda-sorting-strategy '(user-defined-up category-keep))
;;         (org-super-agenda-groups
;;          '((:auto-map
;;             (lambda (line)
;;               (-let* (((&plist :type y
;;                                :local-children lc
;;                                :action-children ac
;;                                :invalid-children ic
;;                                :goal-parents gp
;;                                :invalid-parents ip)
;;                        (get-text-property 1 'x-goal-status line))
;;                       (type (cl-case y
;;                               (:endpoint "0. Endpoint")
;;                               (:lifetime "1. Lifetime")
;;                               (:survival "2. Survival")))
;;                       (subtext (cl-case y
;;                                  (:endpoint
;;                                   (cond
;;                                    (ip "Invalid parent links")
;;                                    ((not gp) "Missing toplevel goal")
;;                                    (ic "Invalid child links")
;;                                    ((and (not lc) (not ac) "Missing action"))
;;                                    ((and lc (not ac)) "Branch")))
;;                                  ((:lifetime :survival)
;;                                   (cond
;;                                    (ic "Invalid child links")
;;                                    ((and (not lc) (not ac) "Missing goal/action"))
;;                                    ((and lc (not ac)) "Branch"))))))
;;                 (if subtext (format "%s (%s)" type subtext) type))))))))))

(defun org-x-dag-agenda-incubated ()
  (interactive)
  (let ((match #'org-x-dag-itemize-incubated)
        (files (org-x-dag->action-files)))
    (org-x-dag-agenda-show-nodes "Incubated-0" match files
      `((org-agenda-sorting-strategy '(user-defined-up category-keep))
        (org-super-agenda-groups
         '((:auto-map
            (lambda (line)
              (-let* ((type (get-text-property 1 'x-type line))
                      (toplevelp (get-text-property 1 'x-toplevelp line))
                      (survivalp (get-text-property 1 'x-survivalp line))
                      (committedp (get-text-property 1 'x-committedp line))
                      ((rank type)
                       (pcase type
                         (:task
                          (if toplevelp '(1 "Standalone Task")
                            '(2 "Task")))
                         (:proj
                          (if toplevelp '(3 "Toplevel Project")
                            '(4 "Project")))
                         (:iter
                          '(5 "Iterator"))
                         (:subiter
                          (if toplevelp '(6 "Parent Subiterator")
                            '(7 "Subiterator")))))
                      ((srank stype) (cond
                                      ((and committedp survivalp)
                                       '(1 "Survival"))
                                      (committedp
                                       '(2 "Non-Survival"))
                                      (t
                                       '(3 "Uncommitted")))))
                (format "%d.%d %s (%s)" srank rank type stype))))))))))

(provide 'org-x-dag)
;;; org-x-dag.el ends here
