;;; org-x.el --- Extra Org Commands -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Nathan Dwarshuis

;; Author: Nathan Dwarshuis <natedwarshuis@gmail.com>
;; Keywords: org-mode, outlines
;; Homepage: https://github.com/ndwarshuis/org-x
;; Package-Requires: ((emacs "27.2") (dash "2.18"))
;; Version: 0.0.1

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

;; XXX

;;; Code:

(require 'org-ml)
(require 'dash)
(require 's)
(require 'org)

;; constants

(defconst org-x-iter-future-time (* 7 24 60 60)
  "Iterators must have at least one task greater into the future to be active.")
  
;; TODO ;unscheduled should trump all
(defconst org-x-iter-statuscodes '(:uninit :empt :actv :project-error :unscheduled)
  "Iterators can have these statuscodes.")
 
(defconst org-x-peri-future-time org-x-iter-future-time
  "Periodicals must have at least one heading greater into the future to be fresh.")

(defconst org-x-peri-statuscodes '(:uninit :empt :actv :unscheduled))

(defconst org-x-archive-delay 30
  "The number of days to wait before tasks are considered archivable.")

(defconst org-x-inert-delay-days 90
  "The number of days to wait before tasks are considered inert.")

(defconst org-x-project-invalid-todostates
  '("WAIT" "NEXT")
  "Projects cannot have these todostates.")
  
(defconst org-x-agenda-todo-sort-order
  '("NEXT" "WAIT" "HOLD" "TODO")
  "Defines the order in which todo keywords should be sorted.")
  
(defconst org-x-project-skip-todostates
  '("HOLD" "CANC")
  "These keywords override all contents within their subtrees.
Currently used to tell skip functions when they can hop over
entire subtrees to save time and ignore tasks")

(defvar org-x-cluster-filter-files t
  "Set to t if files should be filtered in org-cluster.
This option does nothing unless `org-x-cluster-filtered-files' is
also non-nil.")

(defvar org-x-cluster-filtered-files nil
  "Files that should be excluded from org-cluster analysis.
These are pattern-matched so they do not need to be exact names
or paths.")

(defvar org-x-cluster-filter-todo t
  "Set to t if todo keywords should be filtered in org-cluster.
This option does nothing unless `org-x-cluster-filtered-todo' is
also non-nil.")

(defvar org-x-cluster-filtered-todo nil
  "TODO keywords that should be filtered from org-cluster analysis.")

(defvar org-x-cluster-filter-past t
  "Set to t to exclude files from before now in org-cluster analysis.")

(defvar org-x-cluster-filter-habit nil
  "Set to t to exclude habits from org-cluster analysis.")

;; internal vars

(defvar org-x-agenda-hide-incubator-tags t
  "If true, don't show incubator headings.")

(defvar org-x-agenda-hasprop-filter nil)

;; list

(defun org-x-filter-list-prefix (prefix str-list)
  "Return a subset of STR-LIST whose first characters are PREFIX."
  (--filter (and (stringp it) (s-prefix-p prefix it)) str-list))

;; org-element

(defun org-x-element-first-lb-entry (headline)
  "Return epoch time of most recent logbook item or clock from HEADLINE."
  (let* ((config (list :log-into-drawer org-log-into-drawer
                       :clock-into-drawer org-clock-into-drawer
                       :clock-out-notes org-log-note-clock-out))
         (logbook (->> (org-ml-headline-get-supercontents config headline)
                       (org-ml-supercontents-get-logbook)))
         (first-item-ut (-some->> (org-ml-logbook-get-items logbook)
                          (car)
                          ;; TODO this function should be public
                          (org-ml-logbook-item-get-timestamp)))
         (first-clock-ut (-some->> (org-ml-logbook-get-clocks logbook)
                           (car)
                           (org-ml-get-property :value)
                           (org-ml-timestamp-get-end-time)
                           (org-ml-time-to-unixtime))))
    (cond
     ((and first-item-ut first-clock-ut (< first-item-ut first-clock-ut))
      first-clock-ut)
     ((and first-item-ut first-clock-ut (> first-item-ut first-clock-ut))
      first-item-ut)
     (first-item-ut first-item-ut)
     (first-clock-ut first-clock-ut))))

;; timestamp processing

(defun org-x-get-date-property (timestamp-property)
  "Get TIMESTAMP-PROPERTY on current heading and convert to a number.
If it does not have a date, it will return nil."
  (-some-> (org-entry-get nil timestamp-property) (org-2ft)))

;; TODO I don't like this function...it perplexes me
(defun org-x-heading-compare-timestamp (timestamp-fun &optional ref-time future)
  "Compare timestamp to some reference time.

TIMESTAMP-FUN is a function that returns a timestamp when called
on the headline in question. Return t if the returned timestamp
is further back in time compared to REF-TIME (default to 0 which
is now, where negative is past and positive is future). If the
FUTURE flag is t, returns timestamp if it is in the future
compared to REF-TIME. Returns nil if no timestamp is found."
  (let* ((timestamp (funcall timestamp-fun))
        (ref-time (or ref-time 0)))
    (if (and timestamp
             (if future
                 (> (- timestamp (float-time)) ref-time)
               (<= (- timestamp (float-time)) ref-time)))
        timestamp)))

(defun org-x-is-timestamped-heading-p ()
  "Get active timestamp of current heading."
  (org-x-get-date-property "TIMESTAMP"))

(defun org-x-is-scheduled-heading-p ()
  "Get scheduled timestamp of current heading."
  (org-x-get-date-property "SCHEDULED"))

(defun org-x-is-deadlined-heading-p ()
  "Get scheduled timestamp of current heading."
  (org-x-get-date-property "DEADLINE"))

(defun org-x-is-created-heading-p ()
  "Get scheduled timestamp of current heading."
  (org-x-get-date-property "CREATED"))

(defun org-x-is-closed-heading-p ()
  "Get closed timestamp of current heading."
  (org-x-get-date-property "CLOSED"))

(defun org-x-is-stale-heading-p (&optional ts-prop)
  "Return timestamp for TS-PROP (TIMESTAMP by default) if current heading is stale."
  (org-x-heading-compare-timestamp
   (lambda () (let ((ts (org-entry-get nil (or ts-prop "TIMESTAMP"))))
           (when (and ts (not (cl-find ?+ ts))) (org-2ft ts))))))

(defun org-x-is-expired-date-headline-p ()
  "Return timestamp if current headline is expired via \"X-EXPIRE\"."
  (org-x-heading-compare-timestamp
   (lambda () (-some-> (org-entry-get nil "X-EXPIRE")
           (org-2ft)))))

(defun org-x-is-expired-dtl-headline-p ()
  "Return timestamp if current headline is expired via \"X-DAYS_TO_LIVE\"."
  (org-x-heading-compare-timestamp
   (lambda () (let ((dtl (org-entry-get nil "X-DAYS_TO_LIVE"))
               (created (org-entry-get nil "CREATED")))
           (when (and dtl (s-matches-p "[0-9]+" dtl) created)
             (+ (org-2ft created)
                (* (string-to-number dtl) 24 60 60)))))))

(defun org-x-is-expired-headline-p ()
  "Return t if current headline is expired."
  ;; NOTE: this will return the dtl ft even if the date ft is less
  (and (or (org-x-is-expired-dtl-headline-p)
           (org-x-is-expired-date-headline-p))
       t))

(defun org-x-is-fresh-heading-p ()
  "Return timestamp if current heading is fresh."
  (org-x-heading-compare-timestamp 'org-x-is-timestamped-heading-p nil t))

(defun org-x-is-archivable-heading-p ()
  "Return timestamp if current heading is archivable."
  (org-x-heading-compare-timestamp
   'org-x-is-closed-heading-p
    (- (* 60 60 24 org-x-archive-delay))))

(defun org-x-is-inert-p ()
  "Return most recent timestamp if headline is inert."
  (let* ((now (float-time))
         (hl (org-ml-parse-this-headline))
         (most-recent-log-ut (-some->> hl (org-x-element-first-lb-entry)))
         (planning (org-ml-headline-get-planning hl))
         (scheduled-ut (-some->> planning
                         (org-ml-get-property :scheduled)
                         (org-ml-timestamp-get-start-time)
                         (org-ml-time-to-unixtime)))
         (deadline-ut (-some->> planning
                        (org-ml-get-property :deadline)
                        (org-ml-timestamp-get-start-time)
                        (org-ml-time-to-unixtime)))
         (created-ut (-some->> (org-ml-headline-get-node-property "CREATED" hl)
                       (org-2ft))))
    ;; not inert if headline is scheduled or deadlined in the future
    (unless (or (-some->> scheduled-ut (- now) (> 0))
                (-some->> deadline-ut (- now) (> 0)))
      (-some--> (or most-recent-log-ut created-ut)
                (- now it)
                (when (> it (* 86400 org-x-inert-delay-days)) it)))))

;; task-level testing

(defun org-x-is-todoitem-p ()
  "Return todo keyword if heading has one."
  (-some-> (org-get-todo-state) (substring-no-properties)))

(defun org-x-is-project-p ()
  "Return todo keyword if heading has todoitem children."
  (and (org-x-headline-has-children 'org-x-is-todoitem-p) (org-x-is-todoitem-p)))

(defun org-x-is-task-p ()
  "Return todo keyword if heading has no todoitem children."
  (and (not (org-x-headline-has-children 'org-x-is-todoitem-p)) (org-x-is-todoitem-p)))

(defun org-x-is-project-task-p ()
  "Return todo keyword if heading has todoitem parents."
  (and (org-x-headline-has-parent 'org-x-is-todoitem-p) (org-x-is-task-p)))

(defun org-x-is-atomic-task-p ()
  "Return todo keyword if heading has no todoitem parents or children."
  (and (not (org-x-headline-has-parent 'org-x-is-todoitem-p)) (org-x-is-task-p)))

(defun org-x-task-status ()
  "Return the status of the headline under point."
  (-when-let (kw (org-x-is-task-p))
    (cond
     ((org-x-is-archivable-heading-p)
      :archivable)
     ((and (not (member kw org-done-keywords)) (org-x-is-expired-headline-p))
      :expired)
     ((org-x-is-inert-p)
      :inert)
     ((and (member kw org-done-keywords) (not (org-x-is-closed-heading-p)))
      :done-unclosed)
     ((and (not (member kw org-done-keywords)) (org-x-is-closed-heading-p))
      :undone-closed)
     ((member kw org-done-keywords)
      :complete)
     (t :active))))

;; property testing

(defun org-x-is-periodical-heading-p ()
  "Return t if heading is a periodical."
  (equal "periodical" (org-entry-get nil "PARENT_TYPE" t)))

(defun org-x-is-iterator-heading-p ()
  "Return t if heading is an iterator."
  (equal "iterator" (org-entry-get nil "PARENT_TYPE" t)))

(defun org-x-is-habit-heading-p ()
  "Return t if heading is an iterator."
  (equal "habit" (org-entry-get nil "STYLE" t)))

(defun org-x-headline-has-effort-p ()
  "Return t if heading has an effort."
  (org-entry-get nil "Effort"))

(defun org-x-headline-has-context-p ()
  "Return t if heading has a context."
  (let ((tags (org-get-tags)))
    (or (> (length (org-x-filter-list-prefix "#" tags)) 0)
        (> (length (org-x-filter-list-prefix "@" tags)) 0))))

(defun org-x-headline-has-tag-p (tag)
  "Return t if heading has tag TAG."
  (member tag (org-get-tags)))

;; relational testing

;; TODO this function seems slow
(defun org-x-headline-has-children (heading-test)
  "Return t if heading has a child for whom HEADING-TEST is t."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        has-children previous-point)
    (save-excursion
      (setq previous-point (point))
      (outline-next-heading)
      (while (and (not has-children)
                  (< previous-point (point) subtree-end))
        (when (funcall heading-test)
          (setq has-children t))
        (setq previous-point (point))
        (org-forward-heading-same-level 1 t)))
    has-children))

;; (defun org-x-headline-has-children (test-fun)
;;   "Return t if heading has a child for whom TEST-FUN is t."
;;   ;; assume that point is at the beginning of a headline
;;   (let* ((level (1+ (org-current-level)))
;;          (has-children nil)
;;          (cur-level level))
;;     ;; skip over the current headline
;;     (re-search-forward org-outline-regexp-bol nil t)
;;     (save-excursion
;;       (while (and (<= level cur-level)
;;                   (re-search-forward org-outline-regexp-bol nil t))
;;         ;; it's actually more efficient to scan every headline and check its
;;         ;; level rather than using a regexp to match the target depth
;;         (setq cur-level (- (match-end 0) (match-beginning 0) 1))
;;         (when (and (= cur-level level) (funcall test-fun))
;;           (setq has-children t))))
;;     has-children))

(defun org-x-headline-has-parent (heading-test)
  "Return t if heading has parent for whom HEADING-TEST is t."
  (save-excursion (and (org-up-heading-safe) (funcall heading-test))))

(defun org-x-has-discontinuous-parent ()
  "Return t if heading has a non-todoitem parent which in turn has a todoitem parent."
  (let ((has-todoitem-parent)
        (has-non-todoitem-parent))
    (save-excursion
      (while (and (org-up-heading-safe)
                  (not has-todoitem-parent))
        (if (org-x-is-todoitem-p)
            (setq has-todoitem-parent t)
          (setq has-non-todoitem-parent t))))
    (and has-todoitem-parent has-non-todoitem-parent)))

(defun org-x-is-todo-child (keyword)
  "Return t if current headline has a parent (at any level) with todo KEYWORD."
  (let ((has-keyword-parent))
    (save-excursion
      (while (and (org-up-heading-safe) (not has-keyword-parent))
        (when (equal keyword (org-x-is-todoitem-p))
          (setq has-keyword-parent t))))
    has-keyword-parent))

;; project level testing

(defmacro org-x-compare-statuscodes (op sc1 sc2 sc-list)
  "Compare position of statuscodes SC1 and SC2 in SC-LIST using operator OP."
  `(,op (cl-position ,sc1 ,sc-list) (cl-position ,sc2 ,sc-list)))

;; TODO there is likely a better way to handle this
(defmacro org-x-descend-into-project (statuscode-tree get-task-status callback-fun)
  "Loop through (sub)project and return overall statuscode.

The returned statuscode is chosen from list ALLOWED-STATUSCODES where
later entries in the list trump earlier ones.

When a subproject is encountered, this function will obtain the
statuscode of that project and use TRANS-TBL to translate the
subproject statuscode to one in ALLOWED-STATUSCODES (if not found an
error will be raised). TRANS-TBL is given as an alist of two-member
cons cells where the first member is the subproject statuscode and the
 second is the index in ALLOWED-STATUSCODES to which the subproject
statuscode will be translated.

When a task is encountered, function GET-TASK-STATUS will be applied to
obtain a statuscode-equivalent of the status of the tasks.

CALLBACK-FUN is a function to call once this is finished (which
should be this function again)."
  ;; define "breaker-status" as the last of the allowed-statuscodes
  ;; when this is encountered the loop is broken because we are done
  ;; (the last entry trumps all others)
  (let* ((allowed-statuscodes (-map #'car statuscode-tree))
         (trans-tbl (->> statuscode-tree
                      (--map (-let (((a . bs) it)) (--map (cons it a) bs)))
                      (-flatten-n 1)))
         (breaker-status (-last-item allowed-statuscodes))
         (initial-status (car allowed-statuscodes)))
    `(let ((project-status ,initial-status)
           (previous-point nil)
           (new-status nil)
           (it-kw nil))
       (save-excursion
         (setq previous-point (point))
         (outline-next-heading)
         ;; loop through subproject tasks until breaker-status found
         (while (and (not (eq project-status ,breaker-status))
                     (> (point) previous-point))
           (setq it-kw (org-get-todo-state))
           (when it-kw
             (if (org-x-headline-has-children #'org-x-is-todoitem-p)
                 (progn
                   ;; If project returns an allowed status then use that.
                   ;; Otherwise look up the value in the translation table and
                   ;; return error if not found.
                   (setq new-status (,callback-fun))
                   (unless (member new-status ',allowed-statuscodes)
                     (setq new-status (alist-get new-status ',trans-tbl))))
               ;; if tasks then use get-task-status to obtain status
               (setq new-status (nth ,get-task-status ',allowed-statuscodes)))
             (when (org-x-compare-statuscodes > new-status project-status ',allowed-statuscodes)
               (setq project-status new-status)))
           (setq previous-point (point))
           (org-forward-heading-same-level 1 t)))
       project-status)))

(defun org-x-get-project-status ()
  "Return project heading statuscode (assumes it is indeed a project)."
  ;;
  ;; these first three are easy because they only require
  ;; testing the project headline and nothing underneath
  ;;
  ;; it does not make sense for projects to be scheduled
  (if (org-x-is-scheduled-heading-p) :scheduled-project
    (-when-let (keyword (org-get-todo-state))
      (cond
       ;; held projects do not care what is underneath them
       ;; only need to test if they are inert
       ((equal keyword "HOLD") (if (org-x-is-inert-p) :inert :held))

       ;; projects with invalid todostates are nonsense
       ((member keyword org-x-project-invalid-todostates)
        :invalid-todostate)

       ;; canceled projects can either be archivable or complete
       ;; any errors or undone tasks are irrelevant
       ((equal keyword "CANC") (if (org-x-is-archivable-heading-p) :archivable
                                 :complete))
       
       ;;
       ;; these require descending into the project subtasks
       ;;

       ;; done projects are like canceled projects but can also be incomplete
       ((equal keyword "DONE")
        (org-x-descend-into-project
         ((:archivable)
          (:complete)
          (:done-incomplete :stuck :inert :held :wait :active
                            :scheduled-project :invalid-todostate
                            :undone-complete))
         (if (member it-kw org-done-keywords)
             (if (org-x-is-archivable-heading-p) 0 1)
           2)
         org-x-get-project-status))
       
       ;; project with TODO states could be basically any status
       ((equal keyword "TODO")
        (org-x-descend-into-project
         ((:undone-complete :complete :archivable)
          (:stuck :scheduled-project :invalid-todostate :done-incomplete)
          (:held)
          (:wait)
          (:inert)
          (:active))
         (cond
          ((and (not (member it-kw org-done-keywords)) (org-x-is-inert-p)) 4)
          ((equal it-kw "TODO") (if (org-x-is-scheduled-heading-p) 5 1))
          ((equal it-kw "HOLD") 2)
          ((equal it-kw "WAIT") 3)
          ((equal it-kw "NEXT") 5)
          (t 0))
         org-x-get-project-status))
       
       (t (error (concat "invalid keyword detected: " keyword)))))))

(defun org-x--clone-get-iterator-project-status (kw)
  (cond
   ((or (org-x-is-scheduled-heading-p)
        (member kw org-x-project-invalid-todostates)) :project-error)

   ;; canceled tasks add nothing
   ((equal kw "CANC") :empt)
   
   ;;
   ;; these require descending into the project subtasks
   ;;

   ;; done projects either add nothing (empty) or are not actually
   ;; done (project error)
   ((equal kw "DONE")
    (org-x-descend-into-project
     ((:empt)
      (:project-error :unscheduled :actv))
     (if (member it-kw org-done-keywords) 0 1)
     org-x--clone-get-iterator-project-status))
   
   ;; project with TODO states could be basically any status
   ((equal kw "TODO")
    (org-x-descend-into-project
     ((:unscheduled :project-error)
      (:empt)
      (:actv))
     (let ((ts (org-x-is-scheduled-heading-p)))
       (cond
        ((not ts) 0)
        ((> org-x-iter-future-time (- ts (float-time))) 1)
        (t 2)))
     org-x--clone-get-iterator-project-status))
   
   (t (error (concat "invalid keyword detected: " kw)))))

(defun org-x-get-iterator-status ()
  "Get the status of an iterator.
Allowed statuscodes are in list `nd/get-iter-statuscodes.' where
 latter codes in the list trump earlier ones."
  (let ((cur-status (first org-x-iter-statuscodes))
        (breaker-status (-last-item org-x-iter-statuscodes))
        (subtree-end (save-excursion (org-end-of-subtree t)))
        (prev-point (point))
        (kw nil)
        (new-status nil)
        (ts nil))
    (save-excursion
      (outline-next-heading)
      (while (and (not (eq cur-status breaker-status))
                  (< prev-point (point) subtree-end))
        (setq kw (org-x-is-todoitem-p)
              new-status nil)
        (when kw
          ;; test if project of atomic task
          ;; assume that there are no todoitems above this headline
          ;; to make checking easier
          (setq
           new-status
           (if (org-x-headline-has-children 'org-x-is-todoitem-p)
               (org-x--clone-get-iterator-project-status kw)
             (setq ts (or (org-x-is-scheduled-heading-p)
                          (org-x-is-deadlined-heading-p)))
             (cond
              ((member kw org-done-keywords) :empt)
              ((not ts) :unscheduled)
              ((< org-x-iter-future-time (- ts (float-time))) :actv)
              (t :empt))))
          (when (org-x-compare-statuscodes > new-status cur-status org-x-iter-statuscodes)
            (setq cur-status new-status)))
        (setq prev-point (point))
        (org-forward-heading-same-level 1 t)))
    cur-status))
    
(defun org-x-get-periodical-status ()
  "Get the status of a periodical.
Allowed statuscodes are in list `nd/get-peri-statuscodes.' where
latter codes in the list trump earlier ones."
  (cl-flet
      ((get-ts
        ()
        (-some->> (org-ml-parse-this-headline)
          (org-ml-headline-get-contents (list :log-into-drawer org-log-into-drawer
                                              :clock-into-drawer org-clock-into-drawer))
          (org-ml-match '(:first :any * (:and timestamp
                                              (:or (:type 'active)
                                                   (:type 'active-range)))))
          (car)
          (org-ml-timestamp-get-start-time)
          (org-ml-time-to-unixtime)))
       (new-status
        (cur-status ts)
        (let ((new (cond
                    ((not ts) :unscheduled)
                    ((< org-x-peri-future-time (- ts (float-time))) :actv)
                    (t :empt))))
          (if (org-x-compare-statuscodes > new cur-status org-x-peri-statuscodes)
              new
            cur-status))))
    (let ((cur-status (first org-x-peri-statuscodes))
          (breaker-status (-last-item org-x-peri-statuscodes))
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (prev-point (point)))
      (save-excursion
        (outline-next-heading)
        (while (and (not (eq cur-status breaker-status))
                    (< prev-point (point) subtree-end))
          (setq cur-status (->> (get-ts) (new-status cur-status)))
          (setq prev-point (point))
          (org-forward-heading-same-level 1 t)))
      cur-status)))

;; skip functions

(defun org-x-skip-heading ()
  "Skip forward to next heading."
  (save-excursion (or (outline-next-heading) (point-max))))

(defun org-x-skip-subtree ()
  "Skip forward to next subtree."
  (save-excursion (or (org-end-of-subtree t) (point-max))))

(defun org-x-skip-children ()
  "Skip to the end of all subheadings on the current subheading level.
This implies that the current heading has a parent. If it doesn't, this
function will simply return the point of the next headline."
  (save-excursion
    (if (org-up-heading-safe)
        (org-x-skip-subtree)
      (org-x-skip-heading))))

(defun org-x-skip-headings-with-tags (pos-tags-list &optional neg-tags-list)
  "Skip headings that have tags in POS-TAGS-LIST and not in NEG-TAGS-LIST."
  (save-restriction
    (widen)
    (let ((heading-tags (org-get-tags)))
      (if (and (or (not pos-tags-list)
                   (cl-intersection pos-tags-list heading-tags :test 'equal))
               (not (cl-intersection neg-tags-list heading-tags :test 'equal)))
          (org-x-skip-heading)))))

;; interactive functions

(defun org-x-mark-subtree-keyword (new-keyword &optional exclude no-log)
  "Change the todo keyword of all tasks in a subtree to NEW-KEYWORD.
If EXCLUDE is given, it should be a list of todo keywords; any headline
matching a keyword in this list will not be changed. If NO-LOG is t,
don't log changes in the logbook."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (org-todo-log-states (unless no-log org-todo-log-states)))
    (if (not (listp exclude))
        (error "Exclude must be a list if provided"))
    (save-excursion
      (while (< (point) subtree-end)
        (let ((keyword (org-x-is-todoitem-p)))
          (if (and keyword (not (member keyword exclude)))
              (org-todo new-keyword)))
        (outline-next-heading)))))

(defun org-x-mark-subtree-done ()
  "Mark all tasks in subtree as DONE unless they are already CANC."
  (interactive)
  (org-x-mark-subtree-keyword "DONE" '("CANC")))

(defun org-x--reset-headline (headline)
  "Reset HEADLINE node to incomplete state.
This includes unchecking all checkboxes, marking keywords as
\"TODO\", clearing any unique IDs, etc."
  (cl-flet*
      ((reset
        (config created-ts headline)
        (->> (if (org-ml-headline-is-done headline)
                 (org-ml-set-property :todo-keyword "TODO" headline)
               headline)
             (org-ml-headline-map-supercontents* config
               (org-ml-supercontents-set-logbook nil it))
             (org-ml-headline-set-node-property "CREATED" created-ts)
             (org-ml-headline-map-planning*
               (if (not it) it
                 (org-ml-planning-set-timestamp! :closed nil it)))
             (org-ml-headline-set-node-property "ID" nil)
             ;; this obviously will be wrong if I ever want to use TODO
             ;; statistics but at least they will be reset to zero
             (org-ml-headline-update-item-statistics))))
    (let ((config (list :log-into-drawer org-log-into-drawer
                        :clock-into-drawer org-clock-into-drawer
                        :clock-out-notes org-log-note-clock-out))
          (created-ts (-> (float-time)
                          (org-ml-unixtime-to-time-long)
                          (org-ml-build-timestamp!)
                          (org-ml-to-string))))
      (->> (reset config created-ts headline)
           (org-ml-match-map* '(:any * item)
             (org-ml-set-property :checkbox 'off it))
           (org-ml-match-map* '(:any * headline)
             (reset config created-ts it))))))

(defun org-x--headline-repeat-shifted (n shift headline)
  "Return HEADLINE repeated and shifted N times.
SHIFT is a string specifier denoting the amount to shift, eg
\"+2d\"."
  (cl-flet
      ((convert-shift
        (shift)
        (-let* (((n unit)
                 (or (-some->>
                         shift
                       (s-match "\\(\\([-+]?[0-9]+\\)\\([ymwd]\\)\\)")
                       (cddr))
                     (error "Invalid shift specified: %s" shift)))
                ((unit mult)
                 (cl-case (intern unit)
                   (y '(year 1))
                   (m '(month 1))
                   (w '(day 7))
                   (d '(day 1))
                   (t (error "This shouldn't happen: %s" unit))))
                (n (* mult (string-to-number n))))
          (list n unit)))
       (shift-timestamps
        (T unit mult headline)
        (let ((T* (* T mult)))
          (->> headline
               (org-ml-match-map* '(:any * timestamp)
                 (org-ml-timestamp-shift T* unit it))
               (org-ml-match-map* '(:any * planning)
                 (->> it
                      (org-ml-map-property* :scheduled
                        (when it (org-ml-timestamp-shift T* unit it)))
                      (org-ml-map-property* :deadline
                        (when it (org-ml-timestamp-shift T* unit it)))))))))
    (let ((headlines (org-ml-clone-node-n n headline)))
      (if (equal "" shift) headlines
        (-let (((T unit) (convert-shift shift)))
          (--map-indexed (shift-timestamps T unit (1+ it-index) it) headlines))))))

;; TODO make "CREATED" timestamp reflect when these things are cloned
(defun org-x-clone-subtree-with-time-shift (n)
  "Like `org-clone-subtree-with-time-shift' except reset items and todos.
N is the number of clones to produce."
  (interactive "nNumber of clones to produce: ")
  (let* ((st (org-ml-parse-this-subtree))
         (shift
          (or (org-entry-get nil "TIME_SHIFT" 'selective)
              (read-from-minibuffer
               "Date shift per clone (e.g. +1w, empty to copy unchanged): ")))
         (ins (->> (org-x--reset-headline st)
                   (org-x--headline-repeat-shifted n shift)
                   (-map #'org-ml-to-string)
                   (s-join "")))
         (end (org-ml-get-property :end st)))
    (org-ml-insert end ins)))

(defun org-x-clone-subtree-with-time-shift-toplevel (n)
  "Like `org-clone-subtree-with-time-shift' except reset items and todos.
N is the number of clones to produce."
  (interactive "nNumber of clones to produce: ")
  (cl-flet
      ((get-shift
        (subtree)
        (or
         (org-ml-headline-get-node-property "TIME_SHIFT" subtree)
         (read-from-minibuffer
          "Shift per clone (e.g. +1w, empty to copy unchanged): "))))
    (org-ml-update-this-subtree*
      (let ((shift (get-shift it))
            (new (->> (org-ml-headline-get-subheadlines it)
                      (-last-item)
                      (org-x--reset-headline))))
        (org-ml-map-children*
         (append it (org-x--headline-repeat-shifted n shift new))
         it)))
    (let ((post (org-ml-parse-this-subtree)))
      (org-ml-match-do '(section property-drawer) (lambda (it) (org-ml-fold it)) post)
      (org-ml-match-do '(headline) (lambda (it) (org-ml-fold it)) post))))

(defun org-x-log-delete ()
  "Delete logbook drawer of subtree."
  (interactive)
  (save-excursion
    (goto-char (org-log-beginning))
    (when (save-excursion
            (save-match-data
              (beginning-of-line 0)
              (search-forward-regexp org-drawer-regexp)
              (goto-char (match-beginning 1))
              (looking-at "LOGBOOK")))
      (org-mark-element)
      (delete-region (region-beginning) (region-end))
      (org-remove-empty-drawer-at (point)))))

(defun org-x-delete-subtree ()
  "Delete the entire subtree under the current heading without sending to kill ring."
  (interactive)
  (org-back-to-heading t)
  (delete-region (point) (+ 1 (save-excursion (org-end-of-subtree)))))

(defun org-x-clock-range (&optional arg)
  "Add a completed clock entry to the current heading.
Does not touch the running clock. When called with one prefix
ARG, ask for a range in minutes in place of the second date."
  (interactive "P")
  (let* ((t1 (-> (org-read-date t t) (float-time)))
         (t2 (if (equal arg '(4))
                   (-some-> (read-string "Length in minutes: ")
                            (cl-parse-integer :junk-allowed t)
                            (* 60)
                            (+ t1))
                 (-> (org-read-date t t nil nil t1)
                     (float-time)
                     (round)))))
    (cond
     ((not t2) (message "Invalid range given!"))
     ((< t2 t1) (message "Second timestamp earlier than first!"))
     (t
      (let ((s (org-ml-unixtime-to-time-long t1))
            (e (org-ml-unixtime-to-time-long t2)))
        (save-excursion
          (org-clock-find-position nil)
	      (org-indent-line)
          (->> (org-ml-build-clock! s :end e)
               (org-ml-to-string)
               (insert))))))))

(defmacro org-x-agenda-cmd-wrapper (get-head &rest body)
  "Execute BODY in context of agenda buffer.
Specifically, navigate to the original header, execute BODY, then
update the agenda buffer. If GET-HEAD is true, get the headline
string and use it to update the agenda (this is only needed when
the headline changes obviously)."
  `(progn
     (org-agenda-check-no-diary)
     (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                          (org-agenda-error)))
            (buffer (marker-buffer hdmarker))
            (pos (marker-position hdmarker))
            (inhibit-read-only t)
            newhead)
       (org-with-remote-undo buffer
         (with-current-buffer buffer
           (widen)
           (goto-char pos)
           (org-show-context 'agenda)
           ,@body
           (when ,get-head (setq newhead (org-get-heading))))
         (if ,get-head
             (org-agenda-change-all-lines newhead hdmarker)
           (org-agenda-redo))
	     (beginning-of-line 1)))))
  
(defun org-x-agenda-toggle-checkbox ()
  "Toggle checkboxes in org agenda view using `org-toggle-checkbox'."
  (interactive)
  (org-x-agenda-cmd-wrapper
   t
   (call-interactively #'org-toggle-checkbox)))

(defun org-x-agenda-clone-subtree-with-time-shift ()
  "Apply `org-x-clone-subtree-with-time-shift' to an agenda entry.
It will clone the last entry in the selected subtree."
  (interactive)
  (org-x-agenda-cmd-wrapper
   nil
   (call-interactively #'org-x-clone-subtree-with-time-shift-toplevel)))

(defun org-x-agenda-delete-subtree ()
  "Apply `org-x-delete-subtree' to an agenda entry."
  (interactive)
  (org-x-agenda-cmd-wrapper
   nil
   (call-interactively #'org-x-delete-subtree)))

(defun org-x-agenda-clock-range ()
  "Apply `org-x-clock-range' to an agenda entry."
  (interactive)
  (org-x-agenda-cmd-wrapper
   nil
   (call-interactively #'org-x-clock-range)))

(defun org-x-agenda-filter-non-context ()
  "Filter all tasks with context tags."
  (interactive)
  (let* ((tags-list (mapcar #'car org-tag-alist))
         (context-tags (append
                        (org-x-filter-list-prefix "@" tags-list)
                        (org-x-filter-list-prefix "#" tags-list))))
    (setq org-agenda-tag-filter
          (mapcar (lambda (tag) (concat "-" tag)) context-tags))
    (org-agenda-filter-apply org-agenda-tag-filter 'tag)))

(defun org-x-agenda-filter-non-peripheral ()
  "Filter all tasks that don't have peripheral tags."
  (interactive)
  (let* ((peripheral-tags '("PERIPHERAL")))
    (setq org-agenda-tag-filter
          (mapcar (lambda (tag) (concat "-" tag)) peripheral-tags))
    (org-agenda-filter-apply org-agenda-tag-filter 'tag)))
    
(defun org-x-agenda-filter-non-effort ()
  "Filter agenda by non-effort tasks."
  (interactive)
  (setq org-x-agenda-hasprop-filter '("-Effort"))
  (org-agenda-filter-apply org-x-agenda-hasprop-filter 'hasprop))

(defun org-x-agenda-filter-delegate ()
  "Filter agenda by tasks with an external delegate."
  (interactive)
  (setq org-x-agenda-hasprop-filter '("+DELEGATE"))
  (org-agenda-filter-apply org-x-agenda-hasprop-filter 'hasprop))

(defun org-x-agenda--seek-heading (&optional back)
  "Go to next or previous agenda heading.
If BACK is t seek backward, else forward. Ignore blank lines."
  (let* ((limit (if back (point-min) (point-max)))
         (inc (if back -1 1))
         (is-valid-header
          (lambda ()
            (let ((h (buffer-substring (line-beginning-position)
                                       (line-end-position))))
              (and
               (not (equal h ""))
               (get-text-property 0 'org-agenda-structural-header h)))))
         (next
          (let ((header-point))
            (save-excursion
              (while (and (< 0 (abs (- limit (point))))
                          (not header-point))
                (forward-line inc)
                (when (funcall is-valid-header)
                  (setq header-point (point))))
              header-point))))
    (if next (goto-char next)
      (message (if back "Cannot move up" "Cannot move down")))))

(defun org-x-agenda-previous-heading ()
  "Go to the previous agenda heading or end of buffer."
  (interactive)
  (org-x-agenda--seek-heading t))

(defun org-x-agenda-next-heading ()
  "Go to the next agenda heading or end of buffer."
  (interactive)
  (org-x-agenda--seek-heading))

(defun org-x-time-shift ()
  "Shift all scheduled and deadlined timestamps in the current subtree."
  (interactive)
  (save-excursion
    (org-back-to-heading) ;; trigger error here if not at heading
    (let* ((end (save-excursion (org-end-of-subtree)))
           (shift
            (-->
             (read-from-minibuffer "Date shift (e.g. +1w): ")
             (s-match "\\`[ \t]*\\([\\+\\-]?[0-9]+\\)\\([MHdwmy]\\)[ \t]*\\'" it)
             (if (not it) (error "Invalid shift: %s" it) it)))
           (mag (string-to-number (nth 1 shift)))
           (unit
            (pcase (nth 2 shift)
              ("M" 'minute)
              ("H" (setq mag (* mag 60)) 'minute)
              ("d" 'day)
              ("w" (setq mag (* mag 7)) 'day)
              ("m" 'month)
              ("y" 'year)
              (_ (error "Unsupported time unit"))))
           (shift-ts-maybe
            (lambda (type)
              (let ((bound (save-excursion (outline-next-heading))))
                (save-excursion
                  (when (re-search-forward (org-re-timestamp type) bound t)
                    (org-timestamp-change mag unit)))))))
      (while (< (point) end)
        (funcall shift-ts-maybe 'scheduled)
        (funcall shift-ts-maybe 'deadline)
        (outline-next-heading)))))

;; In order to implement the =hasprop= filter, the functions
;; =org-agenda-filter-make-matcher= and =org-agenda-filter-remove-all= need to
;; be advised in order to add the functionality for the =hasprop= filter type.

;; As it is, this allows any filter using =hasprop= to be applied and removed
;; using the standard =org-agenda-filter-apply= function with the
;; =org-x-agenda-hasprop-filter= variable (obviously these can all be extended
;; to different filter types). Note this does not give a shiny indicator at the
;; bottom of spaceline like the built-in filter does...oh well.

(defun org-x-agenda-filter-make-matcher-prop (filter type &rest _args)
  "Override the standard match filter.
This will return matching matcher form for FILTER and TYPE
where TYPE is not in the regular `org-agenda-filter-make-matcher'
function. This is intended to be uses as :before-until advice and
will return nil if the type is not valid (which is currently
'prop')"
  (let (f)
    ;; has property
    (cond
     ((eq type 'hasprop)
      (dolist (x filter)
        (push (org-x-agenda-filter-make-matcher-hasprop-exp x) f))))
    (if f (cons 'and (nreverse f)))))

(defun org-x-agenda-filter-make-matcher-hasprop-exp (h)
 "Return form to test the presence or absence of properties H.
H is a string like +prop or -prop"
 (let* ((op (string-to-char h))
        (h (substring h 1))
        (f `(save-excursion
              (let ((m (org-get-at-bol 'org-hd-marker)))
                (with-current-buffer
                    (marker-buffer m)
                  (goto-char m)
                  (org-entry-get nil ,h))))))
   (if (eq op ?-) (list 'not f) f)))

(defun org-x-agenda-filter-show-all-hasprop ()
  "Remove the 'hasprop filter."
  (org-agenda-remove-filter 'hasprop))

(advice-add #'org-agenda-filter-make-matcher :before-until
            #'org-x-agenda-filter-make-matcher-prop)

(advice-add #'org-agenda-filter-remove-all :before
            (lambda () (when org-x-agenda-hasprop-filter
                    (org-x-agenda-filter-show-all-hasprop))))

;; advice

;; The =org-tags-view= can filter tags for only headings with TODO keywords
;; (with type tags-todo), but this automatically excludes keywords in
;; =org-done-keywords=. Therefore, if I want to include these in any custom
;; agenda blocks, I need to use type tags instead and skip the unwanted TODO
;; keywords with a skip function. This is far slower as it applies the skip
;; function to EVERY heading. Fix that here by nullifying
;; =org--matcher-tags-todo-only= which controls how the matcher is created for
;; tags and tags-todo. Now I can select done keywords using a match string like
;; "+tag/DONE|CANC" (also much clearer in my opinion). While this is usually
;; more efficient, it may be counterproductive in cases where skip functions can
;; be used to ignore huge sections of an org file (which is rarely for me; most
;; only skip ahead to the next heading).

(defun org-x-tags-view-advice (orig-fn &rest args)
  "Include done states in `org-tags-view' for tags-todo agenda types.
This is meant to be used as :around advice, where ORIG-FN is the
original function being advised and ARGS are the arguments."
  (nd/with-advice
      ((#'org-make-tags-matcher
        :around (lambda (f m)
                  (let ((org--matcher-tags-todo-only nil))
                    (funcall f m)))))
    (apply orig-fn args)))

(advice-add #'org-tags-view :around #'org-x-tags-view-advice)

(defun org-x-set-creation-time (&optional _always &rest _args)
  "Set the creation time property of the current heading."
  (let ((np (->> (float-time)
                 (org-ml-unixtime-to-time-long)
                 (org-ml-build-timestamp!)
                 (org-ml-to-string)
                 (org-ml-build-node-property "CREATED"))))
    (org-ml-update-this-headline*
      (org-ml-headline-map-node-properties* (cons np it) it))))

(defun org-x-set-expired-time (&optional arg)
  "Set the expired time of the current headline.
If ARG is non-nil use long timestamp format."
  (interactive "P")
  (-when-let (ut (-some->> (org-read-date nil t)
                   (float-time)
                   (round)))
    (let ((np (->> (if arg (org-ml-unixtime-to-time-long ut)
                     (org-ml-unixtime-to-time-short ut))
                (org-ml-build-timestamp!)
                (org-ml-to-string)
                (org-ml-build-node-property "X-EXPIRE"))))
      (org-ml-update-this-headline*
        (org-ml-headline-map-node-properties* (cons np it) it)))))

(defun org-x-set-dtl ()
  "Set days-to-live of the current headline."
  (interactive)
  (let ((n (read-string "Days to live: ")))
    (if (not (s-matches-p "[0-9]+" n))
        (message "Enter a number")
      (let ((np (org-ml-build-node-property "X-DAYS_TO_LIVE" n)))
        (org-ml-update-this-headline*
          (org-ml-headline-map-node-properties* (cons np it) it))))))

(advice-add 'org-insert-heading :after #'org-x-set-creation-time)

(add-hook 'org-capture-before-finalize-hook #'org-x-set-creation-time)

(defun org-x-refile-logbook ()
  "Refile the current headline with it's logbook.
The original target headline is left in place but without the
logbook. Intended use is for habits and repeating tasks that
build up massive logbook entries that will make my org files huge
and slow."
  (interactive)
  (let ((acat (org-get-category))
        (atime (format-time-string (substring (cdr org-time-stamp-formats) 1 -1)))
        (afile (abbreviate-file-name
                (or (buffer-file-name (buffer-base-buffer))
                    (error "No file associated to buffer"))))
        (apath (s-join "/" (org-get-outline-path)))
        (atags (->> (org-get-tags)
                    (--filter (get-text-property 0 'inherited it))
                    (s-join " ")))
        (config (list :log-into-drawer org-log-into-drawer
                      :clock-into-drawer org-clock-into-drawer)))
    ;; TODO this is basically a function version of org-archive and could
    ;; be refactored/reused as a separate function
    (cl-flet
        ((archive
          (atime afile _apath acat atodo atags target headline)
          (let* ((level-shift (-some-> (org-ml-get-property :level headline)
                                (1-)
                                (-)))
                 (headline*
                  (->> (org-ml-clone-node headline)
                       (org-ml-set-property :todo-keyword "DONE")
                       (org-ml-headline-map-planning*
                         (let ((time (->> (float-time)
                                          (org-ml-unixtime-to-time-long))))
                           (org-ml-planning-set-timestamp! :closed time it)))
                       (org-ml-headline-set-node-property "ARCHIVE_TIME" atime)
                       (org-ml-headline-set-node-property "ARCHIVE_FILE" afile)
                       (org-ml-headline-set-node-property "ARCHIVE_OLPATH" afile)
                       (org-ml-headline-set-node-property "ARCHIVE_CATEGORY" acat)
                       (org-ml-headline-set-node-property "ARCHIVE_TODO" atodo)
                       (org-ml-headline-set-node-property "ARCHIVE_ITAGS" atags)
                       (org-ml-shift-property :level level-shift)
                       (org-ml-match-map* '(:any * headline)
                         (org-ml-shift-property :level level-shift it)))))
            ;; TODO this currently does not refile under specific headlines
            (with-current-buffer (find-file-noselect target)
              (org-ml-insert (point-max) headline*)))))
      (org-ml-update-this-subtree*
        (let ((atodo (org-ml-get-property :todo-keyword it))
              (target (format "%s_archive" afile)))
          (archive atime afile apath acat atodo atags target it)
          (org-ml-headline-map-supercontents* config
            (org-ml-supercontents-set-logbook nil it)
            it))))))

;; inter-headline clustering
;;
;; Conflicts and overloads begin with the same list to process, which is created
;; using `org-element-parse-buffer' and a variety of filtering functions to
;; extract relevent timestamps.

(defun org-x-cluster-make-tsp (start-time range offset fp)
  "Construct a timestamp plist to be used in further processing.

The fields are as follows:

- START-TIME is a list from `org-ml-timestamp-get-start-time'
- RANGE is the duration of the timestamp (could be 0)
- OFFSET is the character offset of the timestamp in its file
- FP the path to the file in which the timestamp resides"
  (list :start-time start-time
        :range range
        :offset offset
        :filepath fp))
        
(defun org-x-cluster-ts-hard-p (ts)
  "Return non-nil if the timestamp TS has hours/minutes."
  (org-element-property :hour-start ts))
  
(defun org-x-cluster-parse-ts (hl fp ts)
  "Parse a timestamp TS belonging to headline HL and filepath FP.
TS is an object as described in the org-element API. Only active
or active-range types are considered. Returns a new timestamp-plist
for TS."
  (when ts
    (let ((offset (org-element-property :begin hl))
          (start-time (org-ml-timestamp-get-start-time ts))
          (range (org-ml-timestamp-get-range ts)))
      (org-x-cluster-make-tsp start-time range offset fp))))
        
(defun org-x-cluster-effort-seconds (effort-str)
  "Convert EFFORT-STR into an integer in seconds from HH:MM format."
  (-some->> effort-str (org-duration-to-minutes) (round) (* 60)))

(defun org-x-cluster-extract-hl-sched (hl fp)
  "Extract scheduled timestamp from headline HL in filepath FP.
Create a new timestamp-plist and add to accumulator ACC."
  (-when-let (ts (-some->> (org-ml-headline-get-planning hl)
                   (org-ml-get-property :scheduled)))
    (let* ((effort-raw (org-ml-headline-get-node-property "Effort" hl))
           (effort (if effort-raw (org-x-cluster-effort-seconds effort-raw) 0))
           (offset (org-ml-get-property :begin hl))
           (start-time (org-ml-timestamp-get-start-time ts)))
      (org-x-cluster-make-tsp start-time effort offset fp))))

(defun org-x-cluster-extract-hl-ts (hl fp)
  "Extract timestamps from headline HL in filepath FP.
All active timestamps that are not in drawers or the planning header
are considered. Each timestamp is converted into a new timestamp-plist
and added to accumulator ACC."
  (-some->> hl
    (org-ml-headline-get-contents
     (list :log-into-drawer org-log-into-drawer
           :clock-into-drawer org-clock-into-drawer
           :clock-out-notes org-log-note-clock-out))
    (apply #'org-ml-build-section)
    (org-ml-match '(:first :any * (:and timestamp
                                        (:or (:type 'active)
                                             (:type 'active-range)))))
    (car)
    (org-x-cluster-parse-ts hl fp)))

(defun org-x-cluster-extract-hl (hl fp)
  "Extract timestamps from headline HL in filepath FP and store in ACC."
  (if (org-ml-get-property :todo-keyword hl)
      (org-x-cluster-extract-hl-sched hl fp)
    (org-x-cluster-extract-hl-ts hl fp)))

(defun org-x-cluster-filter-todo (hls)
  "Filter certain TODO keywords from headline list HLS."
  (if (not org-x-cluster-filter-todo) hls
    (--remove (member (org-element-property :todo-keyword it)
                      org-x-cluster-filtered-todo)
              hls)))

(defun org-x-cluster-filter-files (fps)
  "Filter certain file names from files list FPS."
  (if (not org-x-cluster-filter-files) fps
    (--remove
     (-find (lambda (s) (string-match-p s it)) org-x-cluster-filtered-files)
     fps)))

(defun org-x-cluster-filter-past (tsps)
  "Filter out timestamp-plists in list TSPS if they start in the past."
  (if (not org-x-cluster-filter-past) tsps
    (let ((ft (float-time)))
      (--remove (< (plist-get it :unixtime) ft) tsps))))

(defun org-x-cluster-filter-habit (hls)
  "Filter headlines from headline list HLS that are habits."
  (if (not org-x-cluster-filter-habit) hls
    (--remove (org-element-property :STYLE it) hls)))

(defun org-x-cluster-extract-buffer (fp)
  "Extract headlines from the current buffer for clustering analysis.
FP is the filepath to the current buffer."
  (->> (org-ml-parse-headlines 'all)
    (org-x-cluster-filter-todo)
    (org-x-cluster-filter-habit)
    (--map (org-x-cluster-extract-hl it fp))
    (-non-nil)))

(defun org-x-cluster-extract-file (fp)
  "Extract timestamps from filepath FP and add to accumulator ACC."
  (with-current-buffer (find-file-noselect fp t)
    (org-x-cluster-extract-buffer fp)))

(defun org-x-cluster-append-unixtime (tsps)
  "Append a :unixtime property to TSPS.
The new property will contain an integer representing the unix
time of the :start-time property."
  (--map (append (list :unixtime (org-ml-time-to-unixtime (plist-get it :start-time))) it) tsps))
  
(defun org-x-cluster-get-unprocessed ()
  "Return a list of timestamp-plists with desired filter settings."
  (->> (org-agenda-files)
    (org-x-cluster-filter-files)
    (-mapcat #'org-x-cluster-extract-file)))

;; get conflict headlines
;;
;; This algorithm builds a list of pairs, with each pair being a two tasks that
;; conflict and should be O(n) (best case/no conflicts) to O(n^2) (worst
;; case/everything conflicts).
;;
;; Steps for this:
;; 1. make a list of all entries containing timestamps (active and scheduled)
;; 2. sort timestamp list
;; 3. Walk through list and compare entries immediately after (sorting ensures
;;    that entries can be skipped once one non-conflict is found). If conflicts
;;    are found push the pair to new list.

(defun org-x-cluster-conflicting-p (tsp-a tsp-b)
  "Return t if timestamps TSP-A and TSP-B conflict."
  ;; assume that ts-a starts before ts-b
  (let ((start-a (plist-get tsp-a :unixtime))
        (start-b (plist-get tsp-b :unixtime)))
    (or (= start-a start-b) (< start-b (+ start-a (plist-get tsp-a :range))))))

(defun org-x-cluster-find-conflict (tsp tsps conlist)
  "Test if timestamp-plist TSP conflicts with any in TSPS.
If found, anything in TSPS is cons'd with TSP and added to CONLIST
as a pair. New CONLIST is returned."
  (->> (--take-while (org-x-cluster-conflicting-p tsp it) tsps)
    (--map (list tsp it))
    (append conlist)))
  
(defun org-x-cluster-build-conlist (tsps)
  "Build a list of conflict pairs from timestamp-plist TSPS."
  (let ((conlist))
    (while (< 1 (length tsps))
      (setq conlist (org-x-cluster-find-conflict (car tsps) (cdr tsps) conlist)
            tsps (cdr tsps)))
    conlist))

(defun org-x-cluster-group-conflicts (tsps)
  "Return TSPS that conflict with each other.
The returned list will be a list of pairs of TSPs like (TSP-a TSP-b) which
are two TSPs that conflict."
  (->> (--filter (org-ml-time-is-long (plist-get it :start-time)) tsps)
    (org-x-cluster-append-unixtime)
    (org-x-cluster-filter-past)
    (--sort (< (plist-get it :unixtime) (plist-get other :unixtime)))
    (org-x-cluster-build-conlist)))

;; get overloaded days
;;
;; Overloads are defined as days that have more than 24 hours worth of scheduled
;; material. The algorithm is O(n) as it is basically just a bunch of filtering
;; functions that walk through the list.
;;
;; Steps for the algorithm:
;; 1. filter only ranged entries (unranged entries have zero time)
;; 2. maybe split timestamps if they span multiple days
;; 3. sort from earliest to latest starting time
;; 4. sum the range of timestamps in each day, keeping those that exceed 24 hours

(defun org-x-cluster-split-tsp-maybe (tsp)
  "Split TSP if it spans multiple days."
  ;; NOTE: `encode-time' seems pretty slow but this is necessary since some
  ;; barbarians in power insist on keep daylight savings time, which means I
  ;; can't just do straight modular arithmetic to find where each day boundary
  ;; lies.
  (cl-flet
      ((encode-float-time
        (time)
        (round (float-time (encode-time time)))))
    (-let (((&plist :start-time :range :offset :filepath) tsp))
      (if (= range 0) (list tsp)
        ;; `encode-time' and `decode-time' might not use the right time zone
        ;; unless specified manually
        (-let* ((tz (current-time-zone))
                (start-time* (if (org-ml-time-is-long start-time) start-time
                               `(,@(-take 3 start-time) 0 0)))
                ((y m d H M) start-time*)
                (start-epoch (encode-float-time `(0 ,M ,H ,d ,m ,y nil nil ,tz)))
                (end-epoch (+ start-epoch range))
                (next t)
                (split-epoch nil)
                ((M* H* d* m* y*) '(nil nil nil nil nil))
                (acc nil))
          (while next
            ;; get the projected ending time
            (-setq (M* H* d* m* y*) (-take 5 (cdr (decode-time end-epoch tz))))
            ;; Get the epoch time on which to split. If not on a day boundary,
            ;; calculate the epoch time of the most recent day boundary. If on a
            ;; day boundary, split on the boundary one full day earlier by
            ;; decrementing day by one
            (when (and (= 0 M*) (= 0 H*))
              (setq d* (1- d*)))
            (setq split-epoch (encode-float-time `(0 0 0 ,d* ,m* ,y* nil nil ,tz)))
            ;; If the split-epoch is less than or equal to the start, loop is
            ;; done. Else add a new entry and reset the projected ending time to
            ;; the current split time; rinse and repeat.
            (if (< start-epoch split-epoch)
                (setq acc (cons (org-x-cluster-make-tsp
                                 `(,y* ,m* ,d* 0 0) (- end-epoch split-epoch)
                                 offset filepath)
                                acc)
                      end-epoch split-epoch)
              (setq next nil
                    acc (cons (org-x-cluster-make-tsp
                               start-time* (- end-epoch start-epoch) offset filepath)
                              acc))))
          acc)))))

(defun org-x-cluster-daily-split (tsps)
  "Group timestamp-plist TSPS into sublists for each day."
  (--partition-by (-take 3 (plist-get it :start-time)) tsps))
                          
(defun org-x-cluster-overloaded-p (tsps)
  "Return t if total time of timestamp-plists in TSPS exceeds 24 hours.
It is assumed the TSPS represents tasks and appointments within one
day."
  (<= 86400 (-sum (--map (plist-get it :range) tsps))))

(defun org-x-cluster-group-overloads (tsps)
  "Group TSPS by overloaded day.
A day is overloaded if it has TSPs whose :range properties sum to
greater than 24 hours. TSPs which span multiple days will be
split along day boundaries according to local time zone before
grouping is performed. Returned list will be a list of lists
like (TSP1 TSP2 ...) which are TSPs in a single day that is
overloaded."
  (->> tsps
    (--filter (< 0 (plist-get it :range)))
    (-mapcat #'org-x-cluster-split-tsp-maybe)
    (org-x-cluster-append-unixtime)
    (org-x-cluster-filter-past)
    (--sort (< (plist-get it :unixtime) (plist-get other :unixtime)))
    (org-x-cluster-daily-split)
    (--filter (org-x-cluster-overloaded-p it))))

;; conflict/overload frontend

;; I could just fetch the org headings and throw them into a new buffer. But
;; that's boring, and quite limiting. I basically want all the perks of an
;; agenda buffer...tab-follow, the nice parent display at the bottom, time
;; adjust hotkeys, etc. So the obvious and hacky solution is to throw together a
;; quick-n-dirty agenda buffer.

(defun org-x-cluster-headline-text (tsp)
  "Return string for headline text represented by TSP.
Returned string will have text properties to enable wizzy, fun
things in the agenda like jumpy to the target headline from the
agenda buffer."
  (-let* (((&plist :offset :filepath) tsp)
         (ts-marker (with-current-buffer (find-file-noselect filepath)
                      (copy-marker offset)))
         (props (list 'face nil
                      'done-face 'org-agenda-done
                      'org-not-done-regexp org-not-done-regexp
                      'org-todo-regexp org-todo-regexp
                      'org-complex-heading-regexp org-complex-heading-regexp
                      'mouse-face 'highlight))
	     marker priority category level tags todo-state
	     ts-date ts-date-type ts-date-pair
	     txt beg end inherited-tags todo-state-end-pos)

    (with-current-buffer (marker-buffer ts-marker)
      (save-excursion
	    (goto-char ts-marker)

	    (setq marker (org-agenda-new-marker (point))
	          category (org-get-category)
	          ts-date-pair (org-agenda-entry-get-agenda-timestamp (point))
	          ts-date (car ts-date-pair)
	          ts-date-type (cdr ts-date-pair)
	          txt (org-get-heading t)
	          inherited-tags
	          (or (eq org-agenda-show-inherited-tags 'always)
		          (and (listp org-agenda-show-inherited-tags)
		               (memq 'todo org-agenda-show-inherited-tags))
		          (and (eq org-agenda-show-inherited-tags t)
		               (or (eq org-agenda-use-tag-inheritance t)
			               (memq 'todo org-agenda-use-tag-inheritance))))
	          tags (org-get-tags-at nil (not inherited-tags))
	          level (make-string (org-reduced-level (org-outline-level)) ? )
	          txt (org-agenda-format-item "" txt level category tags t)
	          priority (1+ (org-get-priority txt)))
        
	    (org-add-props txt props
	      'org-marker marker
          'org-hd-marker marker
	      'priority priority
	      'level level
	      'ts-date ts-date
	      'type "timestamp")))))

(defun org-x-cluster-ts-fmt (tsp)
  "Return formatted day-level timestamp for TSP."
  (format-time-string "[%Y-%m-%d]" (plist-get tsp :unixtime)))

(defun org-x-cluster-format-conflict (grouped-tsp)
  "Return GROUPED-TSPs formatted for conflict agenda buffer."
  (format "On %s\n%s\n"
          (org-x-cluster-ts-fmt (car grouped-tsp))
          (mapconcat #'org-x-cluster-headline-text grouped-tsp "\n")))

(defun org-x-cluster-format-overload (grouped-tsp)
  "Return GROUPED-TSPs formatted for overload agenda buffer."
  (format "On %s\n%s\n"
          (org-x-cluster-ts-fmt (car grouped-tsp))
          (mapconcat #'org-x-cluster-headline-text grouped-tsp "\n")))

(defun org-x-cluster-show-agenda (short-name title cluster-fun format-fun arg)
  "Show an inter-headline cluster agenda buffer.
SHORT-NAME is a one-word name describing the buffer which will be
used in the name of the buffer. TITLE will be displayed at the
top of the buffer. CLUSTER-FUN is a function that takes a list of
TSPs and returned a grouped list of TSPs. FORMAT-FUN is a
function that takes one member from the list provided by
CLUSTER-FUN and returns a string with text properties to be
inserted into the agenda buffer. ARG is an argument provided by some
calling interactive function."
  (when org-agenda-overriding-arguments
    (setq arg org-agenda-overriding-arguments))

  (when (and (stringp arg) (not (string-match "\\S-" arg)))
    (setq arg nil))

  (let* ((today (org-today))
	     (date (calendar-gregorian-from-absolute today))
	     (completion-ignore-case t)
         (org-agenda-prefix-format '((agenda . "  %-12:c %-5:e ")))
	     rtn rtnall files file pos)

    (catch 'exit
      (when org-agenda-sticky
        (setq org-agenda-buffer-name (format "*Org %s*" short-name)))

      (org-agenda-prepare)
      (org-compile-prefix-format 'agenda)

      (setq org-agenda-redo-command '(org-x-cluster-show-overloads))

	  (insert (format "%s: \n" title))
      (add-text-properties (point-min) (1- (point))
			               (list 'face 'org-agenda-structure
				                 'short-heading short-name))
	  (org-agenda-mark-header-line (point-min))

      (-some-> (funcall cluster-fun (org-x-cluster-get-unprocessed))
        (--each (insert (funcall format-fun it))))

      ;; clean up and finalize
      (goto-char (point-min))
      (or org-agenda-multi (org-agenda-fit-window-to-buffer))
      (add-text-properties
       (point-min) (point-max)
	   `(org-agenda-type agenda
					     org-last-args ,arg
					     org-redo-cmd ,org-agenda-redo-command
					     org-series-cmd ,org-cmd))
      (org-agenda-finalize)
      (setq buffer-read-only t))))

(defun org-x-cluster-show-conflicts (&optional arg)
  "Show list of conflicting headlines in agenda buffer.
ARG is something that I'm not sure if I need."
  (interactive "P")
  (org-x-cluster-show-agenda "Conflicts" "Conflicting Headlines"
                             #'org-x-cluster-group-conflicts
                             #'org-x-cluster-format-conflict
                             arg))

(defun org-x-cluster-show-overloads (&optional arg)
  "Show list of overloaded days in agenda buffer.
ARG is something that I'm not sure if I need."
  (interactive "P")
  (org-x-cluster-show-agenda "Overloads" "Overloaded Days"
                             #'org-x-cluster-group-overloads
                             #'org-x-cluster-format-overload
                             arg))

(provide 'org-x)
;;; org-x.el ends here
