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

;; internal vars

;; (defvar org-x-agenda-limit-project-toplevel t
;;   "If true, filter projects by all levels or top level only.")

(defvar org-x-agenda-hide-incubator-tags t
  "If true, don't show incubator headings.")

(defvar org-x-agenda-hasprop-filter nil)

;; list

(defun org-x-filter-list-prefix (prefix str-list)
  "Return a subset of STR-LIST whose first characters are PREFIX."
  (--filter (and (stringp it) (s-prefix-p prefix it)) str-list))

;; org-element

(defun org-x-element-parse-headline (&optional granularity subtree)
  "Like `org-element-parse-buffer' but on only one headline. Assumes 
that point is currently on the starting line of the headline in
question. if SUBTREE is t, return all the subheadings under this
heading."
   ;; (line-beginning-position)
  (let ((start (point))
        (end (if subtree
                 (save-excursion (org-end-of-subtree))
               (save-excursion (outline-next-heading) (point)))))
    (-> (org-element--parse-elements
         start end 'first-section nil granularity nil nil)
        car)))

(defun org-x-element-first-lb-entry (headline)
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

;; (defun org-x-element-first-lb-entry (headline)
;;   "Get the first logbook entry of the headline under point."
;;   (letrec
;;       ((get-ts
;;         (lambda (obj)
;;           (if (eq 'clock (org-element-type obj))
;;               (--> obj
;;                    (org-element-property :value it)
;;                    ;; assume this will return the latest even if
;;                    ;; not a range
;;                    (org-timestamp-split-range it t))
;;             (->>
;;              obj
;;              org-element-contents
;;              car
;;              org-element-contents
;;              car
;;              ;; this assumes that the log timestamps are always
;;              ;; at the end of the first line
;;              (--take-while (not (eq 'line-break (org-element-type it))))
;;              (--last (eq 'timestamp (org-element-type it))))))))
;;   (-some-->
;;    headline
;;    (org-element-contents it)
;;    (car it)
;;    (org-element-contents it)
;;    (--first
;;     (equal org-log-into-drawer (org-element-property :drawer-name it))
;;     it)
;;    (org-element-contents it)
;;    (car it)
;;    (funcall get-ts it)
;;    (org-element-property :raw-value it))))

;; timestamp processing

(defun org-x-get-date-property (timestamp-property)
  "Get TIMESTAMP-PROPERTY on current heading and convert to a number.
If it does not have a date, it will return nil."
  (let ((ts (org-entry-get nil timestamp-property)))
        (when ts (org-2ft ts))))

(defun org-x-heading-compare-timestamp (timestamp-fun
                                     &optional ref-time future)
  "Returns the timestamp (from TIMESTAMP-FUN on the current heading) 
if timestamp is futher back in time compared to a REF-TIME (default to 
0 which is now, where negative is past and positive is future). If the 
FUTURE flag is t, returns timestamp if it is in the future compared 
to REF-TIME. Returns nil if no timestamp is found."
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
  (org-x-heading-compare-timestamp
   (lambda () (-some-> (org-entry-get nil "X-EXPIRE")
           (org-2ft)))))

(defun org-x-is-expired-dtl-headline-p ()
  (org-x-heading-compare-timestamp
   (lambda () (let ((dtl (org-entry-get nil "X-DAYS_TO_LIVE"))
               (created (org-entry-get nil "CREATED")))
           (when (and dtl (s-matches-p "[0-9]+" dtl) created)
             (+ (org-2ft created)
                (* (string-to-number dtl) 24 60 60)))))))

(defun org-x-is-expired-headline-p ()
  ;; NOTE: this will return the dtl ft even if the date ft is less
  (or (org-x-is-expired-dtl-headline-p)
      (org-x-is-expired-date-headline-p)))

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
  (let ((keyword (nth 2 (org-heading-components))))
    (if (member keyword org-todo-keywords-1)
        keyword)))

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

(defun org-x-descend-into-project
    (allowed-statuscodes trans-tbl get-task-status callback-fun)
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
obtain a statuscode-equivalent of the status of the tasks."
  ;; define "breaker-status" as the last of the allowed-statuscodes
  ;; when this is encountered the loop is broken because we are done
  ;; (the last entry trumps all others)
  (let ((project-status (cl-first allowed-statuscodes))
        (breaker-status (-last-item allowed-statuscodes))
        (previous-point))
    (save-excursion
      (setq previous-point (point))
      (outline-next-heading)
      ;; loop through subproject tasks until breaker-status found
      (while (and (not (eq project-status breaker-status))
                  (> (point) previous-point))
        (let ((keyword (org-x-is-todoitem-p)))
          (if keyword
              (let ((new-status
                     ;; if project then descend recursively
                     (if (org-x-headline-has-children 'org-x-is-todoitem-p)
                         (let ((n (funcall callback-fun)))
                           ;; if project returns an allowed status
                           ;; then use that
                           (or (and (member n allowed-statuscodes) n)
                               ;; otherwise look up the value in the
                               ;; translation table and return error
                               ;; if not found
                               (nth (or (alist-get n trans-tbl)
                                        (error (concat "status not found: " n)))
                                    allowed-statuscodes)))
                       ;; if tasks then use get-task-status to obtain status
                       (nth (funcall get-task-status keyword)
                            allowed-statuscodes))))
                (if (org-x-compare-statuscodes > new-status project-status allowed-statuscodes)
                    (setq project-status new-status)))))
        (setq previous-point (point))
        (org-forward-heading-same-level 1 t)))
    project-status))

(defun org-x-get-project-status ()
  "Return project heading statuscode (assumes it is indeed a project)."
  (let ((keyword (org-x-is-todoitem-p)))
    ;;
    ;; these first three are easy because they only require
    ;; testing the project headline and nothing underneath
    ;;
    (cond
     ;; it does not make sense for projects to be scheduled
     ((org-x-is-scheduled-heading-p) :scheduled-project)

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
       '(:archivable :complete :done-incomplete)
       '((:stuck . 2)
         (:inert . 2)
         (:held . 2)
         (:wait . 2)
         (:active . 2)
         (:scheduled-project . 2)
         (:invalid-todostate . 2)
         (:undone-complete . 2))
       (lambda (k)
         (if (member k org-done-keywords)
             (if (org-x-is-archivable-heading-p) 0 1)
           2))
       #'org-x-get-project-status))
     
     ;; project with TODO states could be basically any status
     ((equal keyword "TODO")
      (org-x-descend-into-project
       '(:undone-complete :stuck :held :wait :inert :active)
       '((:complete . 0)
         (:archivable . 0)
         (:scheduled-project . 1)
         (:invalid-todostate . 1)
         (:done-incomplete . 1))
       (lambda (k)
         (cond ((and (not (member k org-done-keywords))
                     (org-x-is-inert-p)) 4)
               ((equal k "TODO") (if (org-x-is-scheduled-heading-p) 5 1))
               ((equal k "HOLD") 2)
               ((equal k "WAIT") 3)
               ((equal k "NEXT") 5)
               (t 0)))
       #'org-x-get-project-status))
     
     (t (error (concat "invalid keyword detected: " keyword))))))

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

;; (defmacro org-x-skip-heading-without (heading-fun test-fun)
;;   "Skip headings accoring to certain characteristics. 

;; HEADING-FUN is a function that tests the heading and returns the 
;; todoitem keyword on success. TEST-FUN is a function that further tests 
;; the identity of the heading and may or may not use the keyword output 
;; supplied by the HEADING-FUN. This function will not skip if 
;; HEADING-FUN and TEST-FUN return true"
;;   `(save-restriction
;;      (widen)
;;      (let ((keyword (,heading-fun)))
;;        ;; (message keyword)
;;        (if (not (and keyword ,test-fun))
;;            (org-x-skip-heading)))))

(defun org-x-skip-headings-with-tags (pos-tags-list &optional neg-tags-list)
  "Skip headings that have tags in POS-TAGS-LIST and not in NEG-TAGS-LIST."
  (save-restriction
    (widen)
    (let ((heading-tags (org-get-tags)))
      (if (and (or (not pos-tags-list)
                   (cl-intersection pos-tags-list heading-tags :test 'equal))
               (not (cl-intersection neg-tags-list heading-tags :test 'equal)))
          (org-x-skip-heading)))))

;; (defun org-x-skip-non-stale-headings ()
;;   "Skip headings that do not have stale timestamps and are not part of projects."
;;   (save-restriction
;;     (widen)
;;     (let ((keyword (org-x-is-todoitem-p)))
;;       (if (not
;;            (and (org-x-is-stale-heading-p)
;;                 (not (member keyword org-done-keywords))
;;                 (not (org-x-headline-has-children 'org-x-is-todoitem-p))
;;                 (not (org-x-headline-has-parent 'org-x-is-todoitem-p))))
;;           (org-x-skip-heading)))))

;; (defun org-x-skip-non-tasks ()
;;   "Skip headlines that are not tasks."
;;   (save-restriction
;;     (widen)
;;     (let ((keyword (org-x-is-todoitem-p)))
;;       (if keyword
;;           (when (org-x-headline-has-children 'org-x-is-todoitem-p)
;;               (if (member keyword org-x-project-skip-todostates)
;;                   (org-x-skip-subtree)
;;                 (org-x-skip-heading)))
;;         (org-x-skip-heading)))))

;; (defun org-x-skip-non-uncancelled-tasks ()
;;   "Skip headlines that are not nonarchivable tasks."
;;   (save-restriction
;;     (widen)
;;     (let ((keyword (org-x-is-todoitem-p)))
;;       (if (org-x-headline-has-children 'org-x-is-todoitem-p)
;;           (if (member keyword org-x-project-skip-todostates)
;;               (org-x-skip-subtree)
;;             (org-x-skip-heading))
;;         (when (equal keyword "CANC") (org-x-skip-heading))))))
        

;; (defun org-x-skip-non-created-tasks ()
;;   "Skip tasks that do not have CREATED timestamp properties."
;;   (save-excursion
;;     (widen)
;;     (if (not (and (org-x-is-task-p)
;;                   (not (org-x-is-created-heading-p))))
;;         (org-x-skip-heading))))

;; (defun org-x-skip-non-atomic-tasks ()
;;   "Skip headings that are not atomic tasks."
;;   (save-excursion
;;     (widen)
;;     (if (not (org-x-is-atomic-task-p))
;;         (org-x-skip-heading))))

;; (defun org-x-skip-non-closed-atomic-tasks ()
;;   "Skip headings that are not complete (but not archivable) atomic tasks."
;;   (org-x-skip-heading-without
;;    org-x-is-atomic-task-p
;;    (and (member keyword org-done-keywords)
;;         (not (org-x-is-archivable-heading-p)))))

;; (defun org-x-skip-non-archivable-atomic-tasks ()
;;   "Skip headings that are not archivable atomic tasks."
;;   (org-x-skip-heading-without
;;    org-x-is-atomic-task-p
;;    (org-x-is-archivable-heading-p)))

;; (defun org-x-skip-non-project-tasks ()
;;   "Skip headings that are not project tasks."
;;   (save-restriction
;;     (widen)
;;     (let ((keyword (org-x-is-todoitem-p)))
;;       (if keyword
;;           (if (org-x-headline-has-children 'org-x-is-todoitem-p)
;;               (if (member keyword org-x-project-skip-todostates)
;;                   (org-x-skip-subtree)
;;                 (org-x-skip-heading))
;;             (if (not (org-x-headline-has-parent 'org-x-is-todoitem-p))
;;                 (org-x-skip-heading)))
;;         (org-x-skip-heading)))))

;; (defun org-x-skip-non-discontinuous-project-tasks ()
;;   "Skip headings that are not discontinuous within projects."
;;   (org-x-skip-heading-without
;;    org-x-is-todoitem-p
;;    (org-x-has-discontinuous-parent)))

;; (defun org-x-skip-non-done-unclosed-todoitems ()
;;   "Skip headings that are not completed without a closed timestamp."
;;   (org-x-skip-heading-without
;;    org-x-is-todoitem-p
;;    (and (member keyword org-done-keywords)
;;         (not (org-x-is-closed-heading-p)))))

;; (defun org-x-skip-non-undone-closed-todoitems ()
;;   "Skip headings that are not incomplete with a closed timestamp."
;;   (org-x-skip-heading-without
;;    org-x-is-todoitem-p
;;    (and (not (member keyword org-done-keywords))
;;         (org-x-is-closed-heading-p))))

;; (defun org-x-skip-non-projects (&optional ignore-toplevel)
;;   "Skip headings that are not projects (toplevel-only if IGNORE-TOPLEVEL is t)."
;;   (save-restriction
;;     (widen)
;;     (let ((keyword (org-x-is-project-p)))
;;       (if keyword
;;           (if (and org-x-agenda-limit-project-toplevel
;;                    (not ignore-toplevel)
;;                    (org-x-headline-has-parent 'org-x-is-todoitem-p))
;;               (org-x-skip-subtree))
;;         (org-x-skip-heading)))))

;; sorting and filtering

(defun org-x-agenda-filter-prop (a-line filter prop-fun
                                           &optional prop-key)
  "Filter for `org-agenda-before-sorting-filter-function' where
A-LINE is a line from the agenda view, FILTER is an ordered list
of property values to be filtered/sorted, and PROP-FUN is a function
that determines a property value based on the org content of the
original buffer. If PROP-KEY is supplied, assign the return value of
PROP-FUN to PROP-KEY in A-LINE's text properties. Returns either nil
if return value of PROP-FUN not in FILTER or A-LINE (modified or not)."
  (let* ((m (get-text-property 1 'org-marker a-line))
         (s (with-current-buffer (marker-buffer m)
              (goto-char m)
              (funcall prop-fun))))
    (when (cl-find s filter)
        (if (not prop-key) a-line
          (org-add-props a-line nil prop-key s)))))
               
(defun org-x-agenda-regexp-replace-props (props)
  (letrec
      ((replace-prop
        (lambda (p)
          (let ((prop-val (->> (thing-at-point 'line)
                               (get-text-property 1 (cdr p))
                               symbol-name))
                (re (format "$%s$" (car p))))
            (when prop-val
              (save-excursion
                (when (search-forward re (line-end-position) t 1)
                  (replace-match prop-val))))))))
   (save-excursion
     (goto-char (point-min))
     (while (< (point) (point-max))
       (--each props (funcall replace-prop it))
       (forward-line)))))
       
;; (add-hook
;;  'org-agenda-finalize-hook
;;  (lambda ()
;;    (org-x-agenda-regexp-replace-props '(("y" . atomic)
;;                                          ("xxxx" . statuscode)))))

(defun org-x-agenda-sort-prop (prop order a b)
  "Sort a block agenda view by text property PROP given a list ORDER
of said text properties in the desired order and lines A and B as 
inputs. To be used with `org-agenda-cmp-user-defined'."
  (let* ((ta (get-text-property 1 prop a))
         (tb (get-text-property 1 prop b))
         (pa (cl-position ta order :test (if (stringp ta) #'equal)))
         (pb (cl-position tb order :test (if (stringp tb) #'equal))))
    (cond ((or (null pa) (null pb)) nil)
          ((< pa pb) +1)
          ((> pa pb) -1))))

(defun org-x-agenda-sort-multi (a b &rest funs)
  "Sort lines A and B from block agenda view given functions FUNS.
Functions in FUNS must take either A or B as their arguments and
should return a positive integer indicating their rank. The FUNS
list is traversed in order, where the front is the outermost sorting
order."
  (let* ((fun (car funs))
         (pa (funcall fun a))
         (pb (funcall fun b)))
    (cond
     ((< pa pb) +1)
     ((> pa pb) -1)
     (t (-some->> funs cdr (apply #'org-x-agenda-sort-multi a b))))))

(defun org-x-agenda-sort-task-todo (line)
  (or
   (-some-> (get-text-property 1 'todo-state line)
            (cl-position org-x-agenda-todo-sort-order :test #'equal))
   (length org-x-agenda-todo-sort-order)))
  
(defun org-x-agenda-sort-status (line order)
  (or
   (-some-> (get-text-property 1 'statuscode line) (cl-position order))
   (length order)))

(defun org-x-agenda-sort-task-atomic (line)
  (if (eq '-!- (get-text-property 1 'atomic line)) 1 0))

;; block agenda macros

;; (defun org-x-agenda-base-heading-cmd (match header skip-fun)
;;   "Make a tags agenda view that matches tags in string MATCH with
;; header given as string HEADER and with skip function SKIP-FUN."
;;   `(tags
;;     ,match
;;     ((org-agenda-overriding-header ,header)
;;      (org-agenda-skip-function ,skip-fun)
;;      (org-agenda-sorting-strategy '(category-keep)))))

;; (defun org-x-agenda-base-task-cmd (match header skip-fun &optional sort)
;;   "Make a tags-todo agenda view that matches tags in string MATCH with
;; header given as string HEADER and with skip function SKIP-FUN. Also
;; takes a sorting structure SORT which is passed to 
;; `org-agenda-sorting-strategy'"
;;   (or sort (setq sort ''(category-keep)))
;;   `(tags-todo
;;     ,match
;;     ((org-agenda-overriding-header ,header)
;;      (org-agenda-skip-function ,skip-fun)
;;      (org-agenda-todo-ignore-with-date t)
;;      (org-agenda-sorting-strategy ,sort))))

;; (defun org-x-agenda-base-task-cmd* (match header skip-fun kw-list status-fun
;;                                          &optional status-px)
;;   (let ((prefix (if status-px
;;                     ''((tags . "  %-12:c $xxxx$: $y$ %-5:e "))
;;                   ''((tags . "  %-12:c         %-5:e")))))
;;     `(tags-todo
;;       ,match
;;       ((org-agenda-overriding-header ,header)
;;        (org-agenda-skip-function ,skip-fun)
;;        (org-agenda-todo-ignore-with-date t)
;;        (org-agenda-before-sorting-filter-function
;;         (lambda (l)
;;           (-some->
;;            l
;;            (org-x-agenda-filter-prop ,kw-list ,status-fun 'statuscode)
;;            (org-x-agenda-filter-prop
;;             '(-*- -!-) (lambda () (if (org-x-is-atomic-task-p) '-!- '-*-)) 'atomic))))
;;        (org-agenda-cmp-user-defined
;;         (lambda (a b)
;;           (org-x-agenda-sort-multi
;;            a b
;;            (lambda (l) (org-x-agenda-sort-status l ,kw-list))
;;            #'org-x-agenda-sort-task-atomic
;;            #'org-x-agenda-sort-task-todo)))
;;        (org-agenda-prefix-format ,prefix)
;;        (org-agenda-sorting-strategy '(user-defined-down category-keep))))))

;; (defun org-x-agenda-base-project-cmd (match header skip-fun kw-list status-fun
;;                                          &optional todo status-px)
;;   "Make a tags-todo agenda view that matches tags in string MATCH with
;; header given as string HEADER and with skip function SKIP-FUN. KW-LIST
;; is a list of keywords to be used in filtering and sorting (the order
;; in the list defines the sort order). STATUS-FUN is a function used to
;; get the statuscode of the current line in the agenda. Optional arg
;; TODO determines if this is a tags-todo (t) or tags (nil) block, and
;; STATUS-PX as t enables the statuscode to be formatted into the prefix
;; string."
;;   (let ((prefix (if status-px
;;                     ''((tags . "  %-12:c $xxxx$: "))
;;                   ''((tags . "  %-12:c        ")))))
;;     `(,(if 'tags-todo 'tags)
;;       ,match
;;       ((org-agenda-overriding-header ,header)
;;        (org-agenda-skip-function ,skip-fun)
;;        (org-agenda-before-sorting-filter-function
;;         (lambda (l) (org-x-agenda-filter-prop l ,kw-list ,status-fun 'statuscode)))
;;        (org-agenda-cmp-user-defined
;;         (lambda (a b) (org-x-agenda-sort-prop 'statuscode ,kw-list a b)))
;;        (org-agenda-prefix-format ,prefix)
;;        (org-agenda-sorting-strategy '(user-defined-down category-keep))))))

;; interactive functions

;; (defun org-x-toggle-project-toplevel-display ()
;;   "Toggle all project headings and toplevel only headings in project blocks."
;;   (interactive)
;;   (setq org-x-agenda-limit-project-toplevel (not org-x-agenda-limit-project-toplevel))
;;   (when (equal major-mode 'org-agenda-mode)
;;     (org-agenda-redo))
;;   (message "Showing %s project view in agenda"
;;            (if org-x-agenda-limit-project-toplevel "toplevel" "complete")))

(defun org-x-mark-subtree-keyword (new-keyword &optional exclude no-log)
  "Mark all tasks in a subtree with NEW-KEYWORD unless original
keyword is in the optional argument EXCLUDE."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (org-todo-log-states (unless no-log org-todo-log-states)))
    (if (not (listp exclude))
        (error "exlude must be a list if provided"))
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
    ;; (save-excursion
    ;;   (when (org-up-heading-safe)
    ;;     (-> (org-ml-parse-this-subtree)
    ;;         (org-ml-headline-get-subheadlines)
    ;;         (-each #'org-ml-fold))))))

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

;; (defun org-x-clone-subtree-with-time-shift-toplevel (n)
;;   "Go to the last item underneath an iterator and clone using
;; `org-x-agenda-clone-subtree-with-time-shift'. Assumes point starts on
;; the top level headline and only looks at the second level of
;; headlines to clone."
;;   (interactive "nNumber of clones to produce: ")
;;   ;; do nothing if there is nothing to clone
;;   (unless (eq :uninit
;;               (or (and (org-x-is-iterator-heading-p)
;;                        (org-clone-get-iterator-status))
;;                   (and (org-x-is-periodical-heading-p)
;;                        (org-clone-get-periodical-status))))
;;     ;; goto last item in the second level
;;     (save-excursion
;;       (let ((current-point (point)))
;;         (outline-next-heading)
;;         (while (< current-point (point))
;;           (setq current-point (point))
;;           (org-forward-heading-same-level 1 t)))
;;       (org-x-clone-subtree-with-time-shift n))))

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
Does not touch the running clock. When called with one C-u prefix
argument, ask for a range in minutes in place of the second date."
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
  "Wraps commands in BODY in necessary code to allow commands to be
called from the agenda buffer. Particularly, this wrapper will
navigate to the original header, execute BODY, then update the agenda
buffer."
  '(org-agenda-check-no-diary)
  `(let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
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
	  (beginning-of-line 1))))
  
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
  "Apply `org-x-clock-range' to an agenda entry"
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

;; In order to implement the =hasprop= filter, the functions =org-agenda-filter-make-matcher= and =org-agenda-filter-remove-all= need to be advised in order to add the functionality for the =hasprop= filter type. 

;; As it is, this allows any filter using =hasprop= to be applied and removed using the standard =org-agenda-filter-apply= function with the =org-x-agenda-hasprop-filter= variable (obviously these can all be extended to different filter types). Note this does not give a shiny indicator at the bottom of spaceline like the built-in filter does...oh well.


(defun org-x-agenda-filter-make-matcher-prop
    (filter type &rest _args)
  "Return matching matcher form for FILTER and TYPE where TYPE is not
in the regular `org-agenda-filter-make-matcher' function. This is
intended to be uses as :before-until advice and will return nil if
the type is not valid (which is currently 'prop')"
  (let (f)
    ;; has property
    (cond
     ((eq type 'hasprop)
      (dolist (x filter)
        (push (org-x-agenda-filter-make-matcher-hasprop-exp x) f))))
    (if f (cons 'and (nreverse f)))))

(defun org-x-agenda-filter-make-matcher-hasprop-exp (h)
 "Returns form to test the presence or absence of properties H.
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

(defun org-x-agenda-filter-show-all-hasprop nil
  (org-agenda-remove-filter 'hasprop))

(advice-add #'org-agenda-filter-make-matcher :before-until
            #'org-x-agenda-filter-make-matcher-prop)

(advice-add #'org-agenda-filter-remove-all :before
            (lambda () (when org-x-agenda-hasprop-filter
                    (org-x-agenda-filter-show-all-hasprop))))

;; (defun org-x-agenda-helm-select-categories ()
;;   "Filter by category using helm interface."
;;   (interactive)
;;   (when (eq major-mode 'org-agenda-mode)
;;     (-when-let
;;         (cats
;;          (-->
;;           (buffer-string)
;;           (split-string it "\n")
;;           (--remove (get-text-property 0 'invisible it) it)
;;           (--map (get-text-property 0 'org-category it) it)
;;           (-non-nil it)
;;           (-uniq it)
;;           (sort it #'string<)))
;;       (let ((exclude
;;              (lambda (c)
;;                (org-agenda-filter-apply
;;                 (push (concat "-" c) org-agenda-category-filter)
;;                 'category)))
;;             (include
;;              (lambda (c)
;;                (org-agenda-filter-apply
;;                 (setq org-agenda-category-filter
;;                       (list (concat "+" c)))
;;                 'category))))
;;         (helm :sources
;;               (helm-build-sync-source "Categories"
;;                 :candidates cats
;;                 :action `(("Include" . ,(-partial include))
;;                           ("Exclude" . ,(-partial exclude))))
;;               :buffer "*helm-category-select*"
;;               :prompt "Category: ")))))

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

(defun org-x-tags-view-advice (orig-fn &optional todo-only match)
  "Advice to include done states in `org-tags-view' for tags-todo agenda types."
  (nd/with-advice
      ((#'org-make-tags-matcher
        :around (lambda (f m)
                  (let ((org--matcher-tags-todo-only nil))
                    (funcall f m)))))
    (funcall orig-fn todo-only match)))

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

(provide 'org-x)
;;; org-x.el ends here
