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

;; Extra org-mode glue code I use to run my life. These are generally bits and
;; pieces that I deem useful enough to put in their own file separate from my
;; 'main' config to a) keep me sane b) test things and c) fork off into a
;; separate package if I think it is worthy (mostly (a)).

;;; Code:

(require 'org-ml)
(require 'dash)
(require 's)
(require 'org)
(require 'org-x-agg)

;;; TODO KEYWORDS

(defconst org-x-kw-todo "TODO"
  "Headline todo keyword for open task or project.")

(defconst org-x-kw-next "NEXT"
  "Headline todo keyword for next task.")

(defconst org-x-kw-wait "WAIT"
  "Headline todo keyword for task that is waiting on something.")

(defconst org-x-kw-hold "HOLD"
  "Headline todo keyword for task or project that is held.")

(defconst org-x-kw-done "DONE"
  "Headline todo keyword for completed task or project.")

(defconst org-x-kw-canc "CANC"
  "Headline todo keyword for canceled task or project.")

;;; TAGS

(defun org-x-prepend-char (char string)
  "Return STRING with CHAR appended to the front."
  (concat (char-to-string char) string))

(defconst org-x-tag-location-prefix ?@
  "Prefix character denoting location context tag.")

(defconst org-x-tag-resource-prefix ?#
  "Prefix character denoting resource context tag.")

(defconst org-x-tag-misc-prefix ?%
  "Prefix character denoting misc tag.")

(defconst org-x-tag-category-prefix ?_
  "Prefix character denoting life category tag.")

(defconst org-x-tag-errand
  (org-x-prepend-char org-x-tag-location-prefix "errand")
  "Tag denoting an errand location.")

(defconst org-x-tag-home
  (org-x-prepend-char org-x-tag-location-prefix "home")
  "Tag denoting a home location.")

(defconst org-x-tag-work
  (org-x-prepend-char org-x-tag-location-prefix "work")
  "Tag denoting a work location.")

(defconst org-x-tag-travel
  (org-x-prepend-char org-x-tag-location-prefix "travel")
  "Tag denoting a travel location.")

(defconst org-x-tag-laptop
  (org-x-prepend-char org-x-tag-resource-prefix "laptop")
  "Tag denoting a laptop resource.")

(defconst org-x-tag-tcult
  (org-x-prepend-char org-x-tag-resource-prefix "tcult")
  "Tag denoting a tissue-culture resource.")

(defconst org-x-tag-phone
  (org-x-prepend-char org-x-tag-resource-prefix "phone")
  "Tag denoting a phone resource.")

(defconst org-x-tag-note
  (org-x-prepend-char org-x-tag-misc-prefix "note")
  "Tag denoting a note.")

(defconst org-x-tag-incubated
  (org-x-prepend-char org-x-tag-misc-prefix "inc")
  "Tag denoting an incubated task.")

(defconst org-x-tag-maybe
  (org-x-prepend-char org-x-tag-misc-prefix "maybe")
  "Tag denoting a maybe task.")

(defconst org-x-tag-subdivision
  (org-x-prepend-char org-x-tag-misc-prefix "subdiv")
  "Tag denoting a task awaiting subdivision.")

(defconst org-x-tag-flagged
  (org-x-prepend-char org-x-tag-misc-prefix "flag")
  "Tag denoting a flagged task.")

(defconst org-x-tag-environmental
  (org-x-prepend-char org-x-tag-category-prefix "env")
  "Tag denoting an environmental life category.")

(defconst org-x-tag-financial
  (org-x-prepend-char org-x-tag-category-prefix "fin")
  "Tag denoting a financial life category.")

(defconst org-x-tag-intellectual
  (org-x-prepend-char org-x-tag-category-prefix "int")
  "Tag denoting an intellectual life category.")

(defconst org-x-tag-metaphysical
  (org-x-prepend-char org-x-tag-category-prefix "met")
  "Tag denoting an metaphysical life category.")

(defconst org-x-tag-physical
  (org-x-prepend-char org-x-tag-category-prefix "phy")
  "Tag denoting an physical life category.")

(defconst org-x-tag-professional
  (org-x-prepend-char org-x-tag-category-prefix "pro")
  "Tag denoting a professional life category.")

(defconst org-x-tag-recreational
  (org-x-prepend-char org-x-tag-category-prefix "rec")
  "Tag denoting a recreational life category.")

(defconst org-x-tag-social
  (org-x-prepend-char org-x-tag-category-prefix "soc")
  "Tag denoting a social life category.")

(defconst org-x-tag-no-agenda "NA"
  "Tag denoting a headlines that shouldn't go in the agenda.")

(defconst org-x-tag-refile "REFILE"
  "Tag denoting a headlines that are to be refiled.")

;;; PROPERTIES

(eval-and-compile
  (defun org-x-define-prop-choices (prop options &optional allow-other)
    (let ((options* (if allow-other (-snoc options ":ETC") options)))
      (cons (format "%s_ALL" prop) (s-join " " options*)))))

(eval-and-compile
  (defconst org-x-prop-parent-type "PARENT_TYPE"
    "Property denoting iterator/periodical headline."))

(eval-and-compile
  (defconst org-x-prop-parent-type-choices
    (org-x-define-prop-choices org-x-prop-parent-type '("periodical" "iterator"))
    "Choices for `org-x-prop-parent-type'."))

(defconst org-x-prop-time-shift "TIME_SHIFT"
  "Property denoting time shift when cloning iterator/periodical headlines.")

;; TODO this is a WIP
(defconst org-x-prop-thread "THREAD"
  "Property denoting an email thread to track.")

(eval-and-compile
  (defconst org-x-prop-routine "X-ROUTINE"
    "Property denoting a routine group."))

(eval-and-compile
  (defconst org-x-prop-routine-choices
    (org-x-define-prop-choices org-x-prop-routine '("morning" "evening"))
    "Choices for `org-x-prop-routine'."))

(defconst org-x-prop-created "CREATED"
  "Property denoting when a headline was created.")

(defconst org-x-prop-expire "X-EXPIRE"
  "Property denoting when a headline will expire.")

(defconst org-x-prop-days-to-live "X-DAYS_TO_LIVE"
  "Property denoting after how many days a headline will expire.")

;;; CONSTANTS

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
  (list org-x-kw-wait org-x-kw-next)
  "Projects cannot have these todostates.")
  
(defconst org-x-agenda-todo-sort-order
  (list org-x-kw-next org-x-kw-wait org-x-kw-hold org-x-kw-todo)
  "Defines the order in which todo keywords should be sorted.")
  
(defconst org-x-project-skip-todostates
  '(org-x-kw-hold org-x-kw-canc)
  "These keywords override all contents within their subtrees.
Currently used to tell skip functions when they can hop over
entire subtrees to save time and ignore tasks")

;; internal vars

(defvar org-x-agenda-hasprop-filter nil)

;; list

(defun org-x-filter-list-prefix (prefix str-list)
  "Return a subset of STR-LIST whose first characters are PREFIX."
  (--filter (and (stringp it) (s-prefix-p prefix it)) str-list))

;; org-element

;; TODO this should be in org-ml
(defun org-x-logbook-config ()
  "Return the logbook config for `org-ml-headline-get-supercontents' et al."
  (list :log-into-drawer org-log-into-drawer
        :clock-into-drawer org-clock-into-drawer
        :clock-out-notes org-log-note-clock-out))

(defun org-x-element-first-lb-entry (headline)
  "Return epoch time of most recent logbook item or clock from HEADLINE."
  (let* ((config (org-x-logbook-config))
         (logbook (->> (org-ml-headline-get-supercontents config headline)
                       (org-ml-supercontents-get-logbook)))
         (first-item-ut (-some->> (org-ml-logbook-get-items logbook)
                          (car)
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
  (org-x-get-date-property org-x-prop-created))

(defun org-x-is-closed-heading-p ()
  "Get closed timestamp of current heading."
  (org-x-get-date-property "CLOSED"))

(defun org-x-is-stale-heading-p (&optional ts-prop)
  "Return timestamp for TS-PROP (TIMESTAMP by default) if current heading is stale."
  (org-x-heading-compare-timestamp
   (lambda () (let ((ts (org-entry-get nil (or ts-prop "TIMESTAMP"))))
           (when (and ts (not (cl-find ?+ ts))) (org-2ft ts))))))

(defun org-x-is-expired-date-headline-p ()
  "Return timestamp if current headline is expired via `org-x-prop-expire'."
  (org-x-heading-compare-timestamp
   (lambda () (-some-> (org-entry-get nil org-x-prop-expire)
           (org-2ft)))))

(defun org-x-is-expired-dtl-headline-p ()
  "Return timestamp if current headline is expired via `org-x-prop-days-to-live'."
  (org-x-heading-compare-timestamp
   (lambda () (let ((dtl (org-entry-get nil org-x-prop-days-to-live))
               (created (org-entry-get nil org-x-prop-created)))
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
         (created-ut (-some->> (org-ml-headline-get-node-property org-x-prop-created hl)
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
  (equal "periodical" (org-entry-get nil org-x-prop-parent-type t)))

(defun org-x-is-iterator-heading-p ()
  "Return t if heading is an iterator."
  (equal "iterator" (org-entry-get nil org-x-prop-parent-type t)))

(defun org-x-is-habit-heading-p ()
  "Return t if heading is an iterator."
  (equal "habit" (org-entry-get nil "STYLE" t)))

(defun org-x-headline-has-effort-p ()
  "Return t if heading has an effort."
  (org-entry-get nil org-effort-property))

(defun org-x-headline-has-context-p ()
  "Return non-nil if heading has a context tag."
  (let ((tags (org-get-tags)))
    (--any (memq (elt it 0) (list org-x-tag-resource-prefix
                                  org-x-tag-location-prefix))
           tags)))

(defun org-x-headline-has-tag-p (tag)
  "Return t if heading has tag TAG."
  (member tag (org-get-tags)))

;; relational testing

(defun org-x-headline-get-level ()
  "Return level of the current headline.
Assumes point is at the start of a headline."
  (save-excursion
    (forward-char 1)
    (while (= ?* (following-char)) (forward-char 1))
    (current-column)))

(defmacro org-x-while-child-headlines (while-form &rest body)
  "Run BODY for each child headline in the subtree under point.
Assume point is at the start of a headline. Loop through children
until WHILE-FORM evals to nil. Note that this only applies BODY
to the children one level down from the current headline."
  ;; Rather than using regular expressions, it is much faster and simpler to
  ;; walk down each line and count the number of stars to get the level.
  ;;
  ;; Algorithm steps:
  ;; 1. Count stars on the current headline (move point forward until first
  ;;    non-star, and use the column number to get level) and add 1 to get
  ;;    the "target-level" (that is the child level of the current headline)
  ;; 2. Move forward one line until a) `while-form' returns nil b) the current
  ;;    level of the org-tree is less than the target-level or c) the end of
  ;;    the buffer is reached.
  ;;    2.1. If point not on a star, continue looping.
  ;;    2.2. Otherwise, get the current level similar to (1) using the column
  ;;         number. If the current level is equal to the target level, eval
  ;;         `body', otherwise do nothing since point is too deep in the tree.
  (declare (indent 1))
  `(save-excursion
     (let* ((target-level (1+ (org-x-headline-get-level)))
            (cur-level target-level))
       (while (and ,while-form
                   (<= target-level cur-level)
                   (= 0 (forward-line 1)))
         (when (= ?* (following-char))
           (setq cur-level (org-x-headline-get-level))
           (when (= cur-level target-level)
             ,@body))))))

(defun org-x-headline-has-children (test-fun)
  "Return t if heading has a child for whom TEST-FUN is t.
Assume that point is at the beginning of a headline."
  (let ((has-children nil))
    (org-x-while-child-headlines (not has-children)
      (when (funcall test-fun)
        (setq has-children t)))
    has-children))

(defun org-x-headline-has-parent (heading-test)
  "Return t if heading has parent for whom HEADING-TEST is t."
  (save-excursion (and (org-up-heading-safe) (funcall heading-test))))

(defun org-x-up-headline ()
  "Move point up to the next parent headline or `point-min' if none.
Return point on success and nil on failure. Assume point is
current at the start of a headline."
  (let ((cur-level (org-x-headline-get-level)))
    (if (= 1 cur-level)
        (beginning-of-buffer)
      (let ((target-level (1- cur-level))
            (stop nil))
        (while (and (not stop) (= 0 (forward-line -1)))
          (when (= ?* (following-char))
            (when (= (org-x-headline-get-level) target-level)
              (setq stop t)
              (beginning-of-line)
              (point))))))))

;; (defun org-x-headline-has-parent (heading-test)
;;   "Return t if heading has parent for whom HEADING-TEST is t."
;;   (save-excursion
;;     (and (org-x-up-headline) (funcall heading-test))))

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
    `(save-excursion
      (let ((project-status ,initial-status)
            (new-status nil)
            (it-kw nil))
        ;; loop through tasks one level down until breaker-status found
        (org-x-while-child-headlines (not (eq project-status ,breaker-status))
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
              (setq project-status new-status))))
         project-status))))

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
       ((equal keyword org-x-kw-hold) (if (org-x-is-inert-p) :inert :held))

       ;; projects with invalid todostates are nonsense
       ((member keyword org-x-project-invalid-todostates)
        :invalid-todostate)

       ;; canceled projects can either be archivable or complete
       ;; any errors or undone tasks are irrelevant
       ((equal keyword org-x-kw-canc) (if (org-x-is-archivable-heading-p) :archivable
                                 :complete))
       
       ;;
       ;; these require descending into the project subtasks
       ;;

       ;; done projects are like canceled projects but can also be incomplete
       ((equal keyword org-x-kw-done)
        (org-x-descend-into-project
         ((:archivable)
          (:complete)
          (:done-incomplete :stuck :inert :held :wait :active
                            :scheduled-project :invalid-todostate
                            :undone-complete))
         ;; TODO don't use org-done-keywords
         (if (member it-kw org-done-keywords)
             (if (org-x-is-archivable-heading-p) 0 1)
           2)
         org-x-get-project-status))
       
       ;; project with TODO states could be basically any status
       ((equal keyword org-x-kw-todo)
        (org-x-descend-into-project
         ((:undone-complete :complete :archivable)
          (:stuck :scheduled-project :invalid-todostate :done-incomplete)
          (:held)
          (:wait)
          (:inert)
          (:active))
         (cond
          ((and (not (member it-kw org-done-keywords)) (org-x-is-inert-p)) 4)
          ((equal it-kw org-x-kw-todo) (if (org-x-is-scheduled-heading-p) 5 1))
          ((equal it-kw org-x-kw-hold) 2)
          ((equal it-kw org-x-kw-wait) 3)
          ((equal it-kw org-x-kw-next) 5)
          (t 0))
         org-x-get-project-status))
       
       (t (error (concat "invalid keyword detected: " keyword)))))))

;; iterators

(defun org-x--clone-get-iterator-project-status (kw)
  (cond
   ((or (org-x-is-scheduled-heading-p)
        (member kw org-x-project-invalid-todostates)) :project-error)

   ;; canceled tasks add nothing
   ((equal kw org-x-kw-canc) :empt)
   
   ;;
   ;; these require descending into the project subtasks
   ;;

   ;; done projects either add nothing (empty) or are not actually
   ;; done (project error)
   ((equal kw org-x-kw-done)
    (org-x-descend-into-project
     ((:empt)
      (:project-error :unscheduled :actv))
     (if (member it-kw org-done-keywords) 0 1)
     org-x--clone-get-iterator-project-status))
   
   ;; project with TODO states could be basically any status
   ((equal kw org-x-kw-todo)
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

;; periodicals
    
(defun org-x-get-periodical-status ()
  "Get the status of a periodical.
Allowed statuscodes are in list `nd/get-peri-statuscodes.' where
latter codes in the list trump earlier ones."
  (cl-flet
      ((get-ts
        ()
        (-some->> (org-ml-parse-this-headline)
          (org-ml-headline-get-contents (org-x-logbook-config))
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

;;; INTERACTIVE FUNCTIONS

;; cloning

(defun org-x--reset-headline (headline)
  "Reset HEADLINE node to incomplete state.
This includes unchecking all checkboxes, marking keywords as
\"TODO\", clearing any unique IDs, etc."
  (cl-flet*
      ((reset
        (config created-ts headline)
        (->> (if (org-ml-headline-is-done headline)
                 (org-ml-set-property :todo-keyword org-x-kw-todo headline)
               headline)
             (org-ml-headline-map-supercontents* config
               (org-ml-supercontents-set-logbook nil it))
             (org-ml-headline-set-node-property org-x-prop-created created-ts)
             (org-ml-headline-map-planning*
               (if (not it) it
                 (org-ml-planning-set-timestamp! :closed nil it)))
             (org-ml-headline-set-node-property "ID" nil)
             ;; this obviously will be wrong if I ever want to use TODO
             ;; statistics but at least they will be reset to zero
             (org-ml-headline-update-item-statistics))))
    (let ((created-ts (-> (float-time)
                          (org-ml-unixtime-to-time-long)
                          (org-ml-build-timestamp!)
                          (org-ml-to-string))))
      (->> (reset (org-x-logbook-config) created-ts headline)
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

(defun org-x-clone-subtree-with-time-shift (n)
  "Like `org-clone-subtree-with-time-shift' except reset items and todos.
N is the number of clones to produce."
  (interactive "nNumber of clones to produce: ")
  (let* ((st (org-ml-parse-this-subtree))
         (shift
          (or (org-entry-get nil org-x-prop-time-shift 'selective)
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
         (org-ml-headline-get-node-property org-x-prop-time-shift subtree)
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

;; marking subtrees

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
  (org-x-mark-subtree-keyword org-x-kw-done `(,org-x-kw-canc)))

;; logbook

(defun org-x-log-delete ()
  "Delete logbook drawer of subtree."
  (interactive)
  (save-excursion
    ;; TODO redefine in terms of org-ml
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
        ;; TODO rewrite this in terms of org-ml code
        (save-excursion
          (org-clock-find-position nil)
	      (org-indent-line)
          (->> (org-ml-build-clock! s :end e)
               (org-ml-to-string)
               (insert))))))))

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
        (config (org-x-logbook-config)))
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
                       (org-ml-set-property :todo-keyword org-x-kw-done)
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

(defun org-x-delete-subtree ()
  "Delete entire subtree under point without sending to kill ring."
  (interactive)
  (org-back-to-heading t)
  (delete-region (point) (1+ (save-excursion (org-end-of-subtree)))))

;; lift buffer commands into agenda context

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

;; agenda heading navigation functions

(defun org-x-agenda--seek-heading (&optional back)
  "Go to next or previous agenda heading.
If BACK is t seek backward, else forward. Ignore blank lines."
  (cl-flet
      ((is-valid-header
        ()
        (let ((h (buffer-substring (line-beginning-position)
                                   (line-end-position))))
          (and (not (equal h ""))
               (get-text-property 0 'org-agenda-structural-header h)))))
    (let* ((limit (if back (point-min) (point-max)))
           (inc (if back -1 1))
           (next
            (let ((header-point))
              (save-excursion
                (while (and (< 0 (abs (- limit (point)))) (not header-point))
                  (forward-line inc)
                  (when (is-valid-header)
                    (setq header-point (point))))
                header-point))))
      (if next (goto-char next)
        (message (if back "Cannot move up" "Cannot move down"))))))

(defun org-x-agenda-previous-heading ()
  "Go to the previous agenda heading or end of buffer."
  (interactive)
  (org-x-agenda--seek-heading t))

(defun org-x-agenda-next-heading ()
  "Go to the next agenda heading or end of buffer."
  (interactive)
  (org-x-agenda--seek-heading))

;; timestamp shifter

;; TODO refactor in terms of org-ml to make cleaner/safer
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

;; agenda filtering

;; In order to implement the =hasprop= filter, the functions
;; =org-agenda-filter-make-matcher= and =org-agenda-filter-remove-all= need to
;; be advised in order to add the functionality for the =hasprop= filter type.

;; As it is, this allows any filter using =hasprop= to be applied and removed
;; using the standard =org-agenda-filter-apply= function with the
;; =org-x-agenda-hasprop-filter= variable (obviously these can all be extended
;; to different filter types). Note this does not give a shiny indicator at the
;; bottom of spaceline like the built-in filter does...oh well.

(defun org-x-agenda-filter-non-context ()
  "Filter all tasks with context tags."
  (interactive)
  (let ((context-tags
         (->> (-map #'car org-tag-alist)
           (-filter #'stringp)
           (--filter (memq (elt it 0) (list org-x-tag-resource-prefix
                                            org-x-tag-location-prefix))))))
    (setq org-agenda-tag-filter (--map (concat "-" it) context-tags))
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
                 (org-ml-build-node-property org-x-prop-created))))
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
                (org-ml-build-node-property org-x-prop-expire))))
      (org-ml-update-this-headline*
        (org-ml-headline-map-node-properties* (cons np it) it)))))

(defun org-x-set-dtl ()
  "Set days-to-live of the current headline."
  (interactive)
  (let ((n (read-string "Days to live: ")))
    (if (not (s-matches-p "[0-9]+" n))
        (message "Enter a number")
      (let ((np (org-ml-build-node-property org-x-prop-days-to-live n)))
        (org-ml-update-this-headline*
          (org-ml-headline-map-node-properties* (cons np it) it))))))

(advice-add 'org-insert-heading :after #'org-x-set-creation-time)

(add-hook 'org-capture-before-finalize-hook #'org-x-set-creation-time)

(provide 'org-x)
;;; org-x.el ends here
