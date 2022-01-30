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
(require 'ht)
(require 'org)
(require 'org-id)
(require 'org-x-agg)
(require 'org-x-dag)

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

(defconst org-x-done-keywords `(,org-x-kw-done ,org-x-kw-canc)
  "Headline todo keywords that mark a task as 'complete'.")

(defconst org-x-meeting-keywords (cons org-x-kw-todo org-x-done-keywords)
  "Allowed keywords for meetings.")

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

(defconst org-x-exclusive-prefixes (list org-x-tag-category-prefix
                                         org-x-tag-location-prefix)
  "Tag prefixes which denote mutually exclusive groups.")

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

(defconst org-x-tag-phone
  (org-x-prepend-char org-x-tag-resource-prefix "phone")
  "Tag denoting a phone resource.")

(defconst org-x-tag-deep
  (org-x-prepend-char org-x-tag-misc-prefix "deep")
  "Tag denoting deep work.")

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

(defconst org-x-tag-meeting
  (org-x-prepend-char org-x-tag-misc-prefix "meeting")
  "Tag denoting a meeting.")

;; (defconst org-x-tag-environmental
;;   (org-x-prepend-char org-x-tag-category-prefix "env")
;;   "Tag denoting an environmental life category.")

;; (defconst org-x-tag-financial
;;   (org-x-prepend-char org-x-tag-category-prefix "fin")
;;   "Tag denoting a financial life category.")

;; (defconst org-x-tag-intellectual
;;   (org-x-prepend-char org-x-tag-category-prefix "int")
;;   "Tag denoting an intellectual life category.")

;; (defconst org-x-tag-metaphysical
;;   (org-x-prepend-char org-x-tag-category-prefix "met")
;;   "Tag denoting an metaphysical life category.")

;; (defconst org-x-tag-physical
;;   (org-x-prepend-char org-x-tag-category-prefix "phy")
;;   "Tag denoting an physical life category.")

;; (defconst org-x-tag-professional
;;   (org-x-prepend-char org-x-tag-category-prefix "pro")
;;   "Tag denoting a professional life category.")

;; (defconst org-x-tag-recreational
;;   (org-x-prepend-char org-x-tag-category-prefix "rec")
;;   "Tag denoting a recreational life category.")

;; (defconst org-x-tag-social
;;   (org-x-prepend-char org-x-tag-category-prefix "soc")
;;   "Tag denoting a social life category.")

(defconst org-x-tag-no-agenda "NA"
  "Tag denoting a headlines that shouldn't go in the agenda.")

(defconst org-x-tag-no-archive "NRXIV"
  "Tag denoting a headlines that shouldn't go in the archive.")

(defconst org-x-tag-refile "REFILE"
  "Tag denoting a headlines that are to be refiled.")

(defconst org-x-life-categories
  (->> (list "environmental"
             "financial"
             "intellectual"
             "metaphysical"
             "physical"
             "professional"
             "recreational"
             "social")
       (--map (let* ((abbr (substring it 0 3))
                     (key (intern abbr))
                     (tag (org-x-prepend-char org-x-tag-category-prefix abbr)))
                (list key :tag tag :desc it))))
  "Alist of life categories.
The car of each member is a symbol representing the category, the
cdr is a plist which has entries for :tag and :desc which are the
org tag and a long name respectively for the category.")

(defun org-x-life-category-plist-get (key category-sym)
  (plist-get (alist-get category-sym org-x-life-categories) key))

(defun org-x-life-category-tag (category-sym)
  (org-x-life-category-plist-get :tag category-sym))

(defun org-x-life-category-desc (category-sym)
  (org-x-life-category-plist-get :desc category-sym))

;;; PROPERTIES

;; all follow the nomenclature `org-x-prop-PROPNAME' (key) or
;; `org-x-prop-PROPNAME-VALNAME' (value)

(defconst org-x-prop-parent-type "PARENT_TYPE"
  "Property denoting iterator/periodical headline.")

(defconst org-x-prop-parent-type-periodical "periodical"
  "Property value for a periodical parent type.")

(defconst org-x-prop-parent-type-iterator "iterator"
  "Property value for an iterator parent type.")

(defconst org-x-prop-time-shift "TIME_SHIFT"
  "Property denoting time shift when cloning iterator/periodical headlines.")

(defconst org-x-prop-location "X-LOCATION"
  "Property denoting location for meetings.")

;; TODO this is a WIP
(defconst org-x-prop-thread "THREAD"
  "Property denoting an email thread to track.")

(defconst org-x-prop-routine "X-ROUTINE"
  "Property denoting a routine group.")

(defconst org-x-prop-routine-morning "morning"
  "Property value for morning routine.")

(defconst org-x-prop-routine-evening "evening"
  "Property value for evening routine.")

(defconst org-x-prop-created "CREATED"
  "Property denoting when a headline was created.")

(defconst org-x-prop-expire "X-EXPIRE"
  "Property denoting when a headline will expire.")

(defconst org-x-prop-days-to-live "X-DAYS_TO_LIVE"
  "Property denoting after how many days a headline will expire.")

(defconst org-x-prop-goal "X-GOAL"
  "Property denoting the goal this headline fulfills.")

(defconst org-x-prop-allocate "X-ALLOCATE"
  "Property the property denoting intended time allocation.")

;;; DRAWERS

(defconst org-x-drwr-agenda "AGENDA_ITEMS"
  "Drawer to hold agenda items in meetings.")

(defconst org-x-drwr-action "ACTION_ITEMS"
  "Drawer to hold action items in meetings.")

(defconst org-x-drwr-categories "X_CATEGORIES"
  "Drawer to hold ranked categories for a quarterly plan.")

;;; PUBLIC VARS

(defconst org-x-archive-delay 30
  "The number of days to wait before tasks are considered archivable.")

(defconst org-x-inert-delay-days 90
  "The number of days to wait before tasks are considered inert.")

(defconst org-x-iterator-active-future-offset (* 7 24 60 60)
  "Iterators must have at least one task this far in the future to be active.")

(defconst org-x-periodical-active-future-offset
  org-x-iterator-active-future-offset
  "Periodicals must have at least one heading this far in the future to be active.")

;; files and directories (all relative to `org-directory')

(defvar org-x-action-files nil
  "List of relative paths or globs that hold actions (not incubated).")

(defvar org-x-incubator-files nil
  "List of relative paths or globs that hold incubated actions.")

(defvar org-x-reference-files nil
  "List of relative paths or globs that hold referenced headlines.")

(defvar org-x-capture-file nil
  "Path to capture file.")

(defvar org-x-endpoint-goal-file nil
  "Path to endpoint goal file.")

(defvar org-x-quarterly-plan-file nil
  "Path to quarterly plan file.")

(defvar org-x-weekly-plan-file nil
  "Path to weekly plan file.")

(defvar org-x-lifetime-goal-file nil
  "Path to lifetime goal file.")

(defvar org-x-daily-plan-file nil
  "Path to daily plan file.")

(defvar org-x-meeting-archive-file nil
  "Path to meeting archive file.")

;;; INTERNAL CONSTANTS
  
;; TODO ;unscheduled should trump all
(defconst org-x--iter-statuscodes
  '(:uninit :empt :actv :project-error :unscheduled)
  "Statuscodes for iterators, arranged from high to low precedence.")
 
(defconst org-x--peri-statuscodes
  '(:uninit :empt :actv :unscheduled)
  "Statuscodes for periodicals, arranged from high to low precedence.")

(defconst org-x--project-invalid-todostates
  (list org-x-kw-wait org-x-kw-next)
  "Projects cannot have these todostates.")
  
(defconst org-x--project-skip-todostates
  (list org-x-kw-hold org-x-kw-canc)
  "These keywords override all contents within their subtrees.
Currently used to tell skip functions when they can hop over
entire subtrees to save time and ignore tasks")

(defconst org-x--first-active-ts-pattern
  '(:first :any * (:and timestamp (:or (:type 'active) (:type 'active-range))))
  "Pattern for first active timestamp to be supplied to `org-ml-match' et al.")

;; INTERNAL VARS

(defvar org-x--agenda-property-filter nil)

;; ORG-ELEMENT EXTENSIONS

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

(defun org-x-element-headline-add-created (epoch-time headline)
  "Add the CREATED property to HEADLINE.

EPOCH-TIME is an integer/float for the created time. If nil, use
the current time."
  (let ((ts (->> (or epoch-time (float-time))
                 (org-ml-unixtime-to-time-long)
                 (org-ml-build-timestamp!)
                 (org-ml-to-string))))
    (org-ml-headline-set-node-property org-x-prop-created ts headline)))

(defmacro org-x-with-file (path &rest body)
  "Open PATH and execute BODY."
  (declare (indent 1))
  `(with-current-buffer (find-file-noselect ,path)
     (save-excursion
       ,@body)))

(defun org-x-parse-file-subtrees (path which)
  "Return a list of headlines from file at PATH.
WHICH is passed to the one argument of `org-ml-parse-subtrees'."
  (org-x-with-file path
    (org-ml-parse-subtrees which)))

(defun org-x-parse-file-headlines (path which)
  "Return a list of headlines from file at PATH.
WHICH is passed to the one argument of `org-ml-parse-headlines'."
  (org-x-with-file path
    (org-ml-parse-headlines which)))

;;; ORG FILE LOCATIONS

(defun org-x--abs-org-path (path)
  "Return PATH as an absolute path string.
PATH is a assumed to be a path relative to `org-directory'.
If PATH is not relative, return nil and print a warning."
  (if (f-relative-p path)
      (f-canonical (f-join org-directory path))
    (message "WARNING: %s is not a relative path" path)))

(defun org-x--valid-org-file-p (path)
  "Return t if PATH points to a valid org file.
Valid means that it exists and ends in '.org'."
  (cond
   ((not (f-file-p path))
    (message "WARNING: %s does not exist; ignoring" path)
    nil)
   ((not (s-matches-p ".*\\.org" path))
    (message "WARNING: %s does not end with '.org'; ignoring" path)
    nil)
   (t
    t)))

(defun org-x--expand-path-list (globs)
  "Return GLOBS as expanded list of paths.
GLOBS is a list of strings to be consumed by `f-glob'. Only
expand files that end in '.org' and that exist are returned. All
members of GLOBS should be relative to `org-directory'."
  (->> (-map #'org-x--abs-org-path globs)
       (-non-nil)
       (-mapcat #'f-glob)
       (-filter #'org-x--valid-org-file-p)
       (-uniq)))

(defun org-x--expand-path (path)
  "Return PATH as an expanded path.
PATH must be relative to `org-directory' and end in '.org'."
  (-when-let (a (org-x--abs-org-path path))
    (when (org-x--valid-org-file-p a)
      a)))

(defun org-x-get-endpoint-goal-file ()
  "Return the absolute path of `org-x-endpoint-goal-file'."
  (org-x--expand-path org-x-endpoint-goal-file))

(defun org-x-get-lifetime-goal-file ()
  "Return the absolute path of `org-x-lifetime-goal-file'."
  (org-x--expand-path org-x-lifetime-goal-file))

(defun org-x-get-capture-file ()
  "Return the absolute path of `org-x-capture-file'."
  (org-x--expand-path org-x-capture-file))

(defun org-x-get-action-files ()
  "Return the absolute path of `org-x-action-files'."
  (org-x--expand-path-list org-x-action-files))

(defun org-x-get-daily-plan-file ()
  "Return the absolute path of `org-x-daily-plan-file'."
  (org-x--expand-path org-x-daily-plan-file))

(defun org-x-get-weekly-plan-file ()
  "Return the absolute path of `org-x-weekly-plan-file'."
  (org-x--expand-path org-x-weekly-plan-file))

(defun org-x-qtp-get-file ()
  "Return the absolute path of `org-x-quarterly-plan-file'."
  (org-x--expand-path org-x-quarterly-plan-file))

(defun org-x-get-incubator-files ()
  "Return the absolute path of `org-x-incubator-files'."
  (org-x--expand-path-list org-x-incubator-files))

(defun org-x-get-reference-files ()
  "Return the absolute path of `org-x-reference-files'."
  (org-x--expand-path-list org-x-reference-files))

(defun org-x-get-action-and-incubator-files ()
  "Return combined list of paths for incubator and action files."
  (append (org-x-get-action-files)
          (org-x-get-incubator-files)))

;;; STATEFUL BUFFER HEADLINE FUNCTIONS

;; All of these functions operate on the current headline

;; helper function

(defun org-x--forward-stars ()
  "Move point forward until a star is not encountered."
  (forward-char 1)
  (while (= ?* (following-char))
    (forward-char 1)))

(defun org-x--headline-get-level ()
  "Return level of the current headline.
Assumes point is at the start of a headline."
  (save-excursion
    (while (= ?* (following-char)) (forward-char 1))
    (current-column)))

(defmacro org-x--while-child-headlines (while-form &rest body)
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
     (let* ((target-level (1+ (org-x--headline-get-level)))
            (cur-level target-level))
       (while (and ,while-form
                   (<= target-level cur-level)
                   (= 0 (forward-line 1)))
         (when (= ?* (following-char))
           (org-x--forward-stars)
           (when (= 32 (following-char))
             (setq cur-level (current-column))
             (when (= cur-level target-level)
               ,@body)))))))

(defun org-x--headline-has-children (test-fun)
  "Return t if heading has a child for whom TEST-FUN is t.
Assume that point is at the beginning of a headline."
  (let ((has-children nil))
    (org-x--while-child-headlines (not has-children)
      (when (funcall test-fun)
        (setq has-children t)))
    has-children))

(defun org-x--headline-has-parent (heading-test)
  "Return t if heading has parent for whom HEADING-TEST is t."
  (save-excursion (and (org-up-heading-safe) (funcall heading-test))))

;; timestamp predicates

(defun org-x--headline-get-property-epoch-time (timestamp-property)
  "Return TIMESTAMP-PROPERTY of the current headline as an epoch time.
If TIMESTAMP-PROPERTY is missing, return nil. This will return 0
if a property is given that returns a string that isn't an org
timestamp."
  (-some-> (org-entry-get nil timestamp-property) (org-2ft)))

(defmacro org-x--headline-compare-timestamp (ref-epoch-time future epoch-time-form)
  "Compare epoch-time to some reference time.

EPOCH-TIME-FORM should return an epoch time when called on the
headline under point. Return t if epoch time is further back in
time compared to REF-EPOCH-TIME (0 is now, negative is past, and
positive is future). If the FUTURE flag is t, returns timestamp
if it is in the future compared to REF-EPOCH-TIME. Return nil if
no timestamp is found."
  (declare (indent 2))
  (let ((op (if future '> '<=)))
    `(-when-let (epoch-time ,epoch-time-form)
       (when (,op (- epoch-time (float-time)) ,ref-epoch-time)
         epoch-time))))

(defun org-x-headline-is-scheduled-p ()
  "Return non-nil if current headline has a scheduled timestamp.
Actual returned value is the epoch time of the timestamp."
  (org-x--headline-get-property-epoch-time "SCHEDULED"))

(defun org-x-headline-is-deadlined-p ()
  "Return non-nil if current headline has a deadline timestamp.
Actual returned value is the epoch time of the timestamp."
  (org-x--headline-get-property-epoch-time "DEADLINE"))

(defun org-x-headline-is-closed-p ()
  "Return non-nil if current headline has a closed timestamp.
Actual returned value is the epoch time of the timestamp."
  (org-x--headline-get-property-epoch-time "CLOSED"))

(defun org-x-headline-is-timestamped-p ()
  "Return non-nil if current headline has an active timestamp.
Actual returned value is the epoch time of the timestamp."
  (org-x--headline-get-property-epoch-time "TIMESTAMP"))

(defun org-x-headline-is-created-p ()
  "Return non-nil if current headline has a created timestamp.
Created timestamps are held in the `org-x-prop-created' property.
Actual returned value is the epoch time of the property."
  (org-x--headline-get-property-epoch-time org-x-prop-created))

(defun org-x-headline-is-stale-p ()
  "Return non-nil if current headline is stale.
'Stale' means the headline has an active timestamp in the past.
Actual returned value is the epoch time of the timestamp."
  (org-x--headline-compare-timestamp 0 nil
    (-when-let (ts (org-entry-get nil "TIMESTAMP"))
      (unless (s-matches-p "+[0-9]+[dwmy]" ts)
        (org-2ft ts)))))

(defun org-x-headline-is-expired-date-p ()
  "Return non-nil if current headline is expired.
'Expired' means the headline has an `org-x-prop-expire' property
that is in the past. Actual returned value is the epoch time of
the timestamp."
  (org-x--headline-compare-timestamp 0 nil
    (org-x--headline-get-property-epoch-time org-x-prop-expire)))

(defun org-x-headline-is-expired-days-to-live ()
  "Return non-nil if current headline is expired.
'Expired' means the headline has passed its days to live
according to the `org-x-prop-days-to-live' and
`org-x-prop-created' properties. Actual returned value is the
epoch time of the created property plus the days to live."
  (org-x--headline-compare-timestamp 0 nil
    (-when-let (dtl (org-entry-get nil org-x-prop-days-to-live))
      (when (s-matches-p "[0-9]+" dtl)
        (-when-let (et (org-x--headline-get-property-epoch-time org-x-prop-created))
          (+ et (* (string-to-number dtl) 24 60 60)))))))

(defun org-x-headline-is-expired-p ()
  "Return non-nil if current headline is expired.
This will test the current headline using
`org-x-headline-is-expired-date-p' and
`org-x-headline-is-expired-days-to-live' (in that order)."
  (or (org-x-headline-is-expired-days-to-live)
      (org-x-headline-is-expired-date-p)))

(defun org-x-headline-is-fresh-p ()
  "Return non-nil if current headline is fresh.
'Fresh' means the headline has an active timestamp in the future.
Actual returned value is the epoch time of the timestamp."
  (org-x--headline-compare-timestamp 0 t
    (org-x-headline-is-timestamped-p)))

(defun org-x-headline-is-archivable-p ()
  "Return non-nil if current headline is fresh.
'Archivable' means the headline has been closed at least
`org-x-archive-delay' days in the past. Actual returned value is
the epoch time of the timestamp."
  (org-x--headline-compare-timestamp (- (* 60 60 24 org-x-archive-delay)) nil
    (org-x-headline-is-closed-p)))

(defun org-x-headline-is-created-in-future ()
  "Return non-nil if current headline was 'created' in the future.
This should not happen and is an error if it does, and the
headline is tested analogously to `org-x-headline-is-created-p'
except tests if the timestamp is in the future. Actual returned
value is the epoch time of the timestamp."
  (org-x--headline-compare-timestamp 0 t
    (org-x-headline-is-created-p)))

(defun org-x-headline-is-inert-p ()
  "Return non-nil if current headline is inert.

'Inert means that the headline has had no activity in
`org-x-inert-delay-days' in the past. Activity is assessed using
logbook entries (clocks or items), and the headline must have
been created `org-x-inert-delay-days' in the past to be inert.
Furthermore, headlines with deadline or scheduled timestamps in
the future cannot be inert.

Actual return value is the epoch time of the most recent
timestamp."
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

;; keyword testing

(defmacro org-x--return-keyword-when (keyword when-form)
  "Return keyword under headline if WHEN-FORM is t.
If KEYWORD is non-nil, don't look up the keyword but instead
return KEYWORD if WHEN-FORM is t."
  (declare (indent 1))
  (if keyword `(and ,when-form ,keyword)
    `(-when-let (kw (org-x-headline-is-todoitem-p))
       (and ,when-form kw))))

(defalias 'org-x-headline-is-todoitem-p 'org-get-todo-state
  "Return todo keyword if current headline has one.")

(defun org-x-headline-has-discontinuous-parent ()
  "Return t if heading has a non-todoitem parent which in turn has a todoitem parent."
  (let ((has-todoitem-parent)
        (has-non-todoitem-parent))
    (save-excursion
      (while (and (not has-todoitem-parent) (org-up-heading-safe))
        (if (org-x-headline-is-todoitem-p)
            (setq has-todoitem-parent t)
          (setq has-non-todoitem-parent t))))
    (and has-todoitem-parent has-non-todoitem-parent)))

(defun org-x-headline-has-task-children ()
  "Return todo keyword of first task child under headline if it exists."
  (org-x--headline-has-children #'org-x-headline-is-todoitem-p))

(defun org-x-headline-has-task-parent ()
  "Return todo keyword of current headline's if it exists."
  (org-x--headline-has-parent #'org-x-headline-is-todoitem-p))

(defmacro org-x-headline-is-project-p (&optional keyword)
  "Return todo keyword if heading has todoitem children.

See `org-x--return-keyword-when' for meaning of KEYWORD."
  `(org-x--return-keyword-when ,keyword
     (org-x-headline-has-task-children)))

(defmacro org-x-headline-is-toplevel-project-p (&optional keyword)
  "Return todo keyword if headline has task children and no task parents.

See `org-x--return-keyword-when' for meaning of KEYWORD."
  `(org-x--return-keyword-when (org-x-headline-is-project-p ,keyword)
     (not (org-x-headline-has-task-parent))))

(defmacro org-x-headline-is-task-p (&optional keyword)
  "Return todo keyword if heading has no todoitem children.

See `org-x--return-keyword-when' for meaning of KEYWORD."
  `(org-x--return-keyword-when ,keyword
     (not (org-x-headline-has-task-children))))

(defmacro org-x-headline-is-project-task-p (&optional keyword)
  "Return todo keyword if heading has todoitem parents.

See `org-x--return-keyword-when' for meaning of KEYWORD."
  `(org-x--return-keyword-when (org-x-headline-is-task-p ,keyword)
     (org-x-headline-has-task-parent)))

(defmacro org-x-headline-is-atomic-task-p (&optional keyword)
  "Return todo keyword if heading has no todoitem parents or children.

See `org-x--return-keyword-when' for meaning of KEYWORD."
  `(org-x--return-keyword-when (org-x-headline-is-task-p ,keyword)
     (not (org-x-headline-has-task-parent))))

;; property testing

;; TODO use selective inheritence always? it might be slower
(defun org-x-headline-has-property (property value &optional inherit)
  "Return t if headline under point has PROPERTY with VALUE.
INHERIT is passed to `org-entry-get'."
  (equal value (org-entry-get nil property inherit)))

(defun org-x-headline-is-periodical-p ()
  "Return t if heading is a periodical."
  (org-x-headline-has-property org-x-prop-parent-type
                               org-x-prop-parent-type-periodical t))

(defun org-x-headline-is-iterator-p ()
  "Return t if heading is an iterator."
  (org-x-headline-has-property org-x-prop-parent-type
                               org-x-prop-parent-type-iterator t))

(defun org-x-headline-is-habit-p ()
  "Return t if heading is an iterator."
  (org-x-headline-has-property "STYLE" "habit"))

(defun org-x-headline-has-effort-p ()
  "Return t if heading has an effort."
  (org-entry-get nil org-effort-property))

;; tag testing

(defun org-x-headline-has-context-p ()
  "Return non-nil if heading has a context tag."
  (--any
   (memq (elt it 0) `(,org-x-tag-resource-prefix ,org-x-tag-location-prefix))
   (org-get-tags)))

(defun org-x-headline-has-tag-p (tag)
  "Return t if heading has tag TAG."
  (member tag (org-get-tags)))

;; compound headline testing

(defmacro org-x-headline-get-task-status (&optional keyword)
  "Return the status of the headline under point.

See `org-x--return-keyword-when' for meaning of KEYWORD."
  `(-when-let (kw (org-x-headline-is-task-p ,keyword))
     (cond
      ((org-x-headline-is-archivable-p)
       :archivable)
      ((and (not (member kw org-x-done-keywords)) (org-x-headline-is-expired-p))
       :expired)
      ((org-x-headline-is-inert-p)
       :inert)
      ((and (member kw org-x-done-keywords) (not (org-x-headline-is-closed-p)))
       :done-unclosed)
      ((and (not (member kw org-x-done-keywords)) (org-x-headline-is-closed-p))
       :undone-closed)
      ((member kw org-x-done-keywords)
       :complete)
      (t
       :active))))

(defun org-x-headline-is-discontinous-project-task-p ()
  "Return t if headline is a task with a discontinous project parent."
  (org-x--return-keyword-when (org-x-headline-is-todoitem-p)
    (org-x-headline-has-discontinuous-parent)))

(defun org-x-headline-is-done-unclosed-task-p ()
  "Return t if headline is a done unclosed task.
'Done unclosed' means it is marked with a done keyword but is
missing a closed timestamp."
  (and (member (org-get-todo-state) org-x-done-keywords)
       (not (org-x-headline-is-closed-p))
       t))

(defun org-x-headline-is-undone-closed-task-p ()
  "Return t if headline is a undone closed task.
'Undone closed' means it is not marked with a done keyword but
has closed timestamp."
  (-when-let ((keyword (org-get-todo-state)))
    (and (not (member keyword org-x-done-keywords))
         (org-x-headline-is-closed-p)
         t)))

(defun org-x-headline-is-task-without-creation-timestamp-p ()
  "Return t if headline is a task without a creation timestamp.
Creation timestamps are set using the `org-x-prop-created'
property."
  (-when-let (keyword (org-x-headline-is-task-p))
    (and (not (member keyword org-x-done-keywords))
         (not (org-x-headline-is-created-p))
         t)))

(defun org-x-headline-is-iterator-without-archive-target-p ()
  "Return t if headline is an iterator without an archive target."
  (and (org-x-headline-has-property org-x-prop-parent-type
                                    org-x-prop-parent-type-iterator)
       (org-x-headline-has-property "ARCHIVE" nil)
       t))

(defmacro org-x-headline-is-task-with-p (&rest body)
  "Return t if all of BODY is t on the current headline.
'it' is bound to the keyword (if any)."
  (declare (indent 0))
  `(-when-let (it (org-x-headline-is-task-p))
    (and ,@body t)))

(defun org-x-headline-is-task-with-future-creation-timestamp-p ()
  "Return t if current headline is undone task with missing creation timestamp."
  (org-x-headline-is-task-with-p
    (not (member it org-x-done-keywords))
    (org-x-headline-is-created-in-future)))

(defun org-x-headline-is-meeting-p ()
  "Return t if current headline is a meeting."
  (org-x-headline-is-task-with-p
    (member it org-x-meeting-keywords)
    (org-x-headline-has-tag-p org-x-tag-meeting)))

(defun org-x-headline-is-open-unscheduled-meeting-p ()
  "Return t if current headline is an unscheduled meeting."
  (org-x-headline-is-task-with-p
    (equal it org-x-kw-todo)
    (org-x-headline-has-tag-p org-x-tag-meeting)
    (not (org-x-headline-is-scheduled-p))))

(defun org-x-headline-is-open-meeting-p ()
  "Return t if current headline is a meeting."
  (org-x-headline-is-task-with-p
    (equal it org-x-kw-todo)
    (org-x-headline-has-tag-p org-x-tag-meeting)))

(defun org-x-headline-is-open-meeting-without-effort-p ()
  "Return t if current headline is a meeting with no effort property."
  (org-x-headline-is-task-with-p
    (equal it org-x-kw-todo)
    (org-x-headline-has-tag-p org-x-tag-meeting)
    (not (org-entry-get nil "Effort" nil))))

(defun org-x-headline-is-open-meeting-without-location-p ()
  "Return t if current headline is a meeting without a location."
  (org-x-headline-is-task-with-p
    (equal it org-x-kw-todo)
    (org-x-headline-has-tag-p org-x-tag-meeting)
    (not (org-entry-get nil org-x-prop-location t))))

(defun org-x-headline-is-open-meeting-with-invalid-keyword-p ()
  "Return t if current headline is a meeting with invalid keywords."
  (org-x-headline-is-task-with-p
    (not (member it org-x-meeting-keywords))
    (org-x-headline-has-tag-p org-x-tag-meeting)))

(defun org-x-headline-is-closed-meeting-p ()
  "Return t if current headline is a closed meeting."
  (org-x-headline-is-task-with-p
     (member it org-x-done-keywords)
     (org-x-headline-has-tag-p org-x-tag-meeting)))

(defun org-x-headline-get-meeting-drawer (drawer-name)
  "Return DRAWER-NAME under current headline.
If drawer is present but has no children, return 'none'. If
drawer is present and has a plain-list, return its items as a
list of nodes. If none of these conditions are true, return nil."
  (-when-let (d (->> (org-ml-parse-this-headline)
                     (org-ml-headline-get-section)
                     (--find (and (org-ml-is-type 'drawer it)
                                  (equal (org-ml-get-property :drawer-name it)
                                         drawer-name)))))
    (-if-let (n (car (org-ml-get-children d)))
        (when (org-ml-is-type 'plain-list n)
          (org-ml-get-children n))
      'none)))

(defun org-x-headline-get-meeting-agenda-items ()
  "Return the agenda items for the current headline.
See `org-x-headline-get-meeting-drawer' for rules on what is
returned."
  (org-x-headline-get-meeting-drawer org-x-drwr-agenda))

(defun org-x-headline-get-meeting-action-items ()
  "Return the action items for the current headline.
See `org-x-headline-get-meeting-drawer' for rules on what is
returned."
  (org-x-headline-get-meeting-drawer org-x-drwr-action))

(defun org-x-headline-get-meeting-unresolved-agenda-items ()
  "Return unresolved agenda items for current headline."
  (let ((items (org-x-headline-get-meeting-agenda-items)))
    (when (and items (not (eq 'none items)))
      (--remove (eq 'on (org-ml-get-property :checkbox it)) items))))

(defun org-x-headline-is-open-meeting-without-agenda-p ()
  "Return t if current headline is a meeting with no agenda."
  (org-x-headline-is-task-with-p
    (not (member it org-x-done-keywords))
    (org-x-headline-has-tag-p org-x-tag-meeting)
    (not (org-x-headline-get-meeting-agenda-items))))

(defun org-x-headline-is-closed-meeting-without-action-items-p ()
  "Return t if current headline is a meeting with no action items."
  (org-x-headline-is-task-with-p
    (member it org-x-done-keywords)
    (org-x-headline-has-tag-p org-x-tag-meeting)
    (not (org-x-headline-get-meeting-action-items))))

(defun org-x-headline-is-closed-meeting-with-unresolved-agenda-p ()
  "Return t if current headline is a meeting with unresolved agenda items."
  (org-x-headline-is-task-with-p
    (and (member it org-x-done-keywords)
         (org-x-headline-has-tag-p org-x-tag-meeting)
         (org-x-headline-get-meeting-unresolved-agenda-items))))

;; (defun org-x-is-todo-child (keyword)
;;   "Return t if current headline has a parent (at any level) with todo KEYWORD."
;;   (let ((has-keyword-parent))
;;     (save-excursion
;;       (while (and (not has-keyword-parent) (org-up-heading-safe))
;;         (when (equal keyword (org-x-headline-is-todoitem-p))
;;           (setq has-keyword-parent t))))
;;     has-keyword-parent))

;; project level testing

(defmacro org-x--compare-statuscodes (sc-list sc1 op sc2)
  "Compare position of statuscodes SC1 and SC2 in SC-LIST using operator OP."
  (declare (indent 1))
  `(,op (cl-position ,sc1 ,sc-list) (cl-position ,sc2 ,sc-list)))

(defmacro org-x--descend-into-project (statuscode-tree get-task-status callback-form)
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
        (org-x--while-child-headlines (not (eq project-status ,breaker-status))
          (setq it-kw (org-get-todo-state))
          (when it-kw
            (if (org-x--headline-has-children #'org-x-headline-is-todoitem-p)
                (progn
                  ;; If project returns an allowed status then use that.
                  ;; Otherwise look up the value in the translation table and
                  ;; return error if not found.
                  (setq new-status ,callback-form)
                  (unless (member new-status ',allowed-statuscodes)
                    (setq new-status (alist-get new-status ',trans-tbl))))
              ;; if tasks then use get-task-status to obtain status
              (setq new-status (nth ,get-task-status ',allowed-statuscodes)))
            (when (org-x--compare-statuscodes ',allowed-statuscodes
                    new-status > project-status)
              (setq project-status new-status))))
         project-status))))

(defun org-x-headline-get-project-status (&optional kw)
  "Return project heading statuscode (assumes it is indeed a project)."
  ;;
  ;; these first three are easy because they only require
  ;; testing the project headline and nothing underneath
  ;;
  ;; it does not make sense for projects to be scheduled
  (if (org-x-headline-is-scheduled-p) :scheduled-project
    (-when-let (keyword (or kw (org-get-todo-state)))
      (cond
       ;; held projects do not care what is underneath them
       ;; only need to test if they are inert
       ((equal keyword org-x-kw-hold) (if (org-x-headline-is-inert-p) :inert :held))

       ;; projects with invalid todostates are nonsense
       ((member keyword org-x--project-invalid-todostates)
        :invalid-todostate)

       ;; canceled projects can either be archivable or complete
       ;; any errors or undone tasks are irrelevant
       ((equal keyword org-x-kw-canc) (if (org-x-headline-is-archivable-p) :archivable
                                 :complete))
       
       ;;
       ;; these require descending into the project subtasks
       ;;

       ;; done projects are like canceled projects but can also be incomplete
       ((equal keyword org-x-kw-done)
        (org-x--descend-into-project
         ((:archivable)
          (:complete)
          (:done-incomplete :stuck :inert :held :wait :active
                            :scheduled-project :invalid-todostate
                            :undone-complete))
         (if (member it-kw org-x-done-keywords)
             (if (org-x-headline-is-archivable-p) 0 1)
           2)
         (org-x-headline-get-project-status it-kw)))
       
       ;; project with TODO states could be basically any status
       ((equal keyword org-x-kw-todo)
        (org-x--descend-into-project
         ((:undone-complete :complete :archivable)
          (:stuck :scheduled-project :invalid-todostate :done-incomplete)
          (:held)
          (:wait)
          (:inert)
          (:active))
         (cond
          ((and (not (member it-kw org-x-done-keywords)) (org-x-headline-is-inert-p)) 4)
          ((equal it-kw org-x-kw-todo) (if (org-x-headline-is-scheduled-p) 5 1))
          ((equal it-kw org-x-kw-hold) 2)
          ((equal it-kw org-x-kw-wait) 3)
          ((equal it-kw org-x-kw-next) 5)
          (t 0))
         (org-x-headline-get-project-status it-kw)))
       
       (t (error (concat "invalid keyword detected: " keyword)))))))

;; goals

(defvar org-x-agenda-goal-task-ids nil)
(defvar org-x-agenda-goal-endpoint-ids nil)
(defvar org-x-agenda-lifetime-ids nil)

(defun org-x-get-goal-link-id (&optional inherit)
  (-when-let (g (org-entry-get nil org-x-prop-goal inherit))
      (-if-let (i (org-x-link-get-id g))
          i
        (message "WARNING: invalid id found: %s" i))))

(defmacro org-x-with-id-target (id &rest body)
  (declare (indent 1))
  `(-when-let ((it-file . it-point) (org-id-find ,id))
     (org-x-with-file it-file
       (save-excursion
         (goto-char it-point)
         ,@body))))

(defun org-x-goal-build-link (id)
  (org-x-with-id-target id
    (let ((desc (org-get-heading t t t t)))
      (org-ml-build-link id :type "id" desc))))

(defun org-x-resolve-goal-id ()
  (-when-let (i (org-x-get-goal-link-id t))
    (org-x-with-id-target i
      (cons it-file (org-ml-parse-this-headline)))))

(defun org-x-link-get-id (s)
  (cadr (s-match "^\\[\\[id:\\(.*\\)\\]\\[.*\\]\\]$" s)))

(defun org-x-buffer-get-goal-ids (file)
  (org-x-with-file file
    (let ((acc))
      (cl-flet
          ((get-goal
            ()
            (-when-let (i (org-x-get-goal-link-id))
              (setq acc (cons i acc)))))
        ;; TODO need to return nothing if a file has a toplevel prop drawer with
        ;; a goal in it but no TODO headlines
        (goto-char (point-min))
        (get-goal)
        (while (outline-next-heading)
          (get-goal))
        acc))))

(defun org-x-get-ids-in-file (file)
  (cl-flet
      ((full-path
        (p)
        (f-canonical (f-expand p))))
    (let ((f (full-path file)))
      (->> (ht-to-alist org-id-locations)
           (--filter (equal f (full-path (cdr it))))
           (-map #'car)))))

;; TODO this is necessary since this (rather unintuitively) scans the agenda
;; files, so I need to supply my own files since these are not set
(defun org-x-update-id-locations ()
  (interactive)
  (let ((files (append (org-x-get-action-and-incubator-files)
                       (org-x-get-reference-files)
                       (list (org-x-get-endpoint-goal-file)
                             (org-x-get-lifetime-goal-file)))))
    (org-id-update-id-locations files)))

(defun org-x-update-goal-link-ids ()
  (org-x-update-id-locations)
  (setq org-x-agenda-goal-task-ids
        (-mapcat #'org-x-buffer-get-goal-ids (org-files-list))
        org-x-agenda-goal-endpoint-ids
        (org-x-buffer-get-goal-ids (org-x-get-endpoint-goal-file))
        org-x-agenda-lifetime-ids
        (org-x-get-ids-in-file (org-x-get-lifetime-goal-file))))

(defun org-x-buffer-get-id-headlines (file)
  (cl-flet
      ((is-leaf
        (headline)
        (and (org-ml-get-property :todo-keyword headline)
             (->> (org-ml-headline-get-subheadlines headline)
                  (--none? (org-ml-get-property :todo-keyword it))))))
    (org-x-with-file file
      (->> (org-ml-parse-headlines 'all)
           (-filter #'is-leaf)))))

(defun org-x-get-goal-link-property ()
  "Get the goal link under current headline."
  (-some->> (org-entry-get (point) org-x-prop-goal)
    (s-split ";")
    (--map (->> (s-trim it) (org-ml-from-string 'link)))))

(defun org-x-set-goal-link-property (ids)
  "Set the goal link property of the current headline to IDS.
Assumes point is on a valid headline or org mode file."
  (->> (-map #'org-ml-to-trimmed-string ids)
       (s-join "; ")
       (org-set-property org-x-prop-goal)))

(defmacro org-x-map-goal-link-property (form)
  (declare (indent 0))
  `(let ((it (org-x-get-goal-link-property)))
     (org-x-set-goal-link-property ,form)))

(defun org-x-add-goal-link (id title)
  "Add goal link with ID and TITLE if not under the current headline."
  (org-x-map-goal-link-property
    (let ((cur-ids (--map (org-ml-get-property :path it) it)))
      (if (member id cur-ids) it
        (-> (org-ml-build-link id :type "id" title)
            (org-ml-to-trimmed-string)
            (cons it))))))

(defun org-x-remove-goal-link (id)
  "Remove goal link with ID if under the current headline."
  (org-x-map-goal-link-property
    (--remove (equal id (org-ml-get-property :path it)) it)))

(defun org-x-get-goal-entries (keep-present? cur-ids files)
  (cl-flet*
      ((mk-entry
        (cur-ids path base hl)
        (let* ((title (org-ml-get-property :raw-value hl))
               (id (org-ml-headline-get-node-property "ID" hl))
               (is-present (and id (member id cur-ids) t)))
          (list (format "%s%-10s | %s" (if is-present "*" " ") base title)
                :title title
                :path path
                :id id
                :point (org-ml-get-property :begin hl)
                :is-present is-present)))
       (get-headlines
        (cur-ids path)
        (let ((f (f-base path)))
          (->> (org-x-buffer-get-id-headlines path)
               (--map (mk-entry cur-ids path f it)))))
       (compare-headlines
        (a b)
        (-let (((&plist :title ta :is-present pa) (cdr a))
               ((&plist :title tb :is-present pb) (cdr b)))
          (or (and pa (not pb)) (and pa pb (string< ta tb))))))
    (let ((col (->> (--mapcat (get-headlines cur-ids it) files)
                    (-sort #'compare-headlines))))
      (if keep-present? col
        (--filter (not (plist-get (cdr it) :is-present)) col)))))

(defun org-x-choose-goal (keep-present? cur-ids files)
  (let* ((col (org-x-get-goal-entries keep-present? cur-ids files))
         (res (completing-read "Goal: " col nil t)))
    (alist-get res col nil nil #'equal)))

;; TODO use the current rankings by default if desired
(defun org-x-choose-category ()
  (intern (completing-read "Category: " org-x-life-categories nil t)))

(defun org-x-set-goal-link ()
  (interactive)
  ;; TODO also add a sanity check for if we are in a goals file or not
  (ignore-errors
    (org-back-to-heading t))
  (-let* ((cur-ids (->> (org-x-get-goal-link-property)
                        (--map (org-ml-get-property :path it))))
          (files (list (org-x-get-endpoint-goal-file)
                       (org-x-get-lifetime-goal-file)))
          ((&plist :title :path :id :point :is-present)
           (org-x-choose-goal t cur-ids files)))
    (if is-present
        (progn
          (org-x-remove-goal-link id)
          (message "removed id for '%s'" title))
      (let ((target-id (if id id
                         (org-x-with-file path
                           (goto-char point)
                           (message "ID not present. Creating.")
                           (org-id-get-create)))))
        (org-x-add-goal-link target-id title)))))

(defun org-x-headline-get-category-tag ()
  (--find (s-prefix-p "_" it) (org-get-tags)))

(defun org-x-get-category-score ()
  (-when-let (c (org-x-headline-get-category-tag))
    (alist-get c org-x--qtp-weighted-categories nil nil #'equal)))

(defun org-x-lifetime-goal-get-score ()
  (let* ((p (aref (org-entry-get nil "PRIORITY") 0))
         (priority-score (if (= org-priority-highest p) 1 -1)))
    (-when-let (cat-score (org-x-get-category-score))
      (* cat-score priority-score))))

(defun org-x-endpoint-goal-get-score ()
  (unless org-x--qtp-weighted-categories
    (error "`org-x--qtp-weighted-categories' is not set"))
  (cl-flet
      ((get-link-score
        (link)
        (let ((id (org-ml-get-property :path link)))
          (org-x-with-id-target id
            (org-x-lifetime-goal-get-score)))))
    (-some->> (org-x-get-goal-link-property)
      (-map #'get-link-score)
      (-sum))))

;;; QUARTERLY PLANNING (QTP)

;; qtp state
;;
;; define a data structure to hold a "quarter" (which is just a year and a digit
;; from 1-4)

(defvar org-x--current-quarter nil
  "The currently selected quarter as a list like (YEAR QTR).")

(defun org-x-qtp-is-valid-quarter-p (quarter)
  "Return t if QUARTER is a valid quarter data structure.
Valid means it is a list like (YEAR QUARTER) where YEAR is an
integer 1970 or greater and QUARTER is an integer 1-4."
  (pcase quarter
    (`(,(and (pred integerp) (pred (lambda (x) (<= 1970 x)))) ,(or 1 2 3 4)) t)
    (_ nil)))

(defun org-x-qtp-validate-quarter (quarter)
  "Raise error if QUARTER is invalid."
  (unless (org-x-qtp-is-valid-quarter-p quarter)
    (error "Invalid quarter: %s" quarter)))

(defun org-x-qtp-time-to-quarter (time)
  "Return quarter for TIME.
TIME is anything consumed by `decode-time' (eg an integer for the
epoch time or a list of integers as returned by `current-time')."
  (-let* (((_ _ _ _ month year) (decode-time time))
          (quarter (1+ (/ (1- month) 3))))
    (list year quarter)))

(defun org-x-qtp-read-current-quarter ()
  "Return the current quarter."
  (org-x-qtp-time-to-quarter (float-time)))

(defun org-x-qtp-set-quarter (&optional quarter)
  (when quarter
    (org-x-qtp-validate-quarter quarter))
  (setq org-x--current-quarter (or quarter (org-x-qtp-read-current-quarter))))

;; quarter plan buffer
;;
;; ASSUME the plan buffer has the following structure
;; - level 1: year
;; - level 2: quarter (eg "Q1")
;; - level 3: categories
;; - level 4: specific goals under each category
;;
;; there is also a drawer under level 3 for holding the weighted category
;; rankings for quarter
;;
;; define a data structure that holds the category rankings/weights and the
;; goals as a plist with :categories and :goals keys

(defun org-x--qtp-headline-get-year (headline)
  (let ((rt (org-ml-get-property :raw-value headline)))
    (if (s-matches-p "[0-9]\\{4\\}" rt) (string-to-number rt)
      (error "Invalid year headline in quarterly plan: %s" rt))))

(defun org-x--qtp-headline-get-quarter (headline)
  (let ((rt (org-ml-get-property :raw-value headline)))
    (-if-let ((_ qt) (s-match "Q\\([0-9]\\)" rt)) (string-to-number qt)
      (error "Invalid quarter headline in quarterly plan: %s" rt))))

(defun org-x--qtp-headline-find-year (year headlines)
  (--find (= year (org-x--qtp-headline-get-year it)) headlines))

(defun org-x--qtp-headline-find-quarter (quarter headlines)
  (--find (= quarter (org-x--qtp-headline-get-quarter it)) headlines))

(defun org-x-qtp-drawer-to-categories (drawer)
  (->> (org-ml-get-children drawer)
       (org-ml-match '(plain-list item paragraph))
       (--map (->> (org-ml-get-children it)
                   (-map #'org-ml-to-string)
                   (s-join "")
                   (s-trim)
                   (intern)))))

(defun org-x--qtp-from-children (children)
  ;; ignore properties, planning, etc
  (-let* (((sec goals) (if (org-ml-is-type 'section (car children))
                           `(,(car children) ,(cdr children))
                         `(nil ,children)))
          (cats (-some->> sec
                  (--find (org-x--is-drawer-with-name org-x-drwr-categories it))
                  (org-x-qtp-drawer-to-categories))))
    (list :categories cats :goals goals)))

(defun org-x--qtp-to-children (qt-plan)
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

(defun org-x-qtp-get (quarter)
  (org-x-with-file (org-x-qtp-get-file)
    (-let (((year qnum) quarter))
      (->> (org-ml-parse-subtrees 'all)
           (org-x--qtp-headline-find-year year)
           (org-ml-headline-get-subheadlines)
           (org-x--qtp-headline-find-quarter qnum)
           (org-ml-get-children)
           (org-x--qtp-from-children)))))

(defun org-x-qtp-set (quarter qt-plan)
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
              (children (org-x--qtp-to-children qt-plan)))
        (-if-let (st-yr (org-x--qtp-headline-find-year year sts))
            (-if-let (st-qt (->> (org-ml-headline-get-subheadlines st-yr)
                                 (org-x--qtp-headline-find-quarter qnum)))
                (org-ml-update* (org-ml-set-children children it) st-qt)
              (org-ml-update*
                (-snoc it (build-qt-headline qnum children))
                st-yr))
          (let ((end (1+ (org-ml-get-property :end (-last-item sts)))))
            (org-ml-insert end (build-yr-headline year qnum children))))))))

;; TODO some of these repeated args will be eval'd more than once

;; TODO this is basically the same pattern as what I have in org-ml (which in
;; turn is basically the same as 'deriving (Functor)' in Haskell)

(defmacro org-x-qtp-map (quarter form)
  (declare (indent 1))
  `(let ((it (org-x-qtp-get ,quarter)))
     (org-x-qtp-set ,quarter ,form)))

(defun org-x--qtp-get-key (key quarter)
  (plist-get (org-x-qtp-get quarter) key))

(defun org-x--qtp-set-key (key quarter xs)
  (plist-put (org-x-qtp-get quarter) key xs))

(defun org-x-qtp-get-categories (quarter)
  (org-x--qtp-get-key :categories quarter))

(defun org-x-qtp-get-goals (quarter)
  (org-x--qtp-get-key :goals quarter))

(defun org-x-qtp-set-categories (quarter categories)
  (org-x--qtp-set-key quarter :categories categories))

(defun org-x-qtp-set-goals (quarter goals)
  (org-x--qtp-set-key quarter :goals goals))

(defmacro org-x-qtp-map-categories (quarter form)
  `(let ((it (org-x-qtp-get-categories ,quarter)))
     (org-x-qtp-set-categories ,quarter ,form)))

(defmacro org-x-qtp-map-goals (quarter form)
  `(let ((it (org-x-qtp-get-goals ,quarter)))
     (org-x-qtp-set-goals ,quarter ,form)))

(defun org-x-qtp-get-goal-category (quarter category)
  (let ((title (org-x-life-category-desc category)))
    (-some->> (org-x-qtp-get-goals quarter)
      (--find (equal (org-ml-get-property :raw-value it) title))
      (org-ml-headline-get-subheadlines))))

(defun org-x-qtp-set-goal-category (quarter category goals)
  (cl-flet
      ((sort-goal-cats
        (headlines)
        (--sort (string< (org-ml-get-property :raw-value it)
                         (org-ml-get-property :raw-value other))
                headlines)))
    (let ((title (org-x-life-category-desc category)))
      (org-x-qtp-map-goals quarter
          (-if-let (i (--find-index
                       (equal (org-ml-get-property :raw-value it) title)
                       it))
              (let ((new (org-ml-headline-set-subheadlines goals (nth i it))))
                (sort-goal-cats (-replace-at i new it)))
            (let ((h (apply #'org-ml-build-headline!
                            :level 3
                            :title-text title
                            :tags `(,(org-x-life-category-tag ,category))
                            goals)))
              (sort-goal-cats (cons h it))))))))

(defmacro org-x-qtp-map-goal-category (quarter category form)
  (declare (indent 2))
  `(let ((it (org-x-qtp-get-goal-category ,quarter ,category)))
     (org-x-qtp-set-goal-category ,quarter ,category ,form)))

(defun org-x-qtp-add-goal-headline (quarter category headline)
  (org-x-qtp-map-goal-category quarter category (cons headline it)))

(defun org-x-qtp-build-goal-headline (ids title)
  (let ((p (->> ids
                (--map (org-ml-to-trimmed-string (org-x-goal-build-link it)))
                (s-join ";"))))
    (->> (org-ml-build-headline! :level 4
                                 :title-text title
                                 :todo-keyword org-x-kw-todo)
         (org-ml-headline-set-node-property org-x-prop-goal p))))

;; TODO this accepts a list of ids but not sure if this is the best way to use
;; this functionality
(defun org-x-qtp-add-goal-id (quarter category ids title)
  (->> (org-x-qtp-build-goal-headline ids title)
       (org-x-qtp-add-goal-headline quarter category)))

(defun org-x-qt-plan-add-goal-prompt (quarter)
  (-let* ((files (list (org-x-get-endpoint-goal-file)
                       (org-x-get-lifetime-goal-file)))
          (cat (org-x-choose-category))
          ;; TODO get ids already present
          ((&plist :title :path :id :point)
           (org-x-choose-goal t nil files)))
    (let ((target-id (if id id
                       (org-x-with-file path
                         (goto-char point)
                         (message "ID not present. Creating.")
                         (org-id-get-create)))))
      (org-x-qtp-add-goal-id quarter cat (list target-id) title))))

(defun org-x-qtp-check-categories (cats)
  (seq-set-equal-p cats (-map #'car org-x-life-categories)))

(defvar org-x--qtp-weighted-categories nil
  "Categories for the currently selected quarter.")

(defun org-x-qtp-set-categegories (&optional quarter)
  (->> (or quarter (org-x-qtp-read-current-quarter))
       (org-x-qtp-get-categories)
       (setq org-x--qtp-weighted-categories)))

;; iterators

(defun org-x--clone-get-iterator-project-status (kw)
  "Get the status of a project in an iterator.
KW is the keyword of the parent."
  (cond
   ((or (org-x-headline-is-scheduled-p)
        (member kw org-x--project-invalid-todostates)) :project-error)

   ;; canceled tasks add nothing
   ((equal kw org-x-kw-canc) :empt)
   
   ;;
   ;; these require descending into the project subtasks
   ;;

   ;; done projects either add nothing (empty) or are not actually
   ;; done (project error)
   ((equal kw org-x-kw-done)
    (org-x--descend-into-project
     ((:empt)
      (:project-error :unscheduled :actv))
     (if (member it-kw org-x-done-keywords) 0 1)
     (org-x--clone-get-iterator-project-status it-kw)))
   
   ;; project with TODO states could be basically any status
   ((equal kw org-x-kw-todo)
    (org-x--descend-into-project
     ((:unscheduled :project-error)
      (:empt)
      (:actv))
     (let ((ts (org-x-headline-is-scheduled-p)))
       (cond
        ((not ts) 0)
        ((> org-x-iterator-active-future-offset (- ts (float-time))) 1)
        (t 2)))
     (org-x--clone-get-iterator-project-status it-kw)))
   
   (t (error (concat "invalid keyword detected: " kw)))))

(defun org-x-headline-get-iterator-status ()
  "Get the status of an iterator.
Allowed statuscodes are in list `nd/get-iter-statuscodes.' where
 latter codes in the list trump earlier ones."
  (let ((cur-status (first org-x--iter-statuscodes))
        (breaker-status (-last-item org-x--iter-statuscodes))
        (kw nil)
        (new-status nil)
        (ts nil))
    (org-x--while-child-headlines (not (eq cur-status breaker-status))
      (setq kw (org-x-headline-is-todoitem-p))
      (when kw
        ;; test if project or atomic task
        ;; assume that there are no todoitems above this headline
        ;; to make checking easier
        (setq
         new-status
         (if (org-x--headline-has-children 'org-x-headline-is-todoitem-p)
             (org-x--clone-get-iterator-project-status kw)
           (setq ts (or (org-x-headline-is-scheduled-p)
                        (org-x-headline-is-deadlined-p)))
           (cond
            ((member kw org-x-done-keywords) :empt)
            ((not ts) :unscheduled)
            ((< org-x-iterator-active-future-offset (- ts (float-time))) :actv)
            (t :empt))))
        (when (org-x--compare-statuscodes org-x--iter-statuscodes
                new-status > cur-status)
          (setq cur-status new-status))))
    cur-status))

;; periodicals

(defun org-x-headline-get-periodical-status ()
  "Get the status of a periodical.
Allowed statuscodes are in list `nd/get-peri-statuscodes.' where
latter codes in the list trump earlier ones."
  (cl-flet
      ((get-ts
        ()
        (-some->> (org-ml-parse-this-headline)
          (org-ml-headline-get-contents (org-x-logbook-config))
          ;; wrap in a section here because the matcher needs a single node
          ;; and not a list
          (apply #'org-ml-build-section)
          (org-ml-match org-x--first-active-ts-pattern)
          (car)
          (org-ml-timestamp-get-start-time)
          (org-ml-time-to-unixtime)))
       (new-status
        (cur-status ts)
        (let ((new (cond
                    ((not ts) :unscheduled)
                    ((< org-x-periodical-active-future-offset (- ts (float-time))) :actv)
                    (t :empt))))
          (if (org-x--compare-statuscodes org-x--peri-statuscodes new > cur-status)
              new
            cur-status))))
    (let ((cur-status (first org-x--peri-statuscodes))
          (breaker-status (-last-item org-x--peri-statuscodes)))
      (org-x--while-child-headlines (not (eq cur-status breaker-status))
        (setq cur-status (->> (get-ts) (new-status cur-status))))
      cur-status)))

;;; SKIP FUNCTIONS

;; fundumental skip functions

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
  (org-with-wide-buffer
    (-when-let (heading-tags (org-get-tags))
      (when (and (or (not pos-tags-list)
                     (-intersection pos-tags-list heading-tags))
                 (not (-intersection neg-tags-list heading-tags)))
        (org-x-skip-heading)))))

;; high-level skip functions (used in org-agenda)

(defun org-x-calendar-skip-function ()
  "Skip function for calendar view."
  (org-x-skip-headings-with-tags
   (list org-x-tag-no-agenda org-x-tag-maybe org-x-tag-refile)))

(defun org-x-goal-skip-function ()
  "Skip function for goals view.
This is similar to the task skip function (only show TODO leaf
nodes of the outline)."
  (org-with-wide-buffer
    (let ((keyword (org-get-todo-state)))
      (when (org-x-headline-is-project-p keyword)
        (org-x-skip-heading)))))

(defun org-x-task-skip-function ()
  "Skip function for task view."
  (org-with-wide-buffer
    (let ((keyword (org-get-todo-state)))
      ;; currently we assume that periodicals have no TODOs
      (cond
       ;; skip over held/canc projects
       ((and (member keyword org-x--project-skip-todostates)
             (org-x-headline-is-project-p keyword))
        (org-x-skip-subtree))
       ;; skip iterators
       ((org-x-headline-is-iterator-p)
        (org-x-skip-heading))
       ;; skip project headings
       ((org-x-headline-is-project-p keyword)
        (org-x-skip-heading))
       ;; skip canceled tasks
       ((and (equal keyword org-x-kw-canc) (org-x-headline-is-task-p keyword))
        (org-x-skip-heading))
       ;; skip habits
       ((org-x-headline-is-habit-p)
        (org-x-skip-heading))))))
  
(defun org-x-project-skip-function ()
  "Skip function for project view."
  (org-with-wide-buffer
    (cond
     ((or (org-x-headline-is-iterator-p) (org-x-headline-is-periodical-p))
      (org-x-skip-subtree))
     ((not (org-x-headline-is-project-p))
      (org-x-skip-heading))
     ((org-x--headline-has-parent
       (lambda ()
         (member (org-get-todo-state) org-x--project-skip-todostates)))
      (org-x-skip-children)))))

(defun org-x-incubator-skip-function ()
  "Skip function for incubator view."
  (org-with-wide-buffer
    (let ((keyword (org-get-todo-state)))
      (cond
       ;; skip done/canc projects
       ((and (member keyword org-done-keywords) (org-x-headline-is-project-p keyword))
        (org-x-skip-subtree))
       ;; skip project tasks
       ((org-x-headline-is-project-task-p keyword)
        (org-x-skip-heading))
       ;; skip done/canc tasks
       ((member keyword org-done-keywords)
        (org-x-skip-heading))
       ;; skip non-tasks if they don't have a timestamp
       ((not (or keyword (org-x-headline-is-timestamped-p)))
        (org-x-skip-heading))))))

(defun org-x-periodical-skip-function ()
  "Skip function for periodical view."
  (org-with-wide-buffer
    (cond
     ((not (org-x-headline-is-periodical-p))
      (org-x-skip-heading))
     ((org-x--headline-has-parent #'org-x-headline-is-periodical-p)
      (org-x-skip-children)))))

(defun org-x-iterator-skip-function ()
  "Skip function for iterator view."
  (org-with-wide-buffer
    (cond
     ((not (org-x-headline-is-iterator-p))
      (org-x-skip-heading))
     ((org-x--headline-has-parent #'org-x-headline-is-iterator-p)
      (org-x-skip-children)))))

(defun org-x-error-skip-function ()
  "Skip function for critical error view."
  (org-with-wide-buffer
    (cond
     ((org-x-headline-is-habit-p)
      (org-x-skip-heading))
     ((org-x-headline-is-periodical-p)
      (org-x-skip-subtree)))))

(defun org-x-archive-skip-function ()
  "Skip function for archive view."
  (org-with-wide-buffer
    (let ((keyword (org-get-todo-state)))
      (cond
       ;; skip all non-archivable projects
       ((and (org-x-headline-is-project-p keyword)
             (not (eq :archivable (org-x-headline-get-project-status))))
        (org-x-skip-subtree))
       ;; skip all incubator tasks
       ((org-x-headline-has-tag-p org-x-tag-incubated)
        (org-x-skip-heading))
       ;; skip all project tasks
       ((and (org-x-headline-is-project-task-p keyword))
        (org-x-skip-heading))
       ;; skip all tasks not marked done or archivable
       ((and (org-x-headline-is-task-p keyword)
             (not (eq :archivable (org-x-headline-get-task-status))))
        (org-x-skip-heading))
       ;; skip all non-todoitems that are not stale
       ((and (not keyword) (not (org-x-headline-is-stale-p)))
        (org-x-skip-heading))))))

;;; INTERACTIVE BUFFER FUNCTIONS

;; meeting

(defun org-x--is-drawer-with-name (name node)
  "Return t is NODE is a drawer named NAME."
  (and (org-ml-is-type 'drawer node)
       (equal (org-ml-get-property :drawer-name node) name)))

;; TODO try to make agenda come before action
(defun org-x--headline-meeting-add-link (dname checkbox name)
  "Add a linked headline to drawer with DNAME under the current headline.
Only ID links are considered. Headline must be a meeting (tagged
with proper todo keywords). If CHECKBOX is non-nil, add item with
an empty checkbox. NAME is used as an extra identifier in the
minibuffer."
  (if (not (org-x-headline-is-meeting-p))
      (message "Not in a meeting headline")
    (-if-let (id-alist (->> org-stored-links
                            (--map (cons (car it) (nth 1 it)))
                            (--filter (s-prefix-p "id:" (car it)))
                            (--map (cons (format "%s: %s" (cdr it) (car it))
                                         (list (car it) (cdr it))))))
        ;; ASSUME this will never return nil due to required read
        (-let* (((id desc) (-> (format "%s Link: " name)
                               (completing-read id-alist nil t)
                               (alist-get id-alist nil nil #'equal)))
                (item* (->> (org-ml-build-link id desc)
                            (org-ml-build-paragraph)
                            (org-ml-build-item :checkbox (when checkbox 'off)))))
          (org-ml-update-this-headline*
            (org-ml-headline-map-contents* (org-x-logbook-config)
              (-if-let (i (--find-index (org-x--is-drawer-with-name dname it) it))
                  (let ((drawer* (org-ml-map-children*
                                   (-let* (((all &as x . xs) it))
                                     (if (org-ml-is-type 'plain-list x)
                                         (cons (org-ml-map-children*
                                                 (-snoc it item*)
                                                 x)
                                               xs)
                                       all))
                                   (nth i it))))
                    (-replace-at i drawer* it))
                (let ((drawer* (->> (org-ml-build-plain-list item*)
                                    (org-ml-build-drawer dname))))
                  (cons drawer* it)))
              it))
          (setq org-stored-links (delq (assoc id org-stored-links)
                                       org-stored-links)))
      (message "No stored IDs to insert"))))

(defun org-x-headline-meeting-add-agenda-item ()
  "Add a link to headline in agenda items for current headline."
  (interactive)
  (org-x--headline-meeting-add-link org-x-drwr-agenda t "Agenda Item"))

(defun org-x-headline-meeting-add-action-item ()
  "Add a link to headline in action items for current headline."
  (interactive)
  (org-x--headline-meeting-add-link org-x-drwr-action nil "Action Item"))

(defun org-x-id-store-link (arg &optional interactive)
  "Make and ID for the current headline and store it in the org link ring.
ARG and INTERACTIVE are passed to `org-store-link'."
  (interactive "P\np")
  (org-id-store-link)
  (org-store-link arg interactive))

;; metablox

(defun org-x-metablock-get-timestamp (node)
  "Return the first timestamp of NODE or nil if not found."
  (car (org-ml-match '(:first section paragraph timestamp) node)))

(defun org-x-get-future-metablox ()
  "Return a list of headline nodes representing metablocks in the future."
  (cl-flet
      ((is-future
        (node)
        (-some->> (org-x-metablock-get-timestamp node)
          (org-ml-timestamp-get-start-time)
          (org-ml-time-to-unixtime)
          (< (float-time)))))
    (->> (org-x-parse-file-headlines (org-x-get-daily-plan-file) 'all)
         (--filter (null (org-ml-headline-get-subheadlines it)))
         (-filter #'is-future)
         (-map #'org-ml-remove-parents))))

;; TODO mark lines in the completion buffer that already have a link with the
;; ID we are inserting
(defun org-x-id-store-link-metablock ()
  "Make and ID for the current headline and store it in the org link ring.
ARG and INTERACTIVE are passed to `org-store-link'."
  (interactive)
  (cl-flet
      ((to-menu-line
        (node)
        (let ((ts (->> (org-x-metablock-get-timestamp node)
                       (org-ml-get-property :raw-value)))
              (title (org-ml-get-property :raw-value node)))
          (format "%s | %s" ts title))))
    (-if-let (hls (org-x-get-future-metablox))
        (-if-let (desc (-some->> (org-ml-parse-this-headline)
                         (org-ml-get-property :raw-value)))
            (-if-let (path (org-id-store-link))
                (let* ((lines (-map #'to-menu-line hls))
                       (col (-zip-pair lines hls))
                       (sel (completing-read "Metablock: " col nil t))
                       (target (alist-get sel col nil nil #'equal))
                       (link (org-ml-build-link path desc))
                       ;; ASSUME there will be one paragraph at the end holding
                       ;; the timestamp
                       (para (car (org-ml-match '(:last section paragraph) target))))
                  (org-x-with-file (org-x-get-daily-plan-file)
                    (org-ml~update* nil
                      (org-ml-map-children* (-snoc it link) it)
                      para))
                  (message "Successfully added '%s' to block '%s'" desc sel))
              (message "Could not get link to store"))
          (message "Could not get link description (not on headline?)"))
      (message "No metablocks available"))))
    
;; meeting agenda

(defun org-x--get-meetings-from-buffer ()
  "Return meeting agenda items from the current buffer."
  (cl-labels
      ((has-meeting-tag
        (headline)
        (org-ml-headline-has-tag org-x-tag-meeting headline))
       (has-parent-meeting
        (headline)
        (-when-let (p (org-ml-get-property :parent headline))
          (or (has-meeting-tag p) (has-parent-meeting p))))
       (is-task
        (headline)
        (when (org-ml-get-property :todo-keyword headline)
          (->> (org-ml-headline-get-subheadlines headline)
               (--any (org-ml-get-property :todo-keyword it))
               (not))))
       (is-meeting
        (headline)
          (and (is-task headline)
               (or (has-meeting-tag headline)
                   (has-parent-meeting headline)))))
    (->> (org-ml-parse-headlines 'all)
         (-filter #'is-meeting))))

(defun org-x--make-agenda-metaitem (headline is-closed ts item)
  (list :meeting-closed-p is-closed
        :meeting-timestamp ts
        :meeting-node (org-ml-remove-parents headline)
        :item-desc (org-ml-item-get-paragraph item)
        :item-closed (eq 'on (org-ml-get-property :checkbox item))))

(defun org-x--meeting-get-agenda-items (headline)
  "Return agenda items for HEADLINE."
  (-let ((first (->> (org-ml-headline-get-contents (org-x-logbook-config) headline)
                     (--find (org-x--is-drawer-with-name org-x-drwr-agenda it))
                     (org-ml-get-children)
                     (car)))
         (is-closed (and (member (org-ml-get-property :todo-keyword headline)
                                 org-x-done-keywords)
                         t))
         (ts (-some->> (org-ml-headline-get-planning headline)
               (org-ml-get-property :scheduled)
               (org-ml-timestamp-get-start-time)
               (org-ml-time-to-unixtime))))
    (when (org-ml-is-type 'plain-list first)
      (->> (org-ml-get-children first)
           (--map (org-x--make-agenda-metaitem headline is-closed ts it))))))

(defun org-x--metaitem-get-link-target (mi)
  (-let (((&plist :item-desc) mi))
    (-some->> (--find (org-ml-is-type 'link it) item-desc)
      (org-ml-get-property :path))))

(defun org-x--group-agenda-metaitems-by-link-target (mis)
  (->> (-group-by #'org-x--metaitem-get-link-target mis)
       (--remove (not (car it)))))

(defun org-x--metaitem-is-open (mi)
  (not (plist-get mi :item-closed)))

(defun org-x--metaitems-are-unresolved (grouped-mis)
  (-let* ((now (float-time))
          ((target . mis) grouped-mis)
          ((past future) (--separate
                          (< (plist-get it :meeting-timestamp) now)
                          mis)))
    (-when-let (most-recent (-last-item past))
      (and (org-x--metaitem-is-open most-recent)
           (-none? #'org-x--metaitem-is-open future)
           (list :item-target target
                 :item-headline (plist-get most-recent :meeting-node))))))

(defun org-x--metaitems-get-unresolved-link-targets (mis)
  (->> (org-x--group-agenda-metaitems-by-link-target mis)
       (-map #'org-x--metaitems-are-unresolved)
       (-non-nil)))

(defun org-x--id-parse-headline (id)
  "Return the headline node for ID."
  (save-excursion
    (-let (((file . offset) (org-id-find id)))
      (with-current-buffer (find-file-noselect file)
        (goto-char offset)
        (org-ml-parse-this-headline)))))

(defun org-x--group-unresolved-links (ls)
  "Return links and headlines plist LS grouped by headline offset."
  (->> (--group-by (plist-get it :item-headline) ls)
       (--map
        (-let (((key . rest) it))
          (->> rest
               (--map (org-x--id-parse-headline (plist-get it :item-target)))
               (cons key))))))

;; timestamp shifting

(defun org-x--read-number-from-minibuffer (prompt &optional return-str)
  "Read a number from the minibuffer using PROMPT.
If RETURN-STR is t, return the string and not the number."
  (let ((out (read-string (format "%s: " prompt))))
    (if (s-matches-p "[0-9]+" out)
        (if return-str out (string-to-number out))
      (error "Not a valid number: %s" out))))

(defun org-x--read-shift-from-minibuffer (&optional default)
  "Read a timestamp shift from the minibuffer.

If DEFAULT is a string, process this instead of reading a string
from the minubuffer.

Valid shifts are like +/-(DIGIT)(UNIT) (eg like '+1w') similar to
`org-clone-subtree-with-time-shift'. If invalid throw an error.
Else return a list like (OFFSET UNIT) where OFFSET is the numeric
value of the shift (negative goes back in time) and UNIT is the
unit of the shift. These are later consumed by
`org-ml-timestamp-shift'"
  (let* ((out (or default (read-from-minibuffer "Date shift (e.g. +1w): ")))
         (match (s-match "\\`[ \t]*\\([\\+\\-]?[0-9]+\\)\\([MHdwmy]\\)[ \t]*\\'" out)))
    (if (not match) (error "Invalid shift: %s" out)
      (-let* (((_ mag unit) match)
              ((mult unit*) (pcase unit
                              ("M" '(1 minute))
                              ("H" '(1 hour))
                              ("d" '(1 day))
                              ("w" '(7 day))
                              ("m" '(1 month))
                              ("y" '(1 year))
                              (_ (error "Unsupported time unit")))))
        (list (* mult (string-to-number mag)) unit*)))))

(defun org-x--reset-subtree (headline)
  "Reset HEADLINE node to incomplete state.
This includes unchecking all checkboxes, marking keywords as
\"TODO\", clearing any unique IDs, etc."
  (cl-labels
      ((reset
        (config created-ts headline)
        ;; set keyword to TODO
        (->> (org-ml-map-property* :todo-keyword
               (if (member it org-x-done-keywords) "TODO" it)
               headline)
          ;; remove logbook items and clocks
          (org-ml-headline-map-supercontents* config
            (-some->> it (org-ml-supercontents-set-logbook nil)))
          (org-ml-headline-set-node-property org-x-prop-created created-ts)
          ;; remove agenda/action items (don't bother checking if a meeting)
          (org-ml-headline-map-contents* (org-x-logbook-config)
            (-some->> it
              (--remove (org-x--is-drawer-with-name org-x-drwr-action it))
              (--remove (org-x--is-drawer-with-name org-x-drwr-agenda it))))
          ;; remove CLOSED planning entry
          (org-ml-headline-map-planning*
            (-some->> it (org-ml-planning-set-timestamp! :closed nil)))
          ;; clear item checkboxes
          (org-ml-match-map* '(section :any * item)
            (org-ml-set-property :checkbox 'off it))
          ;; update stats cookie; this obviously will be wrong if I ever want to
          ;; use TODO statistics but at least they will be reset to zero
          (org-ml-headline-update-item-statistics)
          ;; rinse and repeat for subheadlines
          (org-ml-headline-map-subheadlines*
            (--map (reset config created-ts it) it)))))
    (let ((created-ts (-> (float-time)
                          (org-ml-unixtime-to-time-long)
                          (org-ml-build-timestamp!)
                          (org-ml-to-string))))
      (reset (org-x-logbook-config) created-ts headline))))

(defun org-x--subtree-shift-timestamps (offset unit subtree)
  "Return SUBTREE with timestamps shifted OFFSET UNITs.
In the case of task headlines, only scheduled/deadlined
timestamps will be shifted. Otherwise only the first active
timestamp in the contents of the headline will be shifted."
  (cl-labels
      ((shift-timestamps
        (offset unit subtree)
        (let ((kw (org-ml-get-property :todo-keyword subtree)))
          (cond
           ((null kw)
            (org-ml-headline-map-contents* (org-x-logbook-config)
              ;; wrap in a section here because the matcher needs a single node
              ;; and not a list
              (->> (apply #'org-ml-build-section it)
                   (org-ml-match-map* org-x--first-active-ts-pattern
                     (org-ml-timestamp-shift offset unit it))
                   (org-ml-get-children))
              subtree))
           ((member kw org-x-done-keywords)
            subtree)
           (t
            (org-ml-headline-map-planning*
              (-some->> it
                (org-ml-map-property* :scheduled
                  (when it (org-ml-timestamp-shift offset unit it)))
                (org-ml-map-property* :deadline
                  (when it (org-ml-timestamp-shift offset unit it))))
              subtree)))))
       (shift
        (offset unit subtree)
        (->> (shift-timestamps offset unit subtree)
             (org-ml-headline-map-subheadlines*
               (--map (shift offset unit it) it)))))
    (shift offset unit subtree)))

(defun org-x--subtree-repeat-shifted (n offset unit headline)
  "Return HEADLINE repeated and shifted by OFFSET UNITs N times."
  (->> (org-ml-clone-node-n n headline)
       (--map-indexed (org-x--subtree-shift-timestamps
                       (* offset (1+ it-index)) unit it))
       (--map (org-ml-headline-set-node-property "ID" (org-id-new) it))))

(defun org-x-clone-subtree-with-time-shift (n)
  "Like `org-clone-subtree-with-time-shift' except reset items and todos.
N is the number of clones to produce."
  (interactive "nNumber of clones to produce: ")
  (-let* ((subtree (org-ml-parse-this-subtree))
          ((offset unit) (-> (org-entry-get nil org-x-prop-time-shift 'selective)
                             (org-x--read-shift-from-minibuffer)))
          (ins (->> (org-x--reset-subtree subtree)
                    (org-x--subtree-repeat-shifted n offset unit)
                    (-map #'org-ml-to-string)
                    (s-join "")))
          (end (org-ml-get-property :end subtree)))
    (org-ml-insert end ins)))

(defun org-x-clone-subtree-with-time-shift-toplevel (n)
  "Like `org-clone-subtree-with-time-shift' except reset items and todos.
N is the number of clones to produce."
  (interactive "nNumber of clones to produce: ")
  (-let (((offset unit) (-> (org-entry-get nil org-x-prop-time-shift 'selective)
                          (org-x--read-shift-from-minibuffer))))
    (org-ml-update-this-subtree*
      (let ((new (->> (org-ml-headline-get-subheadlines it)
                   (-last-item)
                   (org-x--reset-subtree))))
        (org-ml-map-children*
          (append it (org-x--subtree-repeat-shifted n offset unit new))
          it)))))

(defun org-x-subtree-shift-timestamps ()
  "Shift all timestamps in the current subtree.
Only deadline/scheduled timestamp are shifted (tasks) or the
first active timestamp in the contents (non-tasks)."
  (interactive)
  (-let (((offset unit) (org-x--read-shift-from-minibuffer)))
    (org-ml-update-this-subtree*
      (org-x--subtree-shift-timestamps offset unit it))))

;; marking subtrees

;; put this in terms of org-ml
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
        (let ((keyword (org-x-headline-is-todoitem-p)))
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
  (let ((config (org-x-logbook-config)))
    (org-ml-update-this-headline*
      (->> (org-ml-headline-set-logbook-clocks config nil it)
        (org-ml-headline-set-logbook-items config nil)))))

(defun org-x-clock-range (&optional arg)
  "Add a completed clock entry to the current heading.
Does not touch the running clock. When called with one prefix
ARG, ask for a range in minutes in place of the second date."
  (interactive "P")
  (cl-flet
      ((read-date
        (default-time)
        (round (float-time (org-read-date t t nil nil default-time))))
       (read-duration
        (start-epoch)
        (->> (org-x--read-number-from-minibuffer "Length in minutes")
          (* 60)
          (+ start-epoch))))
    (let* ((t1 (read-date nil))
           (t2 (if (equal arg '(4)) (read-duration t1) (read-date t1))))
      (if (< t2 t1) (message "Second timestamp earlier than first!")
        (let ((s (org-ml-unixtime-to-time-long t1))
              (e (org-ml-unixtime-to-time-long t2)))
          (org-ml-update-this-headline*
            (org-ml-headline-map-logbook-clocks* (org-x-logbook-config)
              (let ((new-clock (org-ml-build-clock! s :end e)))
                (if (org-ml-clock-is-running (car it))
                    `(,(car it) ,new-clock ,@(cdr it))
                  (cons new-clock it)))
              it)))))))

(defun org-x--headline-add-archive-context (afile apath acat atags headline)
  "Add archive context properties to HEADLINE.
AFILE is the source file. APATH is the headline path in the
original buffer. ACAT is the category. ATAGS is a list of tags,
including those that are inherited."
  (let ((atodo (org-ml-get-property :todo-keyword headline))
        (atime (-> (substring (cdr org-time-stamp-formats) 1 -1)
                 (format-time-string))))
    (->> (org-ml-clone-node headline)
      (org-ml-headline-set-node-property "ARCHIVE_TIME" atime)
      (org-ml-headline-set-node-property "ARCHIVE_FILE" afile)
      (org-ml-headline-set-node-property "ARCHIVE_OLPATH" apath)
      (org-ml-headline-set-node-property "ARCHIVE_CATEGORY" acat)
      (org-ml-headline-set-node-property "ARCHIVE_TODO" atodo)
      (org-ml-headline-set-node-property "ARCHIVE_ITAGS" atags))))

(defun org-x-refile-logbook ()
  "Refile the current headline with it's logbook.
The original target headline is left in place but without the
logbook. Intended use is for habits and repeating tasks that
build up massive logbook entries that will make my org files huge
and slow."
  (interactive)
  (let* ((acat (org-get-category))
         (afile (abbreviate-file-name
                 (or (buffer-file-name (buffer-base-buffer))
                     (error "No file associated to buffer"))))
         (apath (s-join "/" (org-get-outline-path)))
         (atags (->> (org-get-tags)
                     (--filter (get-text-property 0 'inherited it))
                     (s-join " ")))
         (add-context (-partial #'org-x--headline-add-archive-context
                                afile apath acat atags))
         (target (format "%s_archive" afile)))
    (cl-flet
        ((archive
          (target headline)
          (let* ((level-shift (-some-> (org-ml-get-property :level headline)
                                (1-)
                                (-)))
                 (headline*
                  (->> (funcall add-context headline)
                       ;; remove the ID property if it exists
                       (org-ml-headline-set-node-property "ID" nil)
                       ;; close the headline (assume it isn't already)
                       (org-ml-set-property :todo-keyword org-x-kw-done)
                       (org-ml-headline-map-planning*
                         (let ((time (org-ml-unixtime-to-time-long (float-time))))
                           (org-ml-planning-set-timestamp! :closed time it)))
                       ;; shift it to the top level
                       (org-ml-shift-property :level level-shift)
                       (org-ml-match-map* '(:any * headline)
                         (org-ml-shift-property :level level-shift it)))))
            ;; TODO this currently does not refile under specific headlines
            (with-current-buffer (find-file-noselect target)
              (org-ml-insert (point-max) headline*)))))
      (org-ml~update-this-subtree* nil
        (progn
          (archive target it)
          (org-ml-headline-map-supercontents* (org-x-logbook-config)
            (org-ml-supercontents-set-logbook nil it)
            it))))))

(defun org-x-delete-subtree ()
  "Delete entire subtree under point without sending to kill ring."
  (interactive)
  (org-back-to-heading t)
  (delete-region (point) (1+ (save-excursion (org-end-of-subtree)))))

(defun org-x-set-creation-time ()
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
  (let ((np (->> (org-x--read-number-from-minibuffer "Days to live" t)
              (org-ml-build-node-property org-x-prop-days-to-live))))
    (org-ml-update-this-headline*
      (org-ml-headline-map-node-properties* (cons np it) it))))

;;; INTERACTIVE AGENDA FUNCTIONS

;; lift buffer commands into agenda context

(defmacro org-x-agenda-cmd-wrapper (update &rest body)
  "Execute BODY in context of agenda buffer.
Specifically, navigate to the original header, execute BODY, then
update the agenda buffer. If UPDATE is 'update-headline', get the
headline string and use it to update the agenda (this is only
needed when the headline changes obviously). When update is
'update-all', reload the entire buffer. When UPDATE is nil, do
nothing."
  (declare (indent 1))
  (-let* ((newhead (make-symbol "newhead"))
          (hdmarker (make-symbol "hdmarker"))
          ((update-form get-head-form)
           (cond
            ((eq update 'update-headline)
             (list `((org-agenda-change-all-lines ,newhead ,hdmarker))
                   `((setq ,newhead (org-get-heading)))))
            ((eq update 'update-all)
             (list '((org-agenda-redo))
                   nil)))))
    `(progn
       (org-agenda-check-no-diary)
       (let* ((,hdmarker (or (org-get-at-bol 'org-hd-marker)
                             (org-agenda-error)))
              (buffer (marker-buffer ,hdmarker))
              (pos (marker-position ,hdmarker))
              (inhibit-read-only t)
              ,newhead)
         (org-with-remote-undo buffer
           (with-current-buffer buffer
             (widen)
             (goto-char pos)
             (org-show-context 'agenda)
             ,@body
             ,@get-head-form)
           ,@update-form
	       (beginning-of-line 1))))))
  
(defun org-x-agenda-toggle-checkbox ()
  "Toggle checkboxes in org agenda view using `org-toggle-checkbox'."
  (interactive)
  (org-x-agenda-cmd-wrapper update-headline
    (call-interactively #'org-toggle-checkbox)))

(defun org-x-agenda-clone-subtree-with-time-shift ()
  "Apply `org-x-clone-subtree-with-time-shift' to an agenda entry.
It will clone the last entry in the selected subtree."
  (interactive)
  (org-x-agenda-cmd-wrapper update-all
    (call-interactively #'org-x-clone-subtree-with-time-shift-toplevel)))

(defun org-x-agenda-delete-subtree ()
  "Apply `org-x-delete-subtree' to an agenda entry."
  (interactive)
  (org-x-agenda-cmd-wrapper update-all
    (call-interactively #'org-x-delete-subtree)))

(defun org-x-agenda-clock-range ()
  "Apply `org-x-clock-range' to an agenda entry."
  (interactive)
  (org-x-agenda-cmd-wrapper nil
    (call-interactively #'org-x-clock-range)))

(defun org-x-agenda-id-store-link ()
  "Apply `org-x-id-store-link' to an agenda entry."
  (interactive)
  (org-x-agenda-cmd-wrapper nil
    (call-interactively #'org-x-id-store-link)))

(defun org-x-agenda-id-store-link-metablock ()
  "Apply `org-x-id-store-link-metablock' to an agenda entry."
  (interactive)
  (org-x-agenda-cmd-wrapper nil
    (call-interactively #'org-x-id-store-link-metablock)))

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
    (let ((inc (if back -1 1))
          (header-point))
      (save-excursion
        (while (and (not header-point) (= 0 (forward-line inc)))
          (when (is-valid-header)
            (setq header-point (point))))
        header-point)
      (if header-point (goto-char header-point)
        (message (if back "Cannot move up" "Cannot move down"))))))

(defun org-x-agenda-previous-heading ()
  "Go to the previous agenda heading or end of buffer."
  (interactive)
  (org-x-agenda--seek-heading t))

(defun org-x-agenda-next-heading ()
  "Go to the next agenda heading or end of buffer."
  (interactive)
  (org-x-agenda--seek-heading))

;; agenda tag filtering

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

;; agenda meeting management

(defun org-x-agenda-meeting-add-agenda-item ()
  "Add item to current agenda headline."
  (interactive)
  (org-x-agenda-cmd-wrapper nil
    (call-interactively #'org-x-headline-meeting-add-agenda-item)))

(defun org-x-agenda-meeting-add-action-item ()
  "Add item to current action headline."
  (interactive)
  (org-x-agenda-cmd-wrapper nil
    (call-interactively #'org-x-headline-meeting-add-action-item)))

;; agenda property filtering

;; The agenda buffer doesn't do property filtering out of the box. In order to
;; implement the property filter, the functions `org-agenda-filter-make-matcher'
;; and `org-agenda-filter-remove-all' need to be advised; this will add a new
;; path to check properties against some user-defined filter.

;; This allows any property filter using to be applied and removed using the
;; standard `org-agenda-filter-apply' function with the
;; `org-x--agenda-property-filter' variable. Obviously these can all be extended
;; to different filter types. Note this does not give a shiny indicator at the
;; bottom of modeline like the built-in filter does...oh well.
    
(defun org-x-agenda-filter-make-property-matcher-form (h)
 "Return form to test the presence or absence of properties H.
H is a string like +prop or -prop"
 (let* ((op (string-to-char h))
        (h (substring h 1))
        (f `(save-excursion
              (let ((m (org-get-at-bol 'org-hd-marker)))
                (with-current-buffer (marker-buffer m)
                  (goto-char m)
                  (org-entry-get nil ,h))))))
   (if (eq op ?-) `(not ,f) f)))

(defun org-x-agenda-filter-make-property-matcher (filter type &rest _args)
  "Make a property agenda filter matcher.
This will return matching matcher form for FILTER and TYPE
where TYPE is not in the regular `org-agenda-filter-make-matcher'
function. This is intended to be used as :before-until advice and
will return nil if the type is not valid (which is currently
'property')"
  (when (eq type 'property)
    (-some->> (-map #'org-x-agenda-filter-make-property-matcher-form filter)
      (cons 'and))))

(defun org-x-agenda-filter-remove-property ()
  "Remove the agenda property filter.
This is meant to be :before advice for
`org-agenda-filter-remove-all'."
  (when org-x--agenda-property-filter
    (org-agenda-remove-filter 'property)))

(defun org-x-agenda-filter-non-effort ()
  "Filter agenda by non-effort tasks."
  (interactive)
  (setq org-x--agenda-property-filter '("-Effort"))
  (org-agenda-filter-apply org-x--agenda-property-filter 'property))

(defun org-x-agenda-filter-delegate ()
  "Filter agenda by tasks with an external delegate."
  (interactive)
  (setq org-x--agenda-property-filter '("+DELEGATE"))
  (org-agenda-filter-apply org-x--agenda-property-filter 'property))

(advice-add #'org-agenda-filter-make-matcher :before-until
            #'org-x-agenda-filter-make-property-matcher)

(advice-add #'org-agenda-filter-remove-all :before
            #'org-x-agenda-filter-remove-property)

(provide 'org-x)
;;; org-x.el ends here
