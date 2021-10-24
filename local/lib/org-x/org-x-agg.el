;;; org-x-agg.el --- Org Aggregation Commands -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Nathan Dwarshuis

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

;; Functions to aggregate headlines, usually on the basis of timestamps (eg
;; finding conflicting timestamps and similar operations).

;; Most of this code was written under intense sleep deprivation at a hackathon
;; after failing to get a project off the ground. The code has been cleaned
;; since ;)

;;; Code:

(require 'org)
(require 'org-ml)
(require 'dash)

;;; CUSTOMIZABLE VARS

(defvar org-x-agg-filtered-files nil
  "Files that should be excluded from aggregation analysis.
These are pattern-matched so they do not need to be exact names
or paths.")

(defvar org-x-agg-filtered-keywords nil
  "TODO keywords that should be filtered from aggregation analysis.")

(defvar org-x-agg-filter-past t
  "Set to t to exclude files from before now in aggregation analysis.")

(defvar org-x-agg-filter-habit nil
  "Set to t to exclude habits from aggregation analysis.")

;;; INTERNAL FUNCTIONS

;; In order to aggregate headlines, get a list of metadata called 'timespans'
;; which contain the 'active' timestamp from a headline, its range, and
;; offset/file information so I can trace the timespan back to the original
;; headline. The 'active' timestamp of a headline is either the scheduled
;; timestamp if the headline is a todo item (task) or the first active timestamp
;; otherwise.

(defun org-x-agg--make-timespan (start-time range offset filepath)
  "Construct a timespan to be used in further processing.

'Timespans' are plists with the following keys:

- START-TIME is a list from `org-ml-timestamp-get-start-time'
- RANGE is the duration of the timestamp (could be 0)
- OFFSET is the character offset of the timestamp in its file
- FILEPATH is the path to the file in which the timestamp
  resides"
  (list :start-time start-time
        :range range
        :offset offset
        :filepath filepath))

(defun org-x-agg--timestamp-to-timespan (headline filepath timestamp)
  "Parse a TIMESTAMP belonging to HEADLINE in file at FILEPATH.
TIMESTAMP is an object as described in the org-element API.
Returns a new timespan."
  (when timestamp
    (let ((offset (org-element-property :begin headline))
          (start-time (org-ml-timestamp-get-start-time timestamp))
          (range (org-ml-timestamp-get-range timestamp)))
      (org-x-agg--make-timespan start-time range offset filepath))))

(defun org-x-agg--effort-to-seconds (effort-str)
  "Return EFFORT-STR as an integer in seconds."
  (-some->> effort-str (org-duration-to-minutes) (round) (* 60)))

(defun org-x-agg--headline-get-timespan-scheduled (headline filepath)
  "Return timestamp from HEADLINE in FILEPATH as timespan.
Only the scheduled timestamp is considered if any."
  (-when-let (ts (-some->> (org-ml-headline-get-planning headline)
                   (org-ml-get-property :scheduled)))
    (let* ((effort-raw (org-ml-headline-get-node-property "Effort" headline))
           (effort (if effort-raw (org-x-agg--effort-to-seconds effort-raw) 0))
           (offset (org-ml-get-property :begin headline))
           (start-time (org-ml-timestamp-get-start-time ts)))
      (org-x-agg--make-timespan start-time effort offset filepath))))

;; TODO this should be in org-ml
(defun org-x-agg--get-logbook-config ()
  "Return a config list with current logbook settings.
This is a plist meant to be consumed by
`org-ml-headline-get-supercontents' and friends."
  (list :log-into-drawer org-log-into-drawer
        :clock-into-drawer org-clock-into-drawer
        :clock-out-notes org-log-note-clock-out))

(defun org-x-agg--headline-get-timespan-active (headline filepath)
  "Return timestamp from HEADLINE in FILEPATH as timespan.
Only the first active timestamp is considered if any."
  (-some->> headline
    (org-ml-headline-get-contents (org-x-agg--get-logbook-config))
    (apply #'org-ml-build-section)
    (org-ml-match '(:first :any * (:and timestamp
                                        (:or (:type 'active)
                                             (:type 'active-range)))))
    (car)
    (org-x-agg--timestamp-to-timespan headline filepath)))

(defun org-x-agg--headline-get-timespans (headline filepath)
  "Return timestamp from HEADLINE in FILEPATH as new timespan.
If HEADLINE has a todo keyword, only consider the scheduled
timestamp. Else only consider the first active timestamp in the
contents of HEADLINE. Return nil if no timestamps exists meeting
these conditions."
  (if (org-ml-get-property :todo-keyword headline)
      (org-x-agg--headline-get-timespan-scheduled headline filepath)
    (org-x-agg--headline-get-timespan-active headline filepath)))

(defun org-x-agg--headlines-filter-keywords (headlines)
  "Return HEADLINES without keywords in `org-x-agg-filtered-keywords'."
  (if (not org-x-agg-filtered-keywords) headlines
    (--remove (member (org-element-property :todo-keyword it)
                      org-x-agg-filtered-keywords)
              headlines)))

(defun org-x-agg--filter-files (filepaths)
  "Return FILEPATHS with paths meeting a pattern filtered out."
  (if (not org-x-agg-filtered-files) filepaths
    (--remove
     (-find (lambda (s) (string-match-p s it)) org-x-agg-filtered-files)
     filepaths)))

(defun org-x-agg--timespans-remove-past (timespans)
  "Return TIMESPANS without those starting in the past."
  (if (not org-x-agg-filter-past) timespans
    (let ((ft (float-time)))
      (--remove (< (plist-get it :unixtime) ft) timespans))))

(defun org-x-agg--headlines-remove-habits (headlines)
  "Return HEADLINES without those that are habits."
  (if (not org-x-agg-filter-habit) headlines
    (--remove (org-element-property :STYLE it) headlines)))

(defun org-x-agg--get-timespans-from-buffer (filepath)
  "Return timespans for current buffer.
FILEPATH is the path to the current buffer."
  (->> (org-ml-parse-headlines 'all)
    (org-x-agg--headlines-filter-keywords)
    (org-x-agg--headlines-remove-habits)
    (--map (org-x-agg--headline-get-timespans it filepath))
    (-non-nil)))

(defun org-x-agg--get-timespans-from-file (filepath)
  "Return timespans from org-file at FILEPATH."
  (with-current-buffer (find-file-noselect filepath t)
    (org-x-agg--get-timespans-from-buffer filepath)))

(defun org-x-agg--get-all-timespans ()
  "Return a list of timespans with desired filter settings."
  (->> (org-agenda-files)
    (org-x-agg--filter-files)
    (-mapcat #'org-x-agg--get-timespans-from-file)))

(defun org-x-agg--timespans-append-unixtime (timespans)
  "Append a :unixtime property to TIMESPANS.
The new property will contain an integer representing the unix
time of the :start-time property."
  (cl-flet
      ((append-unixtime
        (span)
        `(:unixtime ,(org-ml-time-to-unixtime (plist-get span :start-time)) ,@span)))
    (-map #'append-unixtime timespans)))

;; get conflict headlines
;;
;; This algorithm builds a list of pairs, with each pair being a two tasks that
;; conflicts. It should be O(n) (best case/no conflicts) to O(n^2) (worst
;; case/everything conflicts), although this relies on the list being sorted
;; which should be O(nlog(n)) (merge sort?).
;;
;; Steps for this:
;; 1. Sort timespan list
;; 2. For each timespan:
;;    2.1 For each timespan after the current timespan
;;        If conflict found, add the two timespans being compared to new list,
;;        else break out of inner loop and advance outer loop at (2.)
;;
;; NOTE: sorting ensures that timespans can be skipped once one non-conflict is
;; found, which is what makes this algorithm approach O(n) as the number of
;; conflicts -> 0.

(defun org-x-agg--timespans-are-conflicting-p (span-a span-b)
  "Return t if timespans SPAN-A and SPAN-B conflict."
  ;; assume that a starts before b
  (let ((start-a (plist-get span-a :unixtime))
        (start-b (plist-get span-b :unixtime)))
    (or (= start-a start-b) (< start-b (+ start-a (plist-get span-a :range))))))

(defun org-x-agg--timespan-append-conflict (cur-timespan timespans conlist)
  "Test if CUR-TIMESPAN conflicts with any in TIMESPANS.
Each conflicting timespan will be paired with TIMESPAN like
\(CUR CONFLICTING) and appended to CONLIST. New CONLIST is
returned."
  (->> timespans
    (--take-while (org-x-agg--timespans-are-conflicting-p cur-timespan it))
    (--map (list cur-timespan it))
    (append conlist)))

(defun org-x-agg--timespans-build-conlist (timespans)
  "Return conflicting pairs from TIMESPANS."
  (let ((conlist))
    (while (< 1 (length timespans))
      (setq conlist (org-x-agg--timespan-append-conflict (car timespans)
                                                         (cdr timespans)
                                                         conlist)
            timespans (cdr timespans)))
    conlist))

(defun org-x-agg--timespans-group-conflicts (timespans)
  "Return TIMESPANS that conflict with each other.
The returned list will be a list of pairs of timespans
like (SPAN-a SPAN-b) which are two timespans that conflict."
  (->> (--filter (org-ml-time-is-long (plist-get it :start-time)) timespans)
    (org-x-agg--timespans-append-unixtime)
    (org-x-agg--timespans-remove-past)
    (--sort (< (plist-get it :unixtime) (plist-get other :unixtime)))
    (org-x-agg--timespans-build-conlist)))

;; get overloaded days
;;
;; Overloads are defined as days that have more than 24 hours worth of scheduled
;; material. The algorithm itself is O(n) as it is basically just a bunch of
;; filtering functions that walk through the list, but it assumes the list is
;; sorted which can be achieved with O(nlog(n)) (merge sort?).
;;
;; Steps for the algorithm:
;; 1. filter only ranged entries (unranged entries have zero time)
;; 2. split timespans if they span multiple days
;; 3. sort from earliest to latest starting time
;; 4. partition list by starting day
;; 5. sum the timespans in each day, keeping those that exceed 24 hours

(defun org-x-agg--split-timespan-by-day (timespan)
  "Split TIMESPAN if it spans multiple days."
  ;; NOTE: `encode-time' seems pretty slow but this is necessary since some
  ;; barbarians in power insist on keep daylight savings time, which means I
  ;; can't just do straight modular arithmetic to find where each day boundary
  ;; lies.
  (cl-flet
      ((encode-float-time
        (time)
        (round (float-time (encode-time time)))))
    (-let (((&plist :start-time :range :offset :filepath) timespan))
      (if (= range 0) (list timespan)
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
                (setq acc (cons (org-x-agg--make-timespan
                                 `(,y* ,m* ,d* 0 0) (- end-epoch split-epoch)
                                 offset filepath)
                                acc)
                      end-epoch split-epoch)
              (setq next nil
                    acc (cons (org-x-agg--make-timespan
                               start-time* (- end-epoch start-epoch) offset filepath)
                              acc))))
          acc)))))

(defun org-x-agg--timespans-partition-by-day (timespans)
  "Return TIMESPANS partitioned by day.
Assume TIMESPANS is sorted according to :start-time."
  (--partition-by (-take 3 (plist-get it :start-time)) timespans))

(defun org-x-agg--timespan-is-overloaded-p (timespans)
  "Return t if sum of TIMESPANS exceeds 24 hours.
It is assumed the TIMESPANS all start within one day."
  (<= 86400 (-sum (--map (plist-get it :range) timespans))))

(defun org-x-agg--timespans-group-overloads (timespans)
  "Group TIMESPANS by overloaded day.
A day is overloaded if it has timespans whose :range properties
sum to greater than 24 hours. Timespans across multiple days will
be split along day boundaries according to local time zone before
grouping is performed. Returned list will be a list of lists
like (SPAN1 SPAN2 ...) which are timespans in a single day that
is overloaded."
  (->> timespans
    (--filter (< 0 (plist-get it :range)))
    (-mapcat #'org-x-agg--split-timespan-by-day)
    (org-x-agg--timespans-append-unixtime)
    (org-x-agg--timespans-remove-past)
    (--sort (< (plist-get it :unixtime) (plist-get other :unixtime)))
    (org-x-agg--timespans-partition-by-day)
    (--filter (org-x-agg--timespan-is-overloaded-p it))))

;;; AGENDA FRONTEND

;; I could just fetch the org headings and throw them into a new buffer. But
;; that's boring, and quite limiting. I basically want all the perks of an
;; agenda buffer...tab-follow, the nice parent display at the bottom, time
;; adjust hotkeys, etc. So the obvious and hacky solution is to throw together a
;; quick-n-dirty agenda buffer.

(defun org-x-agg--get-headline-text (timespan)
  "Return string for headline text represented by TIMESPAN.
Returned string will have text properties to enable wizzy, fun
things in the agenda like jumpy to the target headline from the
agenda buffer."
  (-let* (((&plist :offset :filepath) timespan)
          (ts-marker (with-current-buffer (find-file-noselect filepath)
                       (copy-marker offset)))
          (props (list 'face nil
                       'done-face 'org-agenda-done
                       'org-not-done-regexp org-not-done-regexp
                       'org-todo-regexp org-todo-regexp
                       'org-complex-heading-regexp org-complex-heading-regexp
                       'mouse-face 'highlight))
          marker priority category level tags ts-date ts-date-pair txt
          inherited-tags)

    (with-current-buffer (marker-buffer ts-marker)
      (save-excursion
        (goto-char ts-marker)

        (setq marker (org-agenda-new-marker (point))
              category (org-get-category)
              ts-date-pair (org-agenda-entry-get-agenda-timestamp (point))
              ts-date (car ts-date-pair)
              txt (org-get-heading t)
              inherited-tags
              (or (eq org-agenda-show-inherited-tags 'always)
                  (and (listp org-agenda-show-inherited-tags)
                       (memq 'todo org-agenda-show-inherited-tags))
                  (and (eq org-agenda-show-inherited-tags t)
                       (or (eq org-agenda-use-tag-inheritance t)
                           (memq 'todo org-agenda-use-tag-inheritance))))
              tags (org-get-tags nil (not inherited-tags))
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

(defun org-x-agg--format-timespan (timespan)
  "Return formatted day-level timestamp for TIMESPAN."
  (format-time-string "[%Y-%m-%d]" (plist-get timespan :unixtime)))

(defun org-x-agg--format-conflict (grouped-timespans)
  "Return GROUPED-TIMESPANS formatted for conflict agenda buffer."
  (format "On %s\n%s\n"
          (org-x-agg--format-timespan (car grouped-timespans))
          (mapconcat #'org-x-agg--get-headline-text grouped-timespans "\n")))

(defun org-x-agg--format-overload (grouped-timespans)
  "Return GROUPED-TIMESPANS formatted for overload agenda buffer."
  (format "On %s\n%s\n"
          (org-x-agg--format-timespan (car grouped-timespans))
          (mapconcat #'org-x-agg--get-headline-text grouped-timespans "\n")))

(defun org-x-agg--show-agenda (short-name title cluster-fun format-fun arg)
  "Show an inter-headline cluster agenda buffer.
SHORT-NAME is a one-word name describing the buffer which will be
used in the name of the buffer. TITLE will be displayed at the
top of the buffer. CLUSTER-FUN is a function that takes a list of
timespans and returned a grouped list of timespans. FORMAT-FUN is
a function that takes one member from the list provided by
CLUSTER-FUN and returns a string with text properties to be
inserted into the agenda buffer. ARG is an argument provided by
some calling interactive function."
  (when org-agenda-overriding-arguments
    (setq arg org-agenda-overriding-arguments))

  (when (and (stringp arg) (not (string-match "\\S-" arg)))
    (setq arg nil))

  (let ((completion-ignore-case t)
        (org-agenda-prefix-format '((agenda . "  %-12:c %-5:e "))))

    (catch 'exit
      (when org-agenda-sticky
        (setq org-agenda-buffer-name (format "*Org %s*" short-name)))

      (org-agenda-prepare)
      (org-compile-prefix-format 'agenda)

      (setq org-agenda-redo-command '(org-x-agg-show-overloads))

      (insert (format "%s: \n" title))
      (add-text-properties (point-min) (1- (point))
                           (list 'face 'org-agenda-structure
                                 'short-heading short-name))
      (org-agenda-mark-header-line (point-min))

      (-some-> (funcall cluster-fun (org-x-agg--get-all-timespans))
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

(defun org-x-agg-show-conflicts (&optional arg)
  "Show list of conflicting headlines in agenda buffer.
ARG is something that I'm not sure if I need."
  (interactive "P")
  (org-x-agg--show-agenda "Conflicts" "Conflicting Headlines"
                          #'org-x-agg--timespans-group-conflicts
                          #'org-x-agg--format-conflict
                          arg))

(defun org-x-agg-show-overloads (&optional arg)
  "Show list of overloaded days in agenda buffer.
ARG is something that I'm not sure if I need."
  (interactive "P")
  (org-x-agg--show-agenda "Overloads" "Overloaded Days"
                          #'org-x-agg--timespans-group-overloads
                          #'org-x-agg--format-overload
                          arg))

(provide 'org-x-agg)
;;; org-x-agg.el ends here
