;;; org-x-text-buffer-state.el --- Examples for org.el's API  -*- lexical-binding: t -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

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

;;; Code:

(require 's)
(require 'dash)
(require 'org-x)

(defmacro org-ml--with-org-env (&rest body)
  "Execute BODY in a standardized Org-mode buffer."
  `(let ((org-tags-column 20)
         (org-todo-keywords
          ;; shamelessly copies from me
          '((sequence "TODO(t/!)" "NEXT(n/!)" "|" "DONE(d/!)")
            (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "CANC(c@/!)")))
         (org-archive-tag "ARCHIVE")
         (org-lowest-priority ?C)
         (org-highest-priority ?A)
         (org-list-allow-alphabetical nil)
         (org-log-into-drawer "LOGBOOK"))
     (with-temp-buffer
       (org-mode)
       ,@body)))

(defun org-ts-to-unixtime (timestamp-string)
  "Convert TIMESTAMP-STRING to unixtime."
  (let ((decoded (org-parse-time-string timestamp-string)))
    (->> (-snoc decoded (current-time-zone))
         (apply #'encode-time)
         (float-time)
         (round))))

(defun org-x-gen-ts (offset &optional active)
  "Generate an org timestamp string.
OFFSET is the length of time from now in seconds (positive is in
the future). Make an active timestamp if ACTIVE is t."
  (-> (float-time)
    (+ offset)
    (org-ml-unixtime-to-time-long)
    (org-ml-build-timestamp! :active active)
    (org-ml-to-string)))

(defun org-x-test-parse-forms (s)
  "Evaluate forms in string S.
Forms are denoted like %(FORM)%."
  (--reduce-from (-let (((rep sform) it))
                   (s-replace rep (format "%s" (eval (read sform))) acc))
                 s
                 (s-match-strings-all "%\\((.*?)\\)%" s)))

(defmacro org-ml--with-org-buffer (string &rest body)
  "Call `org-ml--with-org-env' with BODY and STRING as the buffer."
  (let ((s (->> (if (listp string) (s-join "\n" string) string)
             (org-x-test-parse-forms))))
    `(org-ml--with-org-env
      (insert ,s)
      (goto-char (point-min))
      ,@body)))

(defmacro org-x--test-buffer-strings (name test &rest specs)
  "Run TEST form for SPECS called by toplevel NAME."
  (declare (indent 2))
  (let ((forms (->> (-partition 4 specs)
                 ;; the _op argument is just for looks to make the decl clearer
                 (--map (-let* (((title buffer _op result) it)
                                (result* (if (and (consp result)
                                                  (eq (car result) :result))
                                             (->> (cdr result)
                                               (s-join "\n")
                                               (org-x-test-parse-forms))
                                           result)))
                          `(it ,title
                             (expect (org-ml--with-org-buffer ,buffer ,test)
                                     :to-equal
                                     ,result*)))))))
    `(describe ,name ,@forms)))

(org-x--test-buffer-strings "Task status"
    (org-x-headline-get-task-status)

  "no status"
  "* headline"
  => nil

  "active"
  "* TODO headline"
  => :active

  "active (not yet expired date)"
  ("* TODO headline"
   ":PROPERTIES:"
   ":CREATED: %(org-x-gen-ts (- (* 2 24 60 60)))%"
   ":X-EXPIRE: %(org-x-gen-ts (* 1 24 60 60))%"
   ":END:")
  => :active

  "active (not yet expired dtl)"
  ("* TODO headline"
   ":PROPERTIES:"
   ":CREATED: %(org-x-gen-ts (- (* 2 24 60 60)))%"
   ":X-DAYS_TO_LIVE: 3"
   ":END:")
  => :active

  "done unclosed"
  "* DONE headline"
  => :done-unclosed

  "undone closed"
  ("* TODO headline"
   "CLOSED: %(org-x-gen-ts 0)%")
  => :undone-closed

  "complete"
  ("* DONE headline"
   "CLOSED: %(org-x-gen-ts 0)%")
  => :complete

  "archivable"
  ("* DONE headline"
   "CLOSED: %(org-x-gen-ts (- (* (1+ org-x-archive-delay) 24 60 60)))%")
  => :archivable

  "expired (date)"
  ("* TODO headline"
   ":PROPERTIES:"
   ":CREATED: %(org-x-gen-ts (- (* 2 24 60 60)))%"
   ":X-EXPIRE: %(org-x-gen-ts (- (* 1 24 60 60)))%"
   ":END:")
  => :expired

  "expired (dtl)"
  ("* TODO headline"
   ":PROPERTIES:"
   ":CREATED: %(org-x-gen-ts (- (* 2 24 60 60)))%"
   ":X-DAYS_TO_LIVE: 1"
   ":END:")
  => :expired

  "inert (created timestamp)"
  ("* TODO headline"
   ":PROPERTIES:"
   ":CREATED: %(org-x-gen-ts (- (* (1+ org-x-inert-delay-days) 24 60 60)))%"
   ":END:")
  => :inert

  "not inert (future deadline)"
  ("* TODO headline"
   "DEADLINE: %(org-x-gen-ts (* 1 24 60 60))%"
   ":PROPERTIES:"
   ":CREATED: %(org-x-gen-ts (- (* (1+ org-x-inert-delay-days) 24 60 60)))%"
   ":END:")
  => :active

  "not inert (future schedule)"
  ("* TODO headline"
   "SCHEDULED: %(org-x-gen-ts (* 1 24 60 60))%"
   ":PROPERTIES:"
   ":CREATED: %(org-x-gen-ts (- (* (1+ org-x-inert-delay-days) 24 60 60)))%"
   ":END:")
  => :active)

;; NOTE the silly thing about this function is that headlines need not actually
;; be projects :/
(org-x--test-buffer-strings "Project status"
    (org-x-headline-get-project-status)

  "scheduled"
  ("* TODO project"
   "SCHEDULED: %(org-x-gen-ts 0)%")
  => :scheduled-project

  "held"
  ("* HOLD project")
  => :held

  ;; ASSUME the inert code paths are fully tested elsewhere
  "inert"
  ("* HOLD project"
   ":PROPERTIES:"
   ":CREATED: %(org-x-gen-ts (- (* (1+ org-x-inert-delay-days) 24 60 60)))%"
   ":END:")
  => :inert

  "invalid todo (NEXT)"
  ("* NEXT project")
  => :invalid-todostate

  "invalid todo (WAIT)"
  ("* WAIT project")
  => :invalid-todostate

  "canceled (complete)"
  ("* CANC project")
  => :complete

  "canceled (archivable)"
  ("* CANC project"
   "CLOSED: %(org-x-gen-ts (- (* (1+ org-x-archive-delay) 24 60 60)))%")
  => :archivable

  "complete (subtask complete)"
  ("* DONE project"
   "CLOSED: %(org-x-gen-ts 0)%"
   "** DONE task 1"
   "CLOSED: %(org-x-gen-ts 0)%")
  => :complete

  ;; TODO this should be :complete
  "complete (subtask archivable)"
  ("* DONE project"
   "CLOSED: %(org-x-gen-ts 0)%"
   "** DONE task 1"
   "CLOSED: %(org-x-gen-ts (- (* (1+ org-x-archive-delay) 24 60 60)))%")
  => :archivable

  "archivable"
  ("* DONE project"
   "CLOSED: %(org-x-gen-ts (- (* (1+ org-x-archive-delay) 24 60 60)))%"
   "** DONE task 1"
   "CLOSED: %(org-x-gen-ts (- (* (1+ org-x-archive-delay) 24 60 60)))%")
  => :archivable

  ;; the bold thingy should make this choke unless we specifically detect stars
  ;; with a space after them
  "done-incomplete (subtask TODO)"
  ("* DONE project"
   "CLOSED: %(org-x-gen-ts (- (* (1+ org-x-archive-delay) 24 60 60)))%"
   "*bold thingy*"
   "** TODO task 1")
  => :done-incomplete

  "done-incomplete (subtask TODO)"
  ("* DONE project"
   "CLOSED: %(org-x-gen-ts (- (* (1+ org-x-archive-delay) 24 60 60)))%"
   "** TODO task 1")
  => :done-incomplete

  ;; TODO this should be an error
  "done-incomplete (subtask done-unclosed)"
  ("* DONE project"
   "CLOSED: %(org-x-gen-ts (- (* (1+ org-x-archive-delay) 24 60 60)))%"
   "** DONE task 1")
  => :complete

  ;; TODO this should be :complete
  "complete (subtask archivable nested)"
  ("* DONE project"
   "CLOSED: %(org-x-gen-ts 0)%"
   "** DONE task 1"
   "CLOSED: %(org-x-gen-ts (- (* (1+ org-x-archive-delay) 24 60 60)))%"
   "*** DONE task 2"
   "CLOSED: %(org-x-gen-ts (- (* (1+ org-x-archive-delay) 24 60 60)))%")
  => :archivable

  "archivable (nested)"
  ("* DONE project"
   "CLOSED: %(org-x-gen-ts (- (* (1+ org-x-archive-delay) 24 60 60)))%"
   "** DONE task 1"
   "CLOSED: %(org-x-gen-ts (- (* (1+ org-x-archive-delay) 24 60 60)))%"
   "*** DONE task 2"
   "CLOSED: %(org-x-gen-ts (- (* (1+ org-x-archive-delay) 24 60 60)))%")
  => :archivable

  ;; TODO there are many other paths to test for the DONE toplevel keyword

  ;; TODO this seems error-prone
  "active (singleton...???)"
  ("* TODO project")
  => :undone-complete

  "active (subtask)"
  ("* TODO project"
   "** NEXT task")
  => :active

  "wait (subtask)"
  ("* TODO project"
   "** WAIT task")
  => :wait

  "held (subtask)"
  ("* TODO project"
   "** HOLD task")
  => :held

  "inert (subtask)"
  ("* TODO project"
   "** TODO task"
   ":PROPERTIES:"
   ":CREATED: %(org-x-gen-ts (- (* (1+ org-x-inert-delay-days) 24 60 60)))%"
   ":END:")
  => :inert

  "undone-complete (subtask)"
  ("* TODO project"
   "** DONE TASK"
   "CLOSED: (org-x-gen-ts 0)")
  => :undone-complete

  "stuck (subtask)"
  ("* TODO project"
   "** TODO TASK")
  => :stuck

  "active (subtask 2)"
  ("* TODO project"
   "** TODO task 1 "
   "*** NEXT task 2")
  => :active

  "wait (subtask 2)"
  ("* TODO project"
   "** TODO task 1 "
   "*** WAIT task 2")
  => :wait

  "held (subtask 2)"
  ("* TODO project"
   "** TODO task 1"
   "*** HOLD task 2")
  => :held

  "inert (subtask 2)"
  ("* TODO project"
   "** TODO task 1"
   "*** TODO task 2"
   ":PROPERTIES:"
   ":CREATED: %(org-x-gen-ts (- (* (1+ org-x-inert-delay-days) 24 60 60)))%"
   ":END:")
  => :inert

  "undone-complete (subtask 2)"
  ("* TODO project"
   "** TODO task 1"
   "*** DONE task 2"
   "CLOSED: (org-x-gen-ts 0)")
  => :undone-complete

  "stuck (subtask 2)"
  ("* TODO project"
   "** TODO task 1"
   "*** TODO task 2")
  => :stuck)

(org-x--test-buffer-strings "Iterator status"
    (org-x-headline-get-iterator-status)

    "uninitialized"
    ("* TODO iterator"
     ":PROPERTIES:"
     ":PARENT_TYPE: iterator"
     ":END:")
    => :uninit

    "unscheduled"
    ("* TODO iterator"
     ":PROPERTIES:"
     ":PARENT_TYPE: iterator"
     ":END:"
     "** TODO sub")
    => :unscheduled

    "unscheduled (with DONE)"
    ("* TODO iterator"
     ":PROPERTIES:"
     ":PARENT_TYPE: iterator"
     ":END:"
     "** DONE sub"
     "CLOSED: %(org-x-gen-ts 0)%"
     "** TODO sub")
    => :unscheduled

    "empty"
    ("* TODO iterator"
     ":PROPERTIES:"
     ":PARENT_TYPE: iterator"
     ":END:"
     "** DONE sub"
     "CLOSED: %(org-x-gen-ts 0)%")
    => :empt

    "active"
    ("* TODO iterator"
     ":PROPERTIES:"
     ":PARENT_TYPE: iterator"
     ":END:"
     "** TODO sub"
     "SCHEDULED: %(org-x-gen-ts (+ (* 60 60 24) org-x-iterator-active-future-offset))%")
    => :actv

    "project error"
    ("* TODO iterator"
     ":PROPERTIES:"
     ":PARENT_TYPE: iterator"
     ":END:"
     "** NEXT sub"
     "*** TODO subsub"
     "SCHEDULED: %(org-x-gen-ts (1+ org-x-iterator-active-future-offset))%")
    => :project-error)

(org-x--test-buffer-strings "Periodical status"
    (org-x-headline-get-periodical-status)

    "uninitialized"
    ("* periodical"
     ":PROPERTIES:"
     ":PARENT_TYPE: periodical"
     ":END:")
    => :uninit

    "empty"
    ("* periodical"
     ":PROPERTIES:"
     ":PARENT_TYPE: periodical"
     ":END:"
     "** sub"
     "%(org-x-gen-ts 0 t)%")
    => :empt

    "active"
    ("* periodical"
     ":PROPERTIES:"
     ":PARENT_TYPE: periodical"
     ":END:"
     "** sub"
     "%(org-x-gen-ts (+ (* 60 60 24) org-x-periodical-active-future-offset) t)%")
    => :actv

    "unscheduled"
    ("* periodical"
     ":PROPERTIES:"
     ":PARENT_TYPE: periodical"
     ":END:"
     "** sub")
    => :unscheduled)


(org-x--test-buffer-strings "Conflicts"
    (->> (org-x-agg--get-timespans-from-buffer "fp")
      (org-x-agg--timespans-group-conflicts)
      ;; drop the :unixtime key from the front to make testing easier
      (--map (--map (-drop 2 it) it)))

    "no timestamps"
    ("* TODO one"
     "* TODO two")
    => nil

    "two scheduled timestamps"
    ("* TODO one"
     "SCHEDULED: [2022-01-01 Tue 12:00]"
     "* TODO two"
     "SCHEDULED: [2022-01-01 Tue 12:00]")
    => '(((:start-time (2022 1 1 12 0) :range 0 :offset 1 :filepath "fp")
          (:start-time (2022 1 1 12 0) :range 0 :offset 46 :filepath "fp")))

    "scheduled timestamps + active timestamp"
    ("* TODO one"
     "SCHEDULED: [2022-01-01 Tue 12:00]"
     "* two"
     "<2022-01-01 Tue 12:00>")
    => '(((:start-time (2022 1 1 12 0) :range 0 :offset 1 :filepath "fp")
          (:start-time (2022 1 1 12 0) :range 0 :offset 46 :filepath "fp")))

    "two scheduled timestamps (staggered)"
    ("* TODO one"
     "SCHEDULED: [2022-01-01 Tue 12:00]"
     ":PROPERTIES:"
     ":Effort: 0:30"
     ":END:"
     "* TODO two"
     "SCHEDULED: [2022-01-01 Tue 12:15]"
     ":PROPERTIES:"
     ":Effort: 0:30"
     ":END:")
    => '(((:start-time (2022 1 1 12 0) :range 1800 :offset 1 :filepath "fp")
          (:start-time (2022 1 1 12 15) :range 1800 :offset 79 :filepath "fp")))

    "scheduled + active (staggered)"
    ("* TODO one"
     "SCHEDULED: [2022-01-01 Tue 12:00]"
     ":PROPERTIES:"
     ":Effort: 0:30"
     ":END:"
     "* two"
     "<2022-01-01 Tue 12:15-12:45>")
    => '(((:start-time (2022 1 1 12 0) :range 1800 :offset 1 :filepath "fp")
          (:start-time (2022 1 1 12 15) :range 1800 :offset 79 :filepath "fp")))

    "two scheduled timestamps (non-overlapping)"
    ("* TODO one"
     "SCHEDULED: [2022-01-01 Tue 12:00]"
     ":PROPERTIES:"
     ":Effort: 0:05"
     ":END:"
     "* TODO two"
     "SCHEDULED: [2022-01-01 Tue 12:15]"
     ":PROPERTIES:"
     ":Effort: 0:05"
     ":END:")
    => nil

    "three scheduled timestamps (overlapping)"
    ("* TODO one"
     "SCHEDULED: [2022-01-01 Tue 12:00]"
     ":PROPERTIES:"
     ":Effort: 0:15"
     ":END:"
     "* TODO two"
     "SCHEDULED: [2022-01-01 Tue 12:05]"
     ":PROPERTIES:"
     ":Effort: 0:15"
     ":END:"
     "* TODO three"
     "SCHEDULED: [2022-01-01 Tue 12:10]"
     ":PROPERTIES:"
     ":Effort: 0:15"
     ":END:")
    => '(((:start-time (2022 1 1 12 0) :range 900 :offset 1 :filepath "fp")
          (:start-time (2022 1 1 12 5) :range 900 :offset 79 :filepath "fp"))
         ((:start-time (2022 1 1 12 0) :range 900 :offset 1 :filepath "fp")
          (:start-time (2022 1 1 12 10) :range 900 :offset 157 :filepath "fp"))
         ((:start-time (2022 1 1 12 5) :range 900 :offset 79 :filepath "fp")
          (:start-time (2022 1 1 12 10) :range 900 :offset 157 :filepath "fp"))))

(org-x--test-buffer-strings "Overloads"
    (->> (org-x-agg--get-timespans-from-buffer "fp")
      (org-x-agg--timespans-group-overloads)
      ;; drop the :unixtime key from the front to make testing easier
      (--map (--map (-drop 2 it) it)))

  ;; this assumes the TODO vs active ts parsing works as expected
  "no timestamps"
  ("* TODO one"
   "* TODO two")
  => nil

  "not overloaded"
  ("* TODO one"
   "SCHEDULED: [2022-01-01 Tue 00:00]"
   ":PROPERTIES:"
   ":Effort: 0:15"
   ":END:"
   "* TODO two"
   "SCHEDULED: [2022-01-01 Tue 00:00]"
   ":PROPERTIES:"
   ":Effort: 0:15"
   ":END:")
  => nil

  "overloaded (exactly 24 hours)"
  ("* TODO one"
   "SCHEDULED: [2022-01-01 Tue 00:00]"
   ":PROPERTIES:"
   ":Effort: 12h"
   ":END:"
   "* TODO two"
   "SCHEDULED: [2022-01-01 Tue 00:00]"
   ":PROPERTIES:"
   ":Effort: 12h"
   ":END:")
  => '(((:start-time (2022 1 1 0 0) :range 43200 :offset 1 :filepath "fp")
        (:start-time (2022 1 1 0 0) :range 43200 :offset 78 :filepath "fp")))

  "overloaded (over 24 hours)"
  ("* TODO one"
   "SCHEDULED: [2022-01-01 Tue 00:00]"
   ":PROPERTIES:"
   ":Effort: 12h"
   ":END:"
   "* TODO two"
   "SCHEDULED: [2022-01-01 Tue 00:00]"
   ":PROPERTIES:"
   ":Effort: 13h"
   ":END:")
  => '(((:start-time (2022 1 1 0 0) :range 43200 :offset 1 :filepath "fp")
        (:start-time (2022 1 1 0 0) :range 46800 :offset 78 :filepath "fp")))

  "overloaded (over 24 hours and split)"
  ("* TODO one"
   "SCHEDULED: [2022-01-01 Tue 00:00]"
   ":PROPERTIES:"
   ":Effort: 12h"
   ":END:"
   "* TODO two"
   "SCHEDULED: [2022-01-01 Tue 00:00]"
   ":PROPERTIES:"
   ":Effort: 25h"
   ":END:")
  => '(((:start-time (2022 1 1 0 0) :range 43200 :offset 1 :filepath "fp")
        (:start-time (2022 1 1 0 0) :range 86400 :offset 78 :filepath "fp")))

  "overloaded (over 48 hours)"
  ("* TODO one"
   "SCHEDULED: [2022-01-01 Tue 00:00]"
   ":PROPERTIES:"
   ":Effort: 12h"
   ":END:"
   "* TODO two"
   "SCHEDULED: [2022-01-01 Tue 12:00]"
   ":PROPERTIES:"
   ":Effort: 36h"
   ":END:")
  => '(((:start-time (2022 1 1 0 0) :range 43200 :offset 1 :filepath "fp")
        (:start-time (2022 1 1 12 0) :range 43200 :offset 78 :filepath "fp"))
       ((:start-time (2022 1 2 0 0) :range 86400 :offset 78 :filepath "fp"))))

(org-x--test-buffer-strings "Timestamp shifter"
    (let ((org-log-into-drawer "LOGGING")
          (org-clock-into-drawer "CLOCKING"))
      (->> (org-ml-parse-this-subtree)
        (org-x--subtree-shift-timestamps 1 'day)
        (org-ml-to-trimmed-string)))

  ;; none of these should touch anything in the logbook or properties drawer

  "task (scheduled)"
  ("* TODO headline"
   "SCHEDULED: <2020-01-01 Wed>"
   ":PROPERTIES:"
   ":CREATED:  [2019-01-01 Tue]"
   ":END:"
   ":LOGGING:"
   "- State \"DONE\"       from \"TODO\"       [2021-04-18 Sun 13:09]"
   ":END:"
   ":CLOCKING:"
   "CLOCK: [2021-04-18 Sun 13:02]--[2021-04-18 Sun 13:09] =>  0:07"
   ":END:")
  => (:result "* TODO headline"
              "SCHEDULED: <2020-01-02 Thu>"
              ":PROPERTIES:"
              ":CREATED:  [2019-01-01 Tue]"
              ":END:"
              ":LOGGING:"
              "- State \"DONE\"       from \"TODO\"       [2021-04-18 Sun 13:09]"
              ":END:"
              ":CLOCKING:"
              "CLOCK: [2021-04-18 Sun 13:02]--[2021-04-18 Sun 13:09] =>  0:07"
              ":END:")
  
  "task (deadlined)"
  ("* TODO headline"
   "DEADLINE: <2020-01-01 Wed>"
   ":PROPERTIES:"
   ":CREATED:  [2019-01-01 Tue]"
   ":END:"
   ":LOGGING:"
   "- State \"DONE\"       from \"TODO\"       [2021-04-18 Sun 13:09]"
   ":END:"
   ":CLOCKING:"
   "CLOCK: [2021-04-18 Sun 13:02]--[2021-04-18 Sun 13:09] =>  0:07"
   ":END:")
  => (:result "* TODO headline"
              "DEADLINE: <2020-01-02 Thu>"
              ":PROPERTIES:"
              ":CREATED:  [2019-01-01 Tue]"
              ":END:"
              ":LOGGING:"
              "- State \"DONE\"       from \"TODO\"       [2021-04-18 Sun 13:09]"
              ":END:"
              ":CLOCKING:"
              "CLOCK: [2021-04-18 Sun 13:02]--[2021-04-18 Sun 13:09] =>  0:07"
              ":END:")

  "task (DONE)"
  ("* DONE headline"
   "DEADLINE: <2020-01-01 Wed>"
   ":PROPERTIES:"
   ":CREATED:  [2019-01-01 Tue]"
   ":END:"
   ":LOGGING:"
   "- State \"DONE\"       from \"TODO\"       [2021-04-18 Sun 13:09]"
   ":END:"
   ":CLOCKING:"
   "CLOCK: [2021-04-18 Sun 13:02]--[2021-04-18 Sun 13:09] =>  0:07"
   ":END:")
  => (:result "* DONE headline"
              "DEADLINE: <2020-01-01 Wed>"
              ":PROPERTIES:"
              ":CREATED:  [2019-01-01 Tue]"
              ":END:"
              ":LOGGING:"
              "- State \"DONE\"       from \"TODO\"       [2021-04-18 Sun 13:09]"
              ":END:"
              ":CLOCKING:"
              "CLOCK: [2021-04-18 Sun 13:02]--[2021-04-18 Sun 13:09] =>  0:07"
              ":END:")

  "subtask"
  ("* top"
   "** TODO headline"
   "DEADLINE: <2020-01-01 Wed>"
   ":PROPERTIES:"
   ":CREATED:  [2019-01-01 Tue]"
   ":END:"
   ":LOGGING:"
   "- State \"DONE\"       from \"TODO\"       [2021-04-18 Sun 13:09]"
   ":END:"
   ":CLOCKING:"
   "CLOCK: [2021-04-18 Sun 13:02]--[2021-04-18 Sun 13:09] =>  0:07"
   ":END:")
  => (:result "* top"
              "** TODO headline"
              "DEADLINE: <2020-01-02 Thu>"
              ":PROPERTIES:"
              ":CREATED:  [2019-01-01 Tue]"
              ":END:"
              ":LOGGING:"
              "- State \"DONE\"       from \"TODO\"       [2021-04-18 Sun 13:09]"
              ":END:"
              ":CLOCKING:"
              "CLOCK: [2021-04-18 Sun 13:02]--[2021-04-18 Sun 13:09] =>  0:07"
              ":END:")

  "non-task"
  ("* headline"
   ":PROPERTIES:"
   ":CREATED:  [2019-01-01 Tue]"
   ":END:"
   ":LOGGING:"
   "- State \"DONE\"       from \"TODO\"       [2021-04-18 Sun 13:09]"
   ":END:"
   ":CLOCKING:"
   "CLOCK: [2021-04-18 Sun 13:02]--[2021-04-18 Sun 13:09] =>  0:07"
   ":END:"
   "something"
   "something more"
   "[2020-01-02 Thu]"
   "<2020-01-01 Wed>"
   "<2020-01-01 Wed>")
  => (:result "* headline"
              ":PROPERTIES:"
              ":CREATED:  [2019-01-01 Tue]"
              ":END:"
              ":LOGGING:"
              "- State \"DONE\"       from \"TODO\"       [2021-04-18 Sun 13:09]"
              ":END:"
              ":CLOCKING:"
              "CLOCK: [2021-04-18 Sun 13:02]--[2021-04-18 Sun 13:09] =>  0:07"
              ":END:"
              "something"
              "something more"
              "[2020-01-02 Thu]"
              "<2020-01-02 Thu>"
              "<2020-01-01 Wed>")

  )


(org-x--test-buffer-strings "Timestamp cloner"
    (->> (org-ml-parse-this-subtree)
      (org-x--subtree-repeat-shifted 3 1 'day)
      (-map #'org-ml-to-string)
      (s-join ""))

  "task"
  ("* TODO headline"
   "SCHEDULED: <2020-01-01 Wed>")
  => (:result "* TODO headline"
              "SCHEDULED: <2020-01-02 Thu>"
              "* TODO headline"
              "SCHEDULED: <2020-01-03 Fri>"
              "* TODO headline"
              "SCHEDULED: <2020-01-04 Sat>"
              "")

  "project"
  ("* TODO headline"
   "SCHEDULED: <2020-01-01 Wed>"
   "** TODO headline"
   "DEADLINE: <2020-02-01 Sat>")
  => (:result "* TODO headline"
              "SCHEDULED: <2020-01-02 Thu>"
              "** TODO headline"
              "DEADLINE: <2020-02-02 Sun>"
              "* TODO headline"
              "SCHEDULED: <2020-01-03 Fri>"
              "** TODO headline"
              "DEADLINE: <2020-02-03 Mon>"
              "* TODO headline"
              "SCHEDULED: <2020-01-04 Sat>"
              "** TODO headline"
              "DEADLINE: <2020-02-04 Tue>"
              ""))

(org-x--test-buffer-strings "Timestamp resetter"
    (let ((org-log-into-drawer "LOGGING")
          (org-clock-into-drawer "CLOCKING"))
      (->> (org-ml-parse-this-subtree)
        (org-x--reset-subtree)
        (org-ml-to-trimmed-string)))

  "closed task"
  ("* DONE headline [1/1]"
   "CLOSED: [2021-04-18 Sun 13:09] SCHEDULED: <2020-01-01 Wed>"
   ":PROPERTIES:"
   ":CREATED: [2019-01-01 Tue]"
   ":ID: deadbeef"
   ":END:"
   ":LOGGING:"
   "- State \"DONE\"       from \"TODO\"       [2021-04-18 Sun 13:09]"
   ":END:"
   ":CLOCKING:"
   "CLOCK: [2021-04-18 Sun 13:02]--[2021-04-18 Sun 13:09] =>  0:07"
   ":END:"
   "- [X] resetme")
  => (:result "* TODO headline [0/1]"
              "SCHEDULED: <2020-01-01 Wed>"
              ":PROPERTIES:"
              ":CREATED:  %(org-x-gen-ts 0)%"
              ":END:"
              "- [ ] resetme")

  "closed project"
  ("* TODO top"
   "** DONE headline [1/1]"
   "CLOSED: [2021-04-18 Sun 13:09] SCHEDULED: <2020-01-01 Wed>"
   ":PROPERTIES:"
   ":CREATED: [2019-01-01 Tue]"
   ":ID: deadbeef"
   ":END:"
   ":LOGGING:"
   "- State \"DONE\"       from \"TODO\"       [2021-04-18 Sun 13:09]"
   ":END:"
   ":CLOCKING:"
   "CLOCK: [2021-04-18 Sun 13:02]--[2021-04-18 Sun 13:09] =>  0:07"
   ":END:"
   "- [X] resetme")
  => (:result "* TODO top"
              ":PROPERTIES:"
              ":CREATED:  %(org-x-gen-ts 0)%"
              ":END:"
              "** TODO headline [0/1]"
              "SCHEDULED: <2020-01-01 Wed>"
              ":PROPERTIES:"
              ":CREATED:  %(org-x-gen-ts 0)%"
              ":END:"
              "- [ ] resetme"))

  ;; "closed project"
  ;; ("* DONE headline"
  ;;  "SCHEDULED: <2020-01-01 Wed>"
  ;;  "** CANC headline"
  ;;  "DEADLINE: <2020-02-01 Sat>")
  ;; => (:result "* TODO headline"
  ;;             "SCHEDULED: <2020-01-01 Wed>"
  ;;             "** TODO headline"
  ;;             "DEADLINE: <2020-02-01 Sat>")
  ;; )

(defmacro org-x--test-time-splitter-specs (&rest specs)
  (declare (indent 0))
  ;; 3 args for clarity, currently does nothing functional
  (let ((forms
         (->> (-partition 4 specs)
           (--map (-let* (((title input _useless-sugar output) it)
                          (extra '(:offset 0 :filepath "bass4urface"))
                          (input* (append input extra))
                          (output* (--map (append it extra) output)))
                    `(it ,title
                       (expect (org-x-agg--split-timespan-by-day ',input*)
                               :to-equal
                               ',output*)))))))
    `(describe "Time splitter"
       ,@forms)))


(org-x--test-time-splitter-specs
  "zero range"
  (:start-time (2021 1 1 0 0) :range 0)
  => ((:start-time (2021 1 1 0 0) :range 0))

  "1-hour range"
  (:start-time (2021 1 1 0 0) :range 3600)
  => ((:start-time (2021 1 1 0 0) :range 3600))

  "12-hour range (noon start)"
  (:start-time (2021 1 1 12 0) :range 43200)
  => ((:start-time (2021 1 1 12 0) :range 43200))

  "24-hour range (day boundary)"
  (:start-time (2021 1 1 0 0) :range 86400)
  => ((:start-time (2021 1 1 0 0) :range 86400))

  "24-hour range (noon start)"
  (:start-time (2021 1 1 12 0) :range 86400)
  => ((:start-time (2021 1 1 12 0) :range 43200)
       (:start-time (2021 1 2 0 0) :range 43200))

  "48-hour range (day boundary)"
  (:start-time (2021 1 1 0 0) :range 172800)
  => ((:start-time (2021 1 1 0 0) :range 86400)
       (:start-time (2021 1 2 0 0) :range 86400))

  "48-hour range (noon start)"
  (:start-time (2021 1 1 12 0) :range 172800)
  => ((:start-time (2021 1 1 12 0) :range 43200)
       (:start-time (2021 1 2 0 0) :range 86400)
       (:start-time (2021 1 3 0 0) :range 43200))

  "zero range (short)"
  (:start-time (2021 1 1) :range 0)
  => ((:start-time (2021 1 1) :range 0))

  "1-hour range (short)"
  (:start-time (2021 1 1) :range 3600)
  => ((:start-time (2021 1 1 0 0) :range 3600))

  "24-hour range (short)"
  (:start-time (2021 1 1) :range 86400)
  => ((:start-time (2021 1 1 0 0) :range 86400))

  "48-hour range (short)"
  (:start-time (2021 1 1) :range 172800)
  => ((:start-time (2021 1 1 0 0) :range 86400)
       (:start-time (2021 1 2 0 0) :range 86400)))

(provide 'org-x-test-buffer-state)
;;; org-x-test-buffer-state.el ends here
