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

(defun org-ts-to-unixtime (timestamp-string)
  "Convert TIMESTAMP-STRING to unixtime."
  (let ((decoded (org-parse-time-string timestamp-string)))
    (->> (-snoc decoded (current-time-zone))
         (apply #'encode-time)
         (float-time)
         (round))))

(defun org-x-gen-ts (offset)
  "Generate an org timestamp string.
OFFSET is the length of time from now in seconds (positive is in
the future)."
  (->> (float-time)
    (+ offset)
    (org-ml-unixtime-to-time-long)
    (org-ml-build-timestamp!)
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
    `(org-ml--with-org-env (insert ,s) ,@body)))

(defmacro org-x--test-buffer-strings (name test &rest specs)
  "Run TEST form for SPECS called by toplevel NAME."
  (declare (indent 2))
  (let ((forms (->> (-partition 4 specs)
                 ;; the _op argument is just for looks to make the decl clearer
                 (--map (-let (((title buffer _op result) it))
                          `(it ,title
                             (expect (org-ml--with-org-buffer ,buffer ,test)
                                     :to-equal
                                     ,result)))))))
    `(describe ,name ,@forms)))

(org-x--test-buffer-strings "Task status"
    (org-x-task-status)

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

(provide 'org-x-test-buffer-state)
;;; org-x-test-buffer-state.el ends here
