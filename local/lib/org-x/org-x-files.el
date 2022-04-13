;;; org-x-files.el --- Extra Org file bits -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nathan Dwarshuis

;; Author: Nathan Dwarshuis <natedwarshuis@gmail.com>

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

(require 'f)

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

(defvar org-x-survival-goal-file nil
  "Path to survival goal file.")

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

(defun org-x-get-survival-goal-file ()
  "Return the absolute path of `org-x-survival-goal-file'."
  (org-x--expand-path org-x-survival-goal-file))

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

(provide 'org-x-files)
;;; org-x-files.el ends here
