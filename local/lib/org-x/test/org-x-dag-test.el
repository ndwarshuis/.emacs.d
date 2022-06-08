;;; org-x-dag-test.el --- Smesh my API  -*- lexical-binding: t -*-

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

;; run tests with this (in the org-x directory above this one):

;; emacs -batch -l ../../../init.el -l test/org-x-dag-test.el -f buttercup-run

;;; Code:

(require 's)
(require 'dash)
(require 'org-x)

(defun setup ()
  (setq org-directory "test/dag"
        org-x-action-files (list "action1.org" "action2.org")
        org-x-endpoint-goal-file "endpoint.org"
        org-x-lifetime-goal-file "lifetime.org"
        org-x-survival-goal-file "survival.org"
        org-x-daily-plan-file "daily.org"
        org-x-weekly-plan-file "weekly.org"
        org-x-quarterly-plan-file "quarterly.org"))

(describe "Sync DAG"
  ;; TODO this won't actually fail if there is an error
  (it "Sync completes without error"
    (setup)
    (org-x-dag-sync t))

  (it "One random hash is present in the dag"
    (expect (org-x-dag-id->title "06592f95-9cf5-4d7e-8546-da7796d76813")
            :to-equal
            "don't be a dick")))

(provide 'org-x-dag-test)
;;; org-x-dag-test.el ends here
