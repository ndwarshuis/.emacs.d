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
(require 'either)
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

(buttercup-define-matcher :to-be-left-with (a x)
  (cl-destructuring-bind
      ((a-expr . a) (x-expr . x))
      (mapcar #'buttercup--expr-and-value (list a x))
    (either-from a
      (lambda (l)
        (if (equal l x)
            `(t . ,(format "Expected %s left with %s" a-expr x))
          `(nil . ,(format "Expected %s left with %s, but got left with %s"
                           a-expr x l))))
      (lambda ()
        `(nil . ,(format "Expected %s to be a left, got a right" a-expr))))))

(buttercup-define-matcher :to-have-same-as-plist (a b)
  (cl-destructuring-bind
      ((a-expr . a) (b-expr . b))
      (mapcar #'buttercup--expr-and-value (list a b))
    (let* ((a* (-partition 2 a))
           (b* (-partition 2 b))
           (a-diff (->> (-difference a* b*) (--map (format "%S" it)) (s-join ", ")))
           (b-diff (->> (-difference b* a*) (--map (format "%S" it)) (s-join ", "))))
      (cond
       ((and a-diff b-diff)
        (cons nil (format "Expected %s to have pairs '%s' and not to have pairs '%s'"
                          a-expr b-diff a-diff)))
       (a-diff
        (cons nil (format "Expected %s not to have pairs '%s'" a-expr a-diff)))
       (b-diff
        (cons nil (format "Expected %s to have pairs '%s'" a-expr b-diff)))
       (t
        (cons t (format "Expected %s not to have same items as '%s'"
                        a-expr b-expr)))))))

;; (defun bs-error (id left)
;;   (let ((bs (org-x-dag-id->bs id)))
;;     (expect (either-is-left-p bs))
;;     (expect (either-from-left bs nil) :to-equal left)))

;; (defun bs-action-equal (id ancestry local)
;;   (let ((bs (org-x-dag-id-bs id)))
;;     (expect (either-is-right-p bs))
;;     (from-either (org-x-dag-id-bs id)
;;   (expect (org-x-dag-id-bs id) :to-equal bs))

(describe "Sync DAG"
  ;; TODO this won't actually fail if there is an error
  (it "Sync completes without error"
    (setup)
    (org-x-dag-sync t))

  (it "One random hash is present in the dag"
    (expect (org-x-dag-id->title "06592f95-9cf5-4d7e-8546-da7796d76813")
            :to-equal
            "don't be a dick"))

  (it "test my own macros"
    (expect (either :left "blabla") :to-be-left-with "blabla"))

  (it "test my own macros"
    (expect '(:a 1) :to-have-same-as-plist '(:b 1))))

(provide 'org-x-dag-test)
;;; org-x-dag-test.el ends here
