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

(defun partition-timestamp (s)
  (->> (org-ml-from-string 'timestamp s)
       (org-x-dag-partition-timestamp)))

(defun timestamp-to-datetime (s)
  (->> (org-ml-from-string 'timestamp s)
       (org-ml-timestamp-get-start-time)))

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

(buttercup-define-matcher :to-be-right-with (a x)
  (cl-destructuring-bind
      ((a-expr . a) (x-expr . x))
      (mapcar #'buttercup--expr-and-value (list a x))
    (either-from a
      (lambda ()
        `(nil . ,(format "Expected %s to be a right, got a left" a-expr)))
      (lambda (r)
        (if (equal r x)
            `(t . ,(format "Expected %s right with %s" a-expr x))
          `(nil . ,(format "Expected %s right with %s, but got right with %s"
                           a-expr x r)))))))

(defun split-plists (eq-funs a b)
  (cl-flet
      ((get-keys
        (x)
        (->> (-partition 2 x)
             (-map #'car)))
       (key-eq
        (k)
        (let ((av (plist-get a k))
              (bv (plist-get b k))
              (f (or (plist-get eq-funs k) #'equal)))
          (funcall f av bv))))
    (let* ((a* (get-keys a))
           (b* (get-keys b))
           (a- (-difference a* b*))
           (b- (-difference b* a*))
           (common (->> (-intersection a* b*)
                        (--reduce-from (if (key-eq it) acc
                                         (cons (list it
                                                     (plist-get a it)
                                                     (plist-get b it))
                                               acc))
                                       nil))))
      `(,a- ,b- ,common))))

(defun plists-equal-p (a b)
  (equal (split-plists nil a b) '(nil nil nil)))

(defun element-equal-p (a b)
  ;; NOTE this does not compare children of elements/objects
  (cl-flet
      ((get-useful-props
        (node)
        (->> (org-ml-get-all-properties node)
             (-partition 2)
             (--remove (memq (car it) '(:parent :begin :end :contents-begin :contents-end)))
             (-flatten-n 1))))
    (and (eq (org-ml-get-type a) (org-ml-get-type b))
         (plists-equal-p (get-useful-props a) (get-useful-props b)))))

(defun plist-diff-msg (eq-funs expr a b)
  (-let (((a-diff b-diff common-diffs) (split-plists eq-funs a b)))
    (cond
     ((and a-diff b-diff)
      (format "Expected %s to have keys '%s' and not to have keys '%s'"
              expr b-diff a-diff))
     (a-diff
      (format "Expected %s not to have keys '%s'" expr a-diff))
     (b-diff
      (format "Expected %s to have keys '%s'" expr b-diff))
     (common-diffs
      (-let (((as bs)
              (->> common-diffs
                   (--map `((,(car it) ,(nth 1 it)) (,(car it) ,(nth 2 it))))
                   (apply #'-zip-lists))))
        (format "Expected %s to have key/value pairs '%s' but instead had '%s'"
                expr as bs))))))

(defun status-diff-msg (eq-funs expr type subtype data to-test)
  (-let* (((type* . rest) to-test)
          ((subtype* last) (-split-at (length subtype) rest))
          (data* (car last)))
    (cond
     ((not (eq type* type))
      (format "Expected %s to have type '%s' but instead had type '%s'"
              expr type type*))
     ((and subtype (not (equal subtype* subtype)))
      (format "Expected %s to have subtype '%s' but instead had subtype '%s'"
              expr subtype subtype*))
     (t
      (plist-diff-msg eq-funs expr data data*)))))

(defun ancestry-diff-msg (eq-funs expr ancestry inner-fun to-test)
  (declare (indent 3))
  (-let* (((&plist :ancestry A :local L) to-test))
    (or (plist-diff-msg eq-funs expr A ancestry)
        (funcall inner-fun L))))

(defun buffer-status-diff-msg (expr type inner-fun to-test)
  (declare (indent 3))
  (-let (((type* . rest) to-test))
    (if (eq type type*) (funcall inner-fun rest)
      (format "Expected buffer-status %s to be type '%s' but instead was type '%s'"
              expr type type*))))

(defun right-diff-msg (expr inner-fun to-test)
  (declare (indent 2))
  (either-from to-test
    (lambda ()
      (format "Expected %s to be a right, got a left" expr))
    inner-fun))

(buttercup-define-matcher :id-to-be-action (to-test canceled held deadline
                                                    type subtype data)
  (cl-destructuring-bind
      ((test-expr . test) (_ . c) (_ . h) (_ . e) (_ . y) (_ . s) (_ . d))
      (->> (list to-test canceled held deadline type subtype data)
           (-map #'buttercup--expr-and-value))
    (let* ((ancestry (list :canceled-parent-p c
                           :held-parent-p h
                           :parent-deadline e))
           (ancestry-eq-funs (list :parent-deadline #'element-equal-p))
           (local-eq-funs (list :sched #'element-equal-p
                                ;; TODO this is wrong
                                :child-scheds (lambda (a b)
                                                (seq-set-equal-p
                                                 a b #'element-equal-p))))
           (f (->> (-partial #'status-diff-msg local-eq-funs test-expr y s d)
                   (-partial #'ancestry-diff-msg ancestry-eq-funs test-expr ancestry)
                   (-partial #'buffer-status-diff-msg test-expr :action)
                   (-partial #'right-diff-msg test-expr))))
      (-if-let (m (funcall f (org-x-dag-id->bs test)))
        (cons nil m)
      (cons t (format "Expected '%s' not to be the indicated action" test-expr))))))

;; (buttercup-define-matcher :to-have-same-as-plist (a b)
;;   (cl-destructuring-bind
;;       ((a-expr . a) (b-expr . b))
;;       (mapcar #'buttercup--expr-and-value (list a b))
;;     (let* ((a* (-partition 2 a))
;;            (b* (-partition 2 b))
;;            (a-diff (->> (-difference a* b*) (--map (format "%S" it)) (s-join ", ")))
;;            (b-diff (->> (-difference b* a*) (--map (format "%S" it)) (s-join ", "))))
;;       (cond
;;        ((and a-diff b-diff)
;;         (cons nil (format "Expected %s to have pairs '%s' and not to have pairs '%s'"
;;                           a-expr b-diff a-diff)))
;;        (a-diff
;;         (cons nil (format "Expected %s not to have pairs '%s'" a-expr a-diff)))
;;        (b-diff
;;         (cons nil (format "Expected %s to have pairs '%s'" a-expr b-diff)))
;;        (t
;;         (cons t (format "Expected %s not to have same items as '%s'"
;;                         a-expr b-expr)))))))

(defmacro bs-ltg-active (id)
  (declare (indent 1))
  `(expect (org-x-dag-id->bs ,id) :to-be-right-with '(:lifetime :active)))

(describe "Sync DAG"
  (before-all
    (setup))

  (it "Sync completes without error"
    (expect (org-x-dag-sync t) :not :to-throw))

  (describe "Action buffer statuses"
    (describe "Projects"
      (it "Active"
        (expect "a98df83f-bc98-4767-b2bc-f1054dbf89f9" :id-to-be-action
                nil nil nil :sp-proj '(:proj-active) '(:child-scheds nil)))

      (it "Active (scheduled)"
        (let ((sched (partition-timestamp "<2022-06-10 Fri>")))
          (expect "3788c7bc-390e-4caf-af8e-06831ff3276b" :id-to-be-action
                  nil nil nil :sp-proj '(:proj-active)
                  `(:child-scheds (,sched)))))

      (it "Wait"
        (expect "26586b4d-7fc7-4a9f-b86f-e3c26a83a507" :id-to-be-action
                nil nil nil :sp-proj '(:proj-wait) nil))

      (it "Held (toplevel)"
        (expect "d5065c21-b717-41fe-8232-22afbd6b2243" :id-to-be-action
                nil nil nil :sp-proj '(:proj-held) nil))

      (it "Held (subtask)"
        (expect "a771dc18-0c5f-4196-903d-ada3c8a9d817" :id-to-be-action
                nil nil nil :sp-proj '(:proj-held) nil))

      (it "Stuck"
        (expect "c93fe96f-7130-4433-a960-98c07a3b21f4" :id-to-be-action
                nil nil nil :sp-proj '(:proj-stuck) nil))

      (it "Completed"
        (expect "87682ef6-cd4c-41a7-8f0d-6ac41e572b05" :id-to-be-action
                nil nil nil :sp-proj '(:proj-complete)
                '(:canceledp nil :epoch 1654902600)))
      
      (it "Canceled"
        (expect "eca77dea-4a40-4697-a69d-d1ec798fe9ba" :id-to-be-action
                nil nil nil :sp-proj '(:proj-complete)
                '(:canceledp t :epoch 1654902780))))

    (describe "Tasks"
      (it "Active"
        (expect "2db32ed8-0a1f-488c-8e41-dd3549ac8b1b" :id-to-be-action
                nil nil nil :sp-task '(:task-active)
                '(:todo "NEXT" :sched nil :dead nil)))

      (it "Waiting"
        (expect "cf58280a-ac7c-4951-a3de-a3f79f92f2b0" :id-to-be-action
                nil nil nil :sp-task '(:task-active)
                '(:todo "WAIT" :sched nil :dead nil)))

      (it "Held"
        (expect "4f743d31-2df4-4e32-85de-cedae0cffeb2" :id-to-be-action
                nil nil nil :sp-task '(:task-active)
                '(:todo "HOLD" :sched nil :dead nil)))

      (it "Completed"
        (expect "61866e72-7153-44d1-ae0f-af527fe5f9f4" :id-to-be-action
                nil nil nil :sp-task '(:task-complete)
                '(:canceledp nil :epoch 1654903560)))

      (it "Canceled"
        (expect "322af50a-f431-4940-8caf-cc5acdf5a555" :id-to-be-action
                nil nil nil :sp-task '(:task-complete)
                '(:canceledp t :epoch 1654903560))))

    (describe "Iterators"
      (it "Active non-empty"
        (let ((s0 (partition-timestamp "<2022-06-07 Tue>"))
              (s1 (partition-timestamp "<2022-06-14 Tue>"))
              (s2 (partition-timestamp "<2022-06-21 Tue>")))
          (expect "2711e9b9-f765-415d-930f-b7ff16b3140b" :id-to-be-action
                  nil nil nil :sp-iter '(:iter-nonempty :nonempty-active)
                  (list :child-scheds `(,s0 ,s1 ,s2)
                        :leading-sched-dt (plist-get s2 :datetime)
                        :dead nil)))))))

(provide 'org-x-dag-test)
;;; org-x-dag-test.el ends here
