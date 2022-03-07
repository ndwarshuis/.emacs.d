;;; dag-test.el --- Examples for DAG API  -*- lexical-binding: t -*-

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

;; run with 'emacs --batch -l ../../../init.el -l test/dag-test.el -f ert'

;;; Code:

(require 'ht)
(require 'dash)
(require 'dag)

;; useful bits

(defun dag-test-ht-keys (h)
  (-some-> h (ht-keys)))

(defun dag-test-ht-get (h k)
  (-some-> h (ht-get k)))

(defun dag-test-sets-equal-p (a b)
  (should (seq-set-equal-p a b #'equal)))

;; DAG testing predicates
;;
;; by convention, the curated test value is always the first in binary
;; comparisons

(defun dag-test-has-valid-adjlist-p (dag adjlist-alist)
  (-let (((&plist :adjlist b) dag))
    (--each adjlist-alist
      (-let (((&plist :parents p :children c) (dag-test-ht-get b (car it)))
             ((&plist :parents p* :children c*) (cdr it)))
        (dag-test-sets-equal-p p* p)
        (dag-test-sets-equal-p c* c)))
    (dag-test-sets-equal-p (-map #'car adjlist-alist) (dag-test-ht-keys b))))

(defun dag-test-has-valid-broken-edges-p (dag broken-edges-alist)
  (-let (((&plist :broken-edges b) dag))
    (--each broken-edges-alist
      (dag-test-sets-equal-p (cdr it) (dag-test-ht-get b (car it))))
    (dag-test-sets-equal-p (-map #'car broken-edges-alist)
                           (dag-test-ht-keys b))))

;; (defun dag-test-has-valid-floating-nodes-p (dag floating-nodes)
;;   (-let (((&plist :floating-nodes f) dag))
;;     (dag-test-sets-equal-p floating-nodes (dag-test-ht-keys f))))

(defun dag-test-has-valid-order-p (dag order)
  (-let (((&plist :order o) dag))
    (should (equal order o))))

;; test macros

(defmacro dag-test-dag-is-valid-p (dag adjlist broken-edges order)
  (declare (indent 1))
  `(progn
     (dag-test-has-valid-adjlist-p ,dag ',adjlist)
     (dag-test-has-valid-broken-edges-p ,dag ',broken-edges)
     (dag-test-has-valid-order-p ,dag ',order)))

(defmacro dag-test-alist-is-valid-p (alist adjlist broken-edges order)
  (declare (indent 1))
  `(let ((dag (dag-alist-to-dag ',alist)))
     (dag-test-dag-is-valid-p dag
       ,adjlist ,broken-edges ,order)))

(defmacro dag-test-alist-remove-is-valid-p (alist to-remove adjlist broken-edges
                                                  order)
  (declare (indent 2))
  `(let ((dag (->> (dag-alist-to-dag ',alist)
                   (dag-remove-nodes ',to-remove))))
     (dag-test-dag-is-valid-p dag
       ,adjlist ,broken-edges ,order)))

(defmacro dag-test-alist-insert-is-valid-p (alist to-insert adjlist broken-edges
                                                  order)
  (declare (indent 2))
  `(let ((dag (->> (dag-alist-to-dag ',alist)
                   (dag-insert-nodes ',to-insert))))
     (dag-test-dag-is-valid-p dag
       ,adjlist ,broken-edges ,order)))

(defmacro dag-test-alist-edit-is-valid-p (alist to-remove to-insert adjlist
                                                broken-edges order)
  (declare (indent 3))
  `(let ((dag (->> (dag-alist-to-dag ',alist)
                   (dag-edit-nodes ',to-remove ',to-insert))))
     (dag-test-dag-is-valid-p dag
       ,adjlist ,broken-edges ,order)))

;; tests

(ert-deftest dag-test-null ()
  (dag-test-alist-is-valid-p nil
    nil
    nil
    nil))

(ert-deftest dag-test-one ()
  (dag-test-alist-is-valid-p ((a))
    ((a :children nil :parents nil))
    nil
    (a)))

(ert-deftest dag-test-one-cycle ()
  (dag-test-alist-is-valid-p ((a a))
    ((a :children (a) :parents (a)))
    nil
    nil))

(ert-deftest dag-test-one-broken ()
  (dag-test-alist-is-valid-p ((a b))
    ((a :children nil :parents nil))
    ((a b))
    (a)))

(ert-deftest dag-test-two ()
  (dag-test-alist-is-valid-p ((a) (b a))
    ((a :children (b) :parents nil)
     (b :children nil :parents (a)))
    nil
    (a b)))

(ert-deftest dag-test-two-cycle ()
  (dag-test-alist-is-valid-p ((a b) (b a))
    ((a :children (b) :parents (b))
     (b :children (a) :parents (a)))
    nil
    nil))

(ert-deftest dag-test-two-floating ()
  (dag-test-alist-is-valid-p ((a) (b))
    ((a :children nil :parents nil)
     (b :children nil :parents nil))
    nil
    (b a)))

(ert-deftest dag-test-two-broken ()
  (dag-test-alist-is-valid-p ((a) (b a c))
    ((a :children (b) :parents nil)
     (b :children nil :parents (a)))
    ((b c))
    (a b)))

(ert-deftest dag-test-three-linear ()
  (dag-test-alist-is-valid-p ((a) (b a) (c b))
    ((a :children (b) :parents nil)
     (b :children (c) :parents (a))
     (c :children nil :parents (b)))
    nil
    (a b c)))

(ert-deftest dag-test-three-tree ()
  (dag-test-alist-is-valid-p ((a) (b a) (c a))
    ((a :children (b c) :parents nil)
     (b :children nil :parents (a))
     (c :children nil :parents (a)))
    nil
    (a c b)))

(ert-deftest dag-test-complicated ()
  (dag-test-alist-is-valid-p ((a)
                              (b a)
                              (c a)
                              (d c b)
                              (e c b)
                              (x y)
                              (z))
    ((a :children (b c) :parents nil)
     (b :children (d e) :parents (a))
     (c :children (d e) :parents (a))
     (d :children nil :parents (b c))
     (e :children nil :parents (b c))
     (x :children nil :parents nil)
     (z :children nil :parents nil))
    ((x y))
    (z x a c b e d)))

(ert-deftest dag-test-remove ()
  (dag-test-alist-remove-is-valid-p ((a)
                                     (b a)
                                     (c a)
                                     (d c b)
                                     (e c b))
      (e)
    ((a :children (b c) :parents nil)
     (b :children (d) :parents (a))
     (c :children (d) :parents (a))
     (d :children nil :parents (b c)))
    nil
    (a c b d)))

(ert-deftest dag-test-remove-break ()
  (dag-test-alist-remove-is-valid-p ((a)
                                     (b a)
                                     (c a)
                                     (d c b)
                                     (e c b))
      (e c)
    ((a :children (b) :parents nil)
     (b :children (d) :parents (a))
     (d :children nil :parents (b)))
    ((d c))
    (a b d)))

(ert-deftest dag-test-remove-break-float ()
  (dag-test-alist-remove-is-valid-p ((a)
                                     (b a)
                                     (c b)
                                     (d c))
      (b)
    ((c :children (d) :parents nil)
     (d :children nil :parents (c))
     (a :children nil :parents nil))
    ((c b))
    (c a d)))

(ert-deftest dag-test-insert ()
  (dag-test-alist-insert-is-valid-p ((a)
                                     (b a)
                                     (c a))
      ((d c b)
       (e c b))
    ((a :children (b c) :parents nil)
     (b :children (d e) :parents (a))
     (c :children (d e) :parents (a))
     (d :children nil :parents (b c))
     (e :children nil :parents (b c)))
    nil
    (a c b d e)))

(ert-deftest dag-test-insert-overwrite ()
  (dag-test-alist-insert-is-valid-p ((a)
                                     (b a)
                                     (c a)
                                     (d b))
      ((d b c x))
    ((a :children (b c) :parents nil)
     (b :children (d) :parents (a))
     (c :children (d) :parents (a))
     (d :children nil :parents (b c)))
    ((d x))
    (a c b d)))

(ert-deftest dag-test-insert-floating ()
  (dag-test-alist-insert-is-valid-p ((a)
                                     (b a)
                                     (c a))
      ((d))
    ((a :children (b c) :parents nil)
     (b :children nil :parents (a))
     (c :children nil :parents (a))
     (d :children nil :parents nil))
    nil
    (a d c b)))

(ert-deftest dag-test-insert-broken ()
  (dag-test-alist-insert-is-valid-p ((a)
                                     (b a)
                                     (c a))
      ((d c x))
    ((a :children (b c) :parents nil)
     (b :children nil :parents (a))
     (c :children (d) :parents (a))
     (d :children nil :parents (c)))
    ((d x))
    (a c b d)))

(ert-deftest dag-test-insert-fix-broken ()
  (dag-test-alist-insert-is-valid-p ((a)
                                     (b a)
                                     (d c))
      ((c b))
    ((a :children (b) :parents nil)
     (b :children (c) :parents (a))
     (c :children (d) :parents (b))
     (d :children nil :parents (c)))
    nil
    (a b c d)))

(ert-deftest dag-test-edit ()
  (dag-test-alist-edit-is-valid-p ((a)
                                   (b a)
                                   (c a)
                                   (d b c)
                                   (e b c))
      (b)
      ((c)
       (d c)
       (a c))
    ((a :children nil :parents (c))
     (c :children (a d e) :parents nil)
     (d :children nil :parents (c))
     (e :children nil :parents (c)))
    ((e b))
    (c a e d)))

(ert-deftest dag-test-edit-remove ()
  (dag-test-alist-edit-is-valid-p ((a)
                                   (b a)
                                   (c a)
                                   (d b c)
                                   (e b c))
      (e)
      nil
    ((a :children (b c) :parents nil)
     (b :children (d) :parents (a))
     (c :children (d) :parents (a))
     (d :children nil :parents (b c)))
    nil
    (a c b d)))

(ert-deftest dag-test-edit-insert ()
  (dag-test-alist-edit-is-valid-p ((a)
                                     (b a)
                                     (c a))
      nil
      ((d c b)
       (e c b))
    ((a :children (b c) :parents nil)
     (b :children (d e) :parents (a))
     (c :children (d e) :parents (a))
     (d :children nil :parents (b c))
     (e :children nil :parents (b c)))
    nil
    (a c b d e)))

(ert-deftest dag-test-edit-null ()
  (dag-test-alist-edit-is-valid-p ((a)
                                   (b a)
                                   (c a)
                                   (d b c)
                                   (e b c))
      nil
      nil
    ((a :children (b c) :parents nil)
     (b :children (e d) :parents (a))
     (c :children (e d) :parents (a))
     (d :children nil :parents (b c))
     (e :children nil :parents (b c)))
    nil
    (a c b e d)))

(ert-deftest dag-test-edit-cancel ()
  (dag-test-alist-edit-is-valid-p ((a)
                                   (b a)
                                   (c a)
                                   (d b c)
                                   (e b c))
      (b d)
      ((b a)
       (d b c))
    ((a :children (b c) :parents nil)
     (b :children (e d) :parents (a))
     (c :children (e d) :parents (a))
     (d :children nil :parents (b c))
     (e :children nil :parents (b c)))
    nil
    (a c b e d)))

;; TODO add test for transitive reduction

(provide 'dag-test)
;;; dag-test.el ends here
