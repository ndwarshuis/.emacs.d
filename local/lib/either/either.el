;;; either.el --- This or that -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nathan Dwarshuis

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

;; Lisp implementation (kinda) of Data.Either from Haskell

;;; Code:

(require 'dash)

;; type constructor

(defmacro either (key data)
  "Make an either type.

KEY is either :left or :right and DATA is whatever goes in the
left/right slot."
  (pcase key
    ((or :left :right) `(list ,key ,data))
    (_ (error "Invalid status key: %s" key))))

;; monad-y things

(defmacro either>>= (either form)
  "Bind EITHER to FORM where the right slot is bound to 'it'."
  (declare (indent 1))
  (let ((e (make-symbol "--either")))
    `(let ((,e ,either))
       (pcase ,e
         (`(:left ,_) ,e)
         (`(:right ,it) ,form)
         (e (error "Learn to use monads, dummy; this isn't one: %s" e))))))

(defun either-foldM (fun init xs)
  "Mondically apply FUN to XS (a list).

INIT is the starting value to use for FUN, which takes two
arguments (the accumulator and the next value) and returns an
either."
  (let ((acc (either :right init)))
    (while (and xs (either-is-right-p acc))
      (setq acc (funcall fun (cadr acc) (car xs)))
      (!cdr xs))
    acc))

(defmacro either-foldM* (form init xs)
  `(either-foldM (lambda (acc it) ,form) ,init ,xs))

;; Data.Either ripoff things

(defun either-is-left-p (either)
  "Return t if EITHER is a left."
  (eq (car either) :left))

(defun either-is-right-p (either)
  "Return t if EITHER is a right."
  (eq (car either) :right))

(defmacro either<$> (either form)
  "Map FORM over EITHER where 'it' is bound to the right slot."
  (declare (indent 1))
  (let ((b (make-symbol "--bs")))
    `(let ((,b ,either))
       (pcase ,b
         (`(:left ,_) ,b)
         (`(:right ,it) (either :right ,form))
         (e (error "Learn to use functors, dummy; this isn't one: %s" e))))))

(defun either-from-right (either default)
  "Return contents of EITHER if right or DEFAULT."
  (pcase either
    (`(:left ,_) default)
    (`(:right ,x) x)
    (e (error "Not an either: %s" e))))

(defun either-from-left (either default)
  "Return contents of EITHER if left or DEFAULT."
  (pcase either
    (`(:left ,x) x)
    (`(:right ,_) default)
    (e (error "Not an either: %s" e))))

;; (defun either-from-right* (either default fun)
;;   (declare (indent 2))
;;   (either-from-right either default (funcall fun it)))

;; (defun either-from-left* (either default fun)
;;   (declare (indent 2))
;;   (either-from-left either default (funcall fun it)))

(defmacro either-from* (either left-form right-form)
  "Apply forms to the left or right slot of EITHER.

Use LEFT-FORM or RIGHT-FORM if EITHER is left or right
respectively where 'it' is bound to whatever is in the the
left/right slots."
  (declare (indent 1))
  `(pcase ,either
     (`(:left ,it) ,left-form)
     (`(:right ,it) ,right-form)
     (e (error "Not an either: %s" e))))

(defun either-from (either left-fun right-fun)
  "Apply functions to the left or right slot of EITHER.

Use LEFT-FUN or RIGHT-FUN if EITHER is left or right
respectively where 'it' is bound to whatever is in the the
left/right slots."
  (declare (indent 1))
  (either-from* either
    (funcall left-fun it)
    (funcall right-fun it)))

(defun either-lefts (eithers)
  "Return all left values from EITHERS."
  (let (acc)
    (--each eithers
      (when (either-is-left-p it)
        (!cons (cadr it) acc)))
    (nreverse acc)))

(defun either-rights (eithers)
  "Return all right values from EITHERS."
  (let (acc)
    (--each eithers
      (when (either-is-right-p it)
        (!cons (cadr it) acc)))
    (nreverse acc)))

(defun either-partition (eithers)
  "Return separate EITHERS into list like (LEFTS RIGHTS)."
  (let (acc-left acc-right)
    (--each eithers
      (if (either-is-right-p it)
          (!cons (cadr it) acc-right)
        (!cons (cadr it) acc-left)))
    `(,(nreverse acc-left) ,(nreverse acc-right))))

(defun either-maybe (fun either)
  "Return nil if EITHER is left and apply FUN otherwise."
  (either-from either (-const nil) fun))

(provide 'either)
;;; either.el ends here
