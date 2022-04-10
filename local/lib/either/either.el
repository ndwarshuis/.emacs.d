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

(defmacro either (key data)
  "Make an either type.

KEY is either :left or :right and DATA is whatever goes in the
left/right slot."
  (pcase key
    ((or :left :right) `(list ,key ,data))
    (_ (error "Invalid status key: %s" key))))

(defmacro either>>= (either form)
  "Bind EITHER to FORM where the right slot is bound to 'it'."
  (declare (indent 1))
  `(pcase ,either
     (`(:left ,_) ,either)
     (`(:right ,it) ,form)
     (e (error "Learn to use monads, dummy; this isn't one: %s" e))))

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

(defmacro either-from (either left-form right-form)
  "Apply forms to the left or right slot of EITHER.

Use LEFT-FORM or RIGHT-FORM is EITHER is left or right
respectively where 'it' is bound to whatever is in the the
left/right slots."
  (declare (indent 1))
  `(pcase ,either
     (`(:left ,it) ,left-form)
     (`(:right ,it) ,right-form)
     (e (error "Not an either: %s" e))))

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

(provide 'either)
;;; either.el ends here
