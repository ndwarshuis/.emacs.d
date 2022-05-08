;;; interval.el --- a bedtools knockoff -*- lexical-binding: t; -*-

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

;; Functions pertaining to intervals.

;; An interval is defined as a list like (START END) where START and END are
;; two numbers, and END >= START.

;; Inspired by bedtools (https://github.com/arq5x/bedtools2).

;;; Code:

(require 'dash)

(defun interval< (a b)
  "Test if A comes before B.

Return t if A starts before B, and return t if A ends before B
and they have the same start time."
  (let ((a0 (car a))
        (b0 (car b)))
    (if (= a0 b0) (< (cadr a) (cadr b)) (< a0 b0))))

(defun interval<= (a b)
  "Test if A comes before B or is the same as B."
  (not (interval< b a)))

(defun interval-bimap (fun int)
  "Apply FUN to both numbers in INT."
  `(,(funcall fun (car int)) ,(funcall fun (cadr int))))

(defun interval-len (int)
  "Return the length of INT."
  (- (cadr int) (car int)))

(defun interval-min (ints)
  "Return the earliest starting value in INTS."
  (-min (-map #'car ints)))

(defun interval-max (ints)
  "Return the latest ending value in INTS."
  (-max (-map #'cadr ints)))

(defun interval-span (ints)
  "Return the length covered by all INTS."
  (if (not ints) 0
    (-let (((mn mx) (car ints)))
      ;; I could just get the min/max but this avoids looping twice
      (--each (cdr ints)
        (when (< (car it) mn)
          (setq mn (car it)))
        (when (< mx (cadr it)
          (setq mx (cadr it)))))
      (- mx mn))))

(defun interval-group-overlaps (interval-fun xs)
  "Group XS based on when their intervals overlap.

INTERVAL-FUN is a function that takes one of XS and returns an
interval like (START END) where START and END are numbers.

Return a list of all pairs in XS for which their intervals overlap.

Complexity is O(N^2) in case all members in XS conflict with each other, and
O(N) in case there are no conflicts."
  (cl-labels
      ((get-overlaps
        (acc ss)
        (-if-let (s0 (car ss))
            (-let* (((acc+ acc-) acc)
                    (A (cdr s0))
                    (a1 (cadr (car s0)))
                    (rest (cdr ss)))
              ;; add members while if the starting value is less than the ending
              ;; value of the current member
              (-if-let (over (->> (--take-while (< (car (car it)) a1) rest)
                                  (--map (list A (cdr it)))
                                  (reverse)))
                  (get-overlaps `((,@over ,@acc+) ,acc-) rest)
                (get-overlaps `(,acc+ (,A ,@acc-)) rest)))
          acc)))
    (-let (((over non-over) (->> (-annotate interval-fun xs)
                                 (--sort (interval< (car it) (car other)))
                                 (get-overlaps nil))))
      (list (nreverse over) (nreverse non-over)))))

(defun interval-sort (ints)
  "Sort INTS according to `interval-rank'."
  (-sort #'interval< ints))

(defun interval-merge (ints)
  "Merge a list of overlapping intervals.

Two intervals overlap if the start/end of one is within the other
interval (inclusive).

Assume INTS is sorted according to `interval-sort'.

Complexity is O(N) where N is the length of INTS."
  (cl-flet
      ((merge-intervals
        (acc interval)
        (-let ((((sp ep) . accp) acc)
               ((s e) interval))
          (if (<= s ep) `((,sp ,e) ,@accp) `((,s ,e) ,@acc)))))
    (when ints
      (->> (cdr ints)
           (-reduce-from #'merge-intervals `(,(car ints)))
           (reverse)))))

(defun interval-complement (start end ints)
  "Return the complement of intervals in INTS.

START and END are the lower and upper bound to determine where
the first/last compliment should start/end in case the first/last
in INTS starts/ends after/before START/END respectively.

Assume that INTS is sorted according to `interval-sort', that no
members of INT overlap, and that no members in INT have an end
before START or a start after END.

Complexity is O(N)."
  (cl-flet
      ((complement
        (acc interval)
        (-let (((last gaps) acc)
               ((s e) interval))
          `(,e ((,last ,s) ,@gaps)))))
    (if (not ints)
        `(,start ,end)
      (-let* (((s e) (car ints))
              ((init ints*) (if (<= s start) `(,e ,(cdr ints)) `(,start ,ints)))
              ((last gaps) (-reduce-from #'complement `(,init) ints*)))
        (->> (if (<= end last) gaps `((,last ,end) ,@gaps))
             (reverse))))))

(defun interval-sum (ints)
  "Return the sum of INTS."
  (-sum (-map #'interval-len ints)))

(provide 'interval)
;;; interval.el ends here
