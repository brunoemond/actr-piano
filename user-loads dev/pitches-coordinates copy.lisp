;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; pitches-coordinates.lisp
;;;
;;; 2025-11-06
;;;
;;; Using interval numbers to determine interval coordinate form a source location. 
;;; The algorithm does not rule out coordinates where there would be no accidental key. 
;;; For example 3+ from a C falls between E and F. 

(defconstant +natural-intervals+ '(1 2 3 4 5 6 7))
(defconstant +accidental-intervals+ '(1+ 2+ 3+ 4+ 5+ 6+ 7+))

;; top left is (0 0)
(defparameter *n-x* 10
  "Distance in mm between two natural key centres.")
(defparameter *n-y* 10
  "Distance in mm from the top to the centre of a natural key.")
(defparameter *a-x* 5
  "Distance in mm from the centre of a natural key to the centre of a accidental key.")
(defparameter *a-y* 5
  "Distance in mm from the top to the centre of an accidental key.")


(defun xy-if-natural (interval)
  (vector (* interval *n-x*) *n-y*))

(defun xy-if-accidental (interval+)
  (let ((interval (1+ (position interval+ +accidental-intervals+))))
    (vector (+ (* interval *n-x*) *a-x*) *a-y*)))

(defun xy-from-interval (xy interval)
  (let ((xy-interval
         (if (member interval +natural-intervals+)
             (xy-if-natural interval)
           (xy-if-accidental interval))))
    (vector (+ (elt xy 0) (elt xy-interval 0))
            (+ (elt xy 1) (elt xy-interval 1)))))

(defun distance (v1 v2)
  (sqrt (+ (expt (- (elt v2 0) (elt v1 0)) 2)
           (expt (- (elt v2 1) (elt v1 1)) 2))))

(assert (equalp #(10 10) (xy-from-interval #(0 0) 1)))
(assert (equalp #(15 5) (xy-from-interval #(0 0) '1+)))

(assert (equal 0.0 (distance (xy-from-interval #(0 0) 1) 
                             (xy-from-interval #(0 0) 1))))

(assert (equal 7.071068 (distance (xy-from-interval #(0 0) 1) 
                                  (xy-from-interval #(0 0) '1+))))

(assert (equal 10.0 (distance (xy-from-interval #(0 0) 1) 
                                  (xy-from-interval #(0 0) 2))))


;;; eof