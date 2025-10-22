;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; utilities.lisp
;;;
;;; 2025-10-15
;;;
;;;

(defmethod ->symbol ((object string)) (intern (string-upcase object)))
(defmethod ->symbol ((object symbol)) object)

(defmethod ->string ((object string)) object)
(defmethod ->string ((object symbol)) (write-to-string object :case :downcase))

(defmethod ->vector ((object simple-vector)) object)
(defmethod ->vector ((object list)) (apply #'vector object))

(defmethod ->list ((object simple-vector)) (coerce object 'list))
(defmethod ->list ((object list)) object)

(defun remove-from-plist (plist key)
  (loop for (k v) on plist by #'cddr
        unless (eq k key)
        append (list k v)))

(defun rad (v1 v2)
  (if (and (eq 2 (length v1))
           (eq 2 (length v2)))
      (atan (- (aref v2 1) (aref v1 1))
            (- (aref v2 0) (aref v1 0)))
    0.0))

(defun r-theta (arg1 arg2)
  (let ((v1 (->vector arg1))
        (v2 (->vector arg2)))
    (values (dist v1 v2)
            (rad v1 v2))))

;;; eof