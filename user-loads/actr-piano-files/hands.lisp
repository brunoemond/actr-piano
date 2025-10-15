;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; hands.lisp
;;;
;;; 2025-10-15
;;;

;;;
;;; parameters
;;;
(defconstant +hand-names+ '(left right))
(defconstant +finger-names+ '(thumb index middle ring pinkie))

;;; motor module
(defun motor-module ()
  (get-module :motor))

(defun hand-tracker ()
  (extension (motor-module)))

(defun the-hand (hand-name)
  (ecase hand-name
    (left (left-hand (motor-module)))
    (right (right-hand (motor-module)))))



;;; eof