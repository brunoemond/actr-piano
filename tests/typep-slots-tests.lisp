;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; typep-slots-tests.lisp
;;;
;;; 2025-08-15
;;;

(print ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; typep-slots" *standard-output*)

(defclass test-type1 (typep-slots) ((a) (b :initform 'x :type symbol)))

(let ((instance (make-instance 'test-type1)))
  (with-typep-slots (a b) instance 
    (assert (setf a 23))
    (assert (setf a 'x))
    (assert (setf b 'y)))

  #+lispworks
  (progn 
    (assert (eq t (slot-type instance 'a)))
    (assert (eq 'symbol (slot-type instance 'b)))
    (assert (null (ignore-errors (setf (typep-slot-value instance 'b) 23)))))
  )

(defclass test-type2 (typep-slots) ((a :initarg :a) (b :initarg :b :type symbol) 
                                    (c :initarg :c :initform 'x :type symbol)))
#+lispwork
(assert (null (ignore-errors (make-instance 'test-type2 :c 23))))

(unintern 'test-type1)
(unintern 'test-type2)

;;; eof