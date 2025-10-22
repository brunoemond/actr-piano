;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; piano-keyboard.lisp
;;;
;;; 2025-10-15
;;;
;;;

(defclass piano-keyboard (typep-slots) 
  ((xy->keys :type hash-table :initform (make-hash-table :test #'equalp) :reader xy->keys)))




;;; eof