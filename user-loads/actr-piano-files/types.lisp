;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; types.lisp
;;;
;;; 2025-10-14
;;;


;;; constants
(defconstant +empty+ 'empty)


;;; types

(defun isa-set-of (elements element-type)
  (and (listp elements)
       (every (lambda (element)
                (typep element element-type))
              elements)))

(deftype set-of (&optional (element-type t))
  (let ((predicate (gensym "PREDICATE")))
    (setf (symbol-function predicate)
          (lambda (elements) (isa-set-of elements element-type)))
    `(satisfies ,predicate)))

(defun isa-null-or-type (object type)
  (or (null object)
      (typep object type)))

(deftype null-or-type (&optional (type t))
  (let ((predicate (gensym "PREDICATE")))
    (setf (symbol-function predicate)
          (lambda (object) (isa-null-or-type object type)))
    `(satisfies ,predicate)))

; (typep '(1 2 3) '(set-of number))
; (typep 1 '(null-or-type number))

(defun isa-coordinate (object)
  (and (typep object '(set-of number))
       (eq 2 (length object))))

(deftype coordinate ()
  '(satisfies isa-coordinate))

(defun isa-non-nil-symbol (object)
  (and (symbolp object) 
       (not (null object))))

(deftype non-nil-symbol ()
  '(satisfies isa-non-nil-symbol))

(defun isa-empty-or-symbol (object)
  (or (eq object +empty+) 
      (isa-non-nil-symbol object)))

(deftype empty-or-symbol ()
  '(satisfies isa-empty-or-symbol))




;;; eof