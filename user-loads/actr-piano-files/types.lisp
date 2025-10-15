;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; types.lisp
;;;
;;; 2025-10-15
;;;

;;;
;;; set-of
;;;
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

;;;
;;; null-or-type
;;;
(defun isa-null-or-type (object type)
  (or (null object)
      (typep object type)))

(deftype null-or-type (&optional (type t))
  (let ((predicate (gensym "PREDICATE")))
    (setf (symbol-function predicate)
          (lambda (object) (isa-null-or-type object type)))
    `(satisfies ,predicate)))

;;;
;;; empty-or-symbol
;;;
(defconstant +empty+ 'empty)

(defun isa-empty-or-symbol (object)
  (or (eq object +empty+) 
      (and (symbolp object)
           (not (null object)))))

(deftype empty-or-symbol ()
  '(satisfies isa-empty-or-symbol))

;;;
;;; coordinate
;;;
(defun isa-coordinate (object)
  (and (typep object '(set-of number))
       (eq 2 (length object))))

(deftype coordinate ()
  '(satisfies isa-coordinate))

;;; eof