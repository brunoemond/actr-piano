;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; types.lisp
;;;
;;; 2025-10-15
;;;
;;; Type definitions for class slots. 
;;; Currently includes all types for the dependent code. 

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
;;; member-of
;;;
(deftype member-of (set)
  (let ((predicate (gensym "PREDICATE")))
    (setf (symbol-function predicate)
          (lambda (element)
            (when (member element 
                          (if (symbolp set) (eval set) set))
              t)))
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
;;; xy-coordinate
;;;
(defun isa-xy-coordinate (object)
  (and (or (typep object '(set-of number))
           (typep object 'simple-vector))
       (eq 2 (length object))))

(deftype xy-coordinate ()
  '(satisfies isa-xy-coordinate))

;;;
;;; device-list
;;;
(defun isa-device-list (list)
  (and (member (first list) (defined-interfaces) :test #'string=)
       (member (second list) (defined-devices) :test #'string=)
       (or (null (third list))
           (stringp (third list)))))

(deftype device-list ()
  '(satisfies isa-device-list))

;;;
;;; finger-offset-spec
;;;
(defun isa-finger-offset (object finger-names)
  (and (listp object)
       (eq (length object) 2)
       (eq (length (second object)) 2)
       (member (first object) finger-names)
       (numberp (elt (second object) 0))
       (numberp (elt (second object) 1))))

(deftype finger-offset (&optional (finger-names '(thumb index middle ring pinkie)))
  (let ((predicate (gensym "PREDICATE")))
    (setf (symbol-function predicate)
          (lambda (object) (isa-finger-offset object finger-names)))
    `(satisfies ,predicate)))

;;; eof