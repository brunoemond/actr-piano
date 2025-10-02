;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; types.lisp
;;;
;;; 2025-10-02
;;;

;;; types
(defun isa-coordinate (object)
  (and (listp object)
       (eq 2 (length object))
       (every (lambda (x) (typep x 'number))
              object)))

(deftype coordinate ()
  '(satisfies isa-coordinate))

(defun isa-surface-collection (object)
  (and (listp object)
       (every (lambda (x) (typep x 'surface))
              object)))

(deftype surface-collection ()
  '(satisfies isa-surface-collection))

(defun isa-container (object)
  (or (null object)
      (typep object 'surface)))

(deftype container ()
  '(satisfies isa-container))

(defun isa-number-or-nil (object)
  (or (null object) 
      (typep object 'number)))

(deftype number-or-nil ()
  '(satisfies isa-number-or-nil))

(defun isa-not-nil-symbol (object)
  (and (symbolp object)
       (not (null object))))

(deftype not-nil-symbol ()
  '(satisfies isa-not-nil-symbol))

(defun isa-symbols-set (object)
  (every (lambda (x) (typep x 'not-nil-symbol))
         object))

(deftype symbols-set ()
  '(satisfies isa-symbols-set))

(defconstant *empty-value* 'empty)

(defun isa-chunk-name-or-empty (object)
  (or (eq object *empty-value*)
      (typep object 'not-nil-symbol)))

(deftype chunk-name-or-empty ()
  '(satisfies isa-chunk-name-or-empty))



;;; eof