;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; types-tests.lisp
;;;
;;; 2025-10-15
;;;

;;; set-of
(assert (typep '(1 2 3) '(set-of number)))
(assert (not (typep '(1 x 3) '(set-of number))))

;;; null-or-type
(assert (typep 1 '(null-or-type number)))
(assert (typep nil '(null-or-type number)))
(assert (not (typep t '(null-or-type number))))

;;; empty-or-symbol
(assert (typep +empty+ 'empty-or-symbol))
(assert (typep 'x 'empty-or-symbol))
(assert (not (typep nil 'empty-or-symbol)))


;;; coordinate
(assert (typep '(1 2) 'coordinate))
(assert (not (typep '(1 2 3) 'coordinate)))
(assert (not (typep '(1 x) 'coordinate)))


;;; eof