;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; hands-tests.lisp
;;;
;;; 2025-10-15
;;;

(print ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hands" *standard-output*)

;;; define test model
(define-model test)

;;;
(print "right-hand left-hand default locations" *standard-output*)
;;;
(with-model test
  (assert (equalp #(4 4) (hand-pos-loc (hand-position (the-hand 'left)))))
  (assert (equalp #(7 4) (hand-pos-loc (hand-position (the-hand 'right))))))

;;; delete test model
(delete-model test)


;;; eof