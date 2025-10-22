;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; types-tests.lisp
;;;
;;; 2025-10-15
;;;

(print ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; types" *standard-output*)

;;;
(print "set-of" *standard-output*)
;;;
(assert (typep '(1 2 3) '(set-of number)))
(assert (not (typep '(1 x 3) '(set-of number))))

;;;
(print "member-of" *standard-output*)
;;;
(assert (typep 1 '(member-of (1 2 3))))
(assert (not (typep 4 '(member-of (1 2 3)))))

;;;
(print "null-or-type" *standard-output*)
;;;
(assert (typep 1 '(null-or-type number)))
(assert (typep nil '(null-or-type number)))
(assert (not (typep t '(null-or-type number))))

;;;
(print "empty-or-symbol" *standard-output*)
;;;
(assert (typep +empty+ 'empty-or-symbol))
(assert (typep 'x 'empty-or-symbol))
(assert (not (typep nil 'empty-or-symbol)))

;;;
(print "xy-coordinate" *standard-output*)
;;;
(assert (typep '(1 2) 'xy-coordinate))
(assert (not (typep '(1 2 3) 'xy-coordinate)))
(assert (not (typep '(1 x) 'xy-coordinate)))

;;;
(print "device-list" *standard-output*)
;;;
(assert (typep '("vision" "exp-window") 'device-list))
(assert (typep '("motor" "cursor" "mouse") 'device-list))
(assert (not (typep '("abc" "cursor") 'device-list)))

;;;
(print "finger-offset" *standard-output*)
;;;
(assert (typep '(index 1 2) 'finger-offset))
(assert (not (typep '(something 1 2) 'finger-offset)))

(assert (typep '((thumb 1 2) (index 3 2)) '(set-of finger-offset)))


;;; eof