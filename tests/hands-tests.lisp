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


;;;
(print "set-default-finger-offsets" *standard-output*)
;;;
(with-model test
  (multiple-value-bind (right left)
      (set-default-finger-offsets '(5 5))
    (assert (equalp #(5 5) (hand-pos-loc right)))
    (assert (equalp #(5 5) (hand-pos-loc left)))))

;;;
(print "move-piano-hand style - prepare-features" *standard-output*)
;;;
(with-model test
  (set-default-finger-offsets '(5 5))
  (let ((action (make-instance 'move-piano-hand :hand 'right :to-xy '(7 5))))
    (prepare-features (motor-module) action)
    (assert (equalp #(5 5) (->vector (hand-xy 'right))))
    (assert (equalp #(7 5) (hand-pos-loc (updated-pos action))))))


;;;
(print "move-piano-hand style - feat-differences" *standard-output*)
;;;
(with-model test
  (let ((action1 (make-instance 'move-piano-hand :hand 'right :to-xy '(7 5)))
        (action2 (make-instance 'move-piano-hand :hand 'right :to-xy '(8 5))))
    (assert (equalp 0 (feat-differences action1 action1)))
    (assert (equalp 1 (feat-differences action1 action2)))))

;;;
(print "move-piano-hand style - manual request" *standard-output*)
;;;

(define-model test2
  (set-default-finger-offsets '(5 5))
  (p test-move
     ?manual>
     state free
     ==> 
     +manual> 
     isa move-piano-hand 
     hand right 
     thumb-xy (7 5)
     to-xy (8 5)))

(setf *break-on-signals* t)

(with-model test2 (run 1))


;;; delete test model
(progn 
  (delete-model test)
  ;(delete-model test2)
  )





;;; eof