;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; hands-tests.lisp
;;;
;;; 2025-10-15
;;;

(print ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hands" *standard-output*)

;;; define test model

;;;
(print "hand-pos" *standard-output*)
;;;
(clear-all)
(define-model test)
(with-model test
  (assert (hand-pos 'right :current t))
  (assert (hand-pos 'left :current t))
  (assert (null (hand-pos 'right)))
  (assert (null (hand-pos 'left)))
  (initialize-next-hand-pos 'right)
  (initialize-next-hand-pos 'left)
  (assert (hand-pos 'right))
  (assert (hand-pos 'left))
  (clear-next-hand-pos 'right)
  (clear-next-hand-pos 'left)
  (assert (null (hand-pos 'right)))
  (assert (null (hand-pos 'left))))


;;;
(print "hand-xy" *standard-output*)
;;;
;;; test model is new and hands are set on the computer keyboard
(clear-all)
(define-model test)
(with-model test
  (assert (equalp #(4 4) (hand-xy 'left :current t)))
  (setf (hand-xy 'left :current t) #(5 5))
  (assert (equalp #(5 5) (hand-xy 'left :current t))))

;;;
(print "finger-offsets" *standard-output*)
;;;
(clear-all)
(define-model test)
(with-model test
  (let ((new-offsets `((THUMB ,#(0 0)) (INDEX ,#(1 0)) (MIDDLE ,#(2 0)) (RING ,#(3 0)) (PINKIE ,#(4 0)))))
    (initialize-next-hand-pos 'right)
    (assert (hand-pos 'right))
    (assert (equalp (finger-offsets 'right) 
                    `((INDEX ,#(0 0)) (MIDDLE ,#(1 0)) (RING ,#(2 0)) (PINKIE ,#(3 0)) (THUMB ,#(-1 2)))))
    (setf (finger-offsets 'right) new-offsets)
    (assert (equalp (finger-offsets 'right) new-offsets))))

;;;
(print "finger-offset" *standard-output*)
;;;
(clear-all)
(define-model test)
(with-model test
  (initialize-next-hand-pos 'right)
  (assert (hand-pos 'right))
  (assert (equalp #(3 0) (finger-offset 'right 'pinkie)))
  (setf (finger-offset 'right 'pinkie) #(5 0))
  (assert (equalp #(5 0) (finger-offset 'right 'pinkie))))

;;;
(print "finger-xy" *standard-output*)
;;;
;;; test model is new and hands are set on the computer keyboard
(clear-all)
(define-model test)
(with-model test
  (assert (equalp #(7 4) (hand-xy 'right :current t)))
  (assert (equalp #(3 0) (finger-offset 'right 'pinkie :current t)))
  (assert (equalp #(10 4) (finger-xy 'right 'pinkie :current t)))
  (setf (finger-xy 'right 'pinkie :current t) #(12 5))
  (assert (equalp #(5 1) (finger-offset 'right 'pinkie :current t))))

;;;
(print "change-anchor-finger" *standard-output*)
;;;
(let ((offsets `((THUMB ,#(0 0)) (INDEX ,#(1 0)) (MIDDLE ,#(2 0)) (RING ,#(3 0)) (PINKIE ,#(4 0)))))
  (assert (equalp (offsets->xys #(5 5) offsets)
                  `((THUMB ,#(5 5)) (INDEX ,#(6 5)) (MIDDLE ,#(7 5)) (RING ,#(8 5)) (PINKIE ,#(9 5)))))
  (assert (equalp (change-anchor-finger #(5 5) offsets 'thumb)
                  offsets))
  (assert (equalp (change-anchor-finger #(5 5) offsets 'middle)
                  `((THUMB ,#(-2 0)) (INDEX ,#(-1 0)) (MIDDLE ,#(0 0)) (RING ,#(1 0)) (PINKIE ,#(2 0)))))
  )

;;;
(print "place-hand-on-keyboard & place-hands-on-keyboard" *standard-output*)
;;;
(clear-all)
(define-model test)
(with-model test
  (let ((c3-xy '(17 0))
        (c4-xy '(25 0))
        (finger-spacing 1))
    (assert (equalp (place-hand-on-keyboard 'left 'pinkie c3-xy finger-spacing)
                    #S(HAND-POS :LOC #(17 0) :FINGERS ((THUMB #(4 0)) (INDEX #(3 0)) (MIDDLE #(2 0)) (RING #(1 0)) (PINKIE #(0 0))) :OTHER NIL :DEVICE NIL)))
    (assert (equalp (place-hand-on-keyboard 'right 'thumb c4-xy finger-spacing)
                    #S(HAND-POS :LOC #(25 0) :FINGERS ((THUMB #(0 0)) (INDEX #(1 0)) (MIDDLE #(2 0)) (RING #(3 0)) (PINKIE #(4 0))) :OTHER NIL :DEVICE NIL)))
    ))


;;; eof