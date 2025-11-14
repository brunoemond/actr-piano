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

;;;
(print "moving hand with move-hand request style" *standard-output*)
;;;

(clear-all)
(define-model test
  (chunk-type goal task)
  ;(chunk-type hand-movement (hand empty) (to-xy empty)
  ;            (thumb empty) (index  empty) (middle  empty) (ring  empty) (pinkie  empty))

  (define-chunks move-hand end)
  (define-chunks (goal isa goal task move-hand))
  (goal-focus goal)

  ;(place-hands-on-keyboard '(25 0) 1)
  (place-hand-on-keyboard 'left 'thumb '(25 0) 1)
  (place-hand-on-keyboard 'right 'thumb '(25 0) 1)
  ;; also possible, and equivalent
  ;; (place-hand-on-keyboard 'left 'thumb '(25 0) 1)
  ;; (place-hand-on-keyboard 'right 'thumb '(25 0) 1)
  ;; or hands can be placed differently
  ;; (place-hand-on-keyboard 'left 'pinkie #(17 0) 1)
  ;; (place-hand-on-keyboard 'right 'thumb '(25 0) 1)
  
  (p test-move-hand
     =goal>
     isa goal
     task move-hand 

     ?manual>
     state free

     ==> 

     =goal>
     task end

     +manual> 
     isa hand-movement
     hand right 
     to-xy (29 0)
     )

  (p end
     =goal>
     isa goal
     task end 

     ?manual>
     state free

     ==>

     -goal>))

(setf *break-on-signals* t)

(with-model test
  (let ((rh-offsets `((THUMB ,#(0 0)) (INDEX ,#(1 0)) (MIDDLE ,#(2 0)) (RING ,#(3 0)) (PINKIE ,#(4 0))))
        (lh-offsets `((THUMB ,#(0 0)) (INDEX ,#(1 0)) (MIDDLE ,#(2 0)) (RING ,#(3 0)) (PINKIE ,#(4 0)))))
    (assert (equalp #(25 0) (hand-xy 'right :current t)))
    (assert (equalp rh-offsets (finger-offsets 'right :current t)))
    (assert (equalp #(25 0) (hand-xy 'left :current t)))
    (assert (equalp lh-offsets (finger-offsets 'right :current t)))
    (format *standard-output* "Model execution trace:~%")
    (run 1)
    (assert (equalp #(29 0) (hand-xy 'right :current t)))
    (assert (equalp rh-offsets (finger-offsets 'right :current t)))
    (assert (equalp #(25 0) (hand-xy 'left :current t)))
    ))

;;;
(print "moving hand and finger with move-hand request style" *standard-output*)
;;;

(clear-all)
(define-model test
  (chunk-type goal task)
  ;(chunk-type hand-movement (hand empty) (to-xy empty)
  ;            (thumb empty) (index  empty) (middle  empty) (ring  empty) (pinkie  empty))

  (define-chunks move-hand end)
  (define-chunks (goal isa goal task move-hand))
  (goal-focus goal)

  ;(place-hands-on-keyboard '(25 0) 1)
  (place-hand-on-keyboard 'left 'thumb '(25 0) 1)
  (place-hand-on-keyboard 'right 'thumb '(25 0) 1)
  ;; also possible, and equivalent
  ;; (place-hand-on-keyboard 'left 'thumb '(25 0) 1)
  ;; (place-hand-on-keyboard 'right 'thumb '(25 0) 1)
  ;; or hands can be placed differently
  ;; (place-hand-on-keyboard 'left 'pinkie #(17 0) 1)
  ;; (place-hand-on-keyboard 'right 'thumb '(25 0) 1)
  
  (p test-move-hand
     =goal>
     isa goal
     task move-hand 

     ?manual>
     state free

     ==> 

     =goal>
     task end

     +manual> 
     isa hand-movement
     hand right 
     to-xy (29 0)
     pinkie (5 0)
     )

  (p end
     =goal>
     isa goal
     task end 

     ?manual>
     state free

     ==>

     -goal>))

(setf *break-on-signals* t)

(with-model test
  (let ((rh-offsets `((THUMB ,#(0 0)) (INDEX ,#(1 0)) (MIDDLE ,#(2 0)) (RING ,#(3 0)) (PINKIE ,#(4 0))))
        (lh-offsets `((THUMB ,#(0 0)) (INDEX ,#(1 0)) (MIDDLE ,#(2 0)) (RING ,#(3 0)) (PINKIE ,#(4 0)))))
    (assert (equalp #(25 0) (hand-xy 'right :current t)))
    (assert (equalp rh-offsets (finger-offsets 'right :current t)))
    (assert (equalp #(25 0) (hand-xy 'left :current t)))
    (assert (equalp lh-offsets (finger-offsets 'right :current t)))
    (format *standard-output* "Model execution trace:~%")
    (run 1)
    (assert (equalp #(29 0) (hand-xy 'right :current t)))
    (assert (not (equalp rh-offsets (finger-offsets 'right :current t))))
    (assert (equalp #(5 0) (finger-offset 'right 'pinkie :current t)))
    (assert (equalp #(25 0) (hand-xy 'left :current t)))
    ))


;;; eof