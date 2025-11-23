;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; hands-styles-tests.lisp
;;;
;;; 2025-11-19
;;;

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