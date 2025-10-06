;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; visicon-object-tests.lisp
;;;
;;; 2025-10-02
;;;
#|
A visicon-object is a surface that can be perceived by actr as a set of visicon-entries.
|#

;;; surface
(let ((top-surface (make-instance 'surface :xy '(1 1)))
       (sub-surface (make-instance 'surface :xy '(2 3)))
       (subsub-surface (make-instance 'surface :xy '(2 2))))
  ;; adding sub surfaces only if coordinates are not equal
  (setf (surface-collection top-surface) sub-surface)
  (assert (eq 1 (length (surface-collection top-surface))))
  (setf (surface-collection top-surface) sub-surface)
  (assert (eq 1 (length (surface-collection top-surface))))
  ;; adjusting xy surface colection items
  (assert (equalp '(1 1) (adjust-xy top-surface)))
  (assert (equalp '(3 4) (adjust-xy sub-surface)))
  (setf (surface-collection sub-surface) (list subsub-surface))
  (assert (equalp '(5 6) (adjust-xy subsub-surface))) 
  )


;; visicon-object
(defclass piano-kbd (visicon-object) ())
(defclass piano-key (visicon-object) 
  ((black-group :type symbol :initform nil :initarg :black-group :reader black-group)
   (finger :type symbol :initform *empty-value* :initarg :finger :reader finger)
   (fstatus :type symbol :initform *empty-value* :initarg :fstatus :reader fstatus))
  (:default-initargs
   :visual-features '(black-group finger fstatus)))

(defparameter *piano-kbd* 
  (let ((kbd (make-instance 'piano-kbd :xy '(0 0)))
        (c (make-instance 'piano-key :xy '(0 0) :color 'white :black-group 'b2 :finger 'r1))
        (d (make-instance 'piano-key :xy '(1 0) :color 'white :black-group 'b2 :finger 'r2))
        (e (make-instance 'piano-key :xy '(2 0) :color 'white :black-group 'b2 :finger 'r3))
        (f (make-instance 'piano-key :xy '(3 0) :color 'white :black-group 'b3 :finger 'r4))
        (g (make-instance 'piano-key :xy '(4 0) :color 'white :black-group 'b3 :finger 'r5))
        (a (make-instance 'piano-key :xy '(5 0) :color 'white :black-group 'b3))
        (b (make-instance 'piano-key :xy '(6 0) :color 'white :black-group 'b3)))
    (setf (surface-collection kbd) (list c d e f g a b))
    kbd))

(progn 
  (clear-all)
  (define-model test)

  (setf (device-instance "piano-kbd") *piano-kbd*)
  
  (with-model test  
    (install-device '("vision" "piano-kbd"))

    (add-to-visicon 
     (device-instance "piano-kbd"))

    (run-n-events 5)
    (print-visicon))

  (delete-model test)
  (setf (device-instance "piano-kbd") nil)
  )


(progn 
  (clear-all)

  (define-model test
    (p find-third-from-r1
       =visual-search>
       finger r3
       screen-x =x
       ==>
       !output! (third is on screen-x =x)
       !stop!
       )
    )

  (setf (device-instance "piano-kbd") *piano-kbd*)
  
  (with-model test  
    (install-device '("vision" "piano-kbd"))

    (add-to-visicon 
     (device-instance "piano-kbd"))

    (run-n-events 5)
    (print-visicon)
    (run 3))

  

  (delete-model test)
  (setf (device-instance "piano-kbd") nil)
  )



(unintern 'piano-kbd)
(unintern 'piano-key)
(unintern '*piano-kbd*)



;;; eof