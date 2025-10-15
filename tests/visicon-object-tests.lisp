;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; visicon-object-tests.lisp
;;;
;;; 2025-10-02
;;;

;;;
(print "surface" *standard-output*)
;;;
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

;;;
(print "class definitions" *standard-output*)
;;;
(defclass piano-kbd (visicon-object) 
  ((nb-keys :type number :initform 7 :initarg :nb-keys :reader nb-keys))
  (:default-initargs
   :visual-features '(nb-keys)))
(defclass piano-key (visicon-object) 
  ((black-group :type symbol :initform nil :initarg :black-group :reader black-group)
   (finger :type symbol :initform +empty+ :initarg :finger :accessor finger)
   (fstatus :type symbol :initform +empty+ :initarg :fstatus :accessor fstatus))
  (:default-initargs
   :visual-features '(black-group finger fstatus)))

;;;
(print "types and visual-features" *standard-output*)
;;;
(let ((kbd (make-instance 'piano-kbd :xy '(0 0)))
      (c (make-instance 'piano-key :xy '(0 0) :color 'white :black-group 'b2)))
  (setf (surface-collection kbd) (list c))

  ;; types
  (assert (eq 'piano-kbd (visobj-type kbd)))
  (assert (eq 'piano-key (visobj-type c)))
  (assert (eq 'piano-kbd-feature (visloc-type kbd)))
  (assert (eq 'piano-key-feature (visloc-type c)))

  ;; visual features
  (assert (equal '(black-group b2) 
                 (slot->feature c 'black-group)))
  (assert (equal (object->features c +actr-features+)
                 '(SCREEN-X 0 SCREEN-Y 0 VALUE EMPTY COLOR WHITE HEIGHT 1 WIDTH 1)))
  (assert (equal (object->features c (visual-features c))
                 '(BLACK-GROUP B2 FINGER EMPTY FSTATUS EMPTY)))
  
  (assert (equal (visual-object-features c)
                 '(SCREEN-X 0 SCREEN-Y 0 VALUE EMPTY COLOR WHITE HEIGHT 1 WIDTH 1 BLACK-GROUP B2 FINGER EMPTY FSTATUS EMPTY)))
  (assert (equal (visual-object-features c :isa t)
                 '(ISA (PIANO-KEY-FEATURE PIANO-KEY) SCREEN-X 0 SCREEN-Y 0 VALUE EMPTY COLOR WHITE HEIGHT 1 WIDTH 1 BLACK-GROUP B2 FINGER EMPTY FSTATUS EMPTY)))
  )

;;;
(print "chunk-type and chunk definitions" *standard-output*)
;;;
(let ((c (make-instance 'piano-key :xy '(0 0) :color 'white :black-group 'b2)))
  (clear-all)
  (define-model test)
  (with-model test
    (assert (equal (define-chunk-types c)
                   '(PIANO-KEY PIANO-KEY-FEATURE)))
    (assert (null (define-chunk-types c))))
  )

;;;
(print "add to visicon" *standard-output*)
(terpri)
;;;
(let ((c (make-instance 'piano-key :xy '(0 0) :color 'white :black-group 'b2)))
  (clear-all)
  (define-model test)
  (with-model test    
    (define-chunk-types c)
    (define-chunks b2)
    
    (add-to-visicon c)

    (run-n-events 5)
    (print-visicon)
    ))

;;;
(print "modify visicon" *standard-output*)
(terpri)
;;;
(let ((c (make-instance 'piano-key :xy '(0 0) :color 'white :black-group 'b2)))
  (clear-all)
  (define-model test)
  (with-model test
    (define-chunk-types c)
    (define-chunks b2)
    (define-chunks r1)
    
    (add-to-visicon c)

    (run-n-events 5)
    (print-visicon)

    (setf (finger c) 'r1)
    (modify-visicon c)

    (run-n-events 5)
    (print-visicon)                   
    ))

;;;
(print "delete from visicon" *standard-output*)
(terpri)
;;;
(let ((c (make-instance 'piano-key :xy '(0 0) :color 'white :black-group 'b2)))
  (clear-all)
  (define-model test)
  (with-model test
    (define-chunk-types c)
    (define-chunks b2)
    
    (add-to-visicon c)

    (run-n-events 5)
    (print-visicon)

    (delete-from-visicon c)

    (run-n-events 5)
    (print-visicon)                   
    ))


;;;
(print "visual search on visicon" *standard-output*)
(terpri)
;;;
(let ((kbd (make-instance 'piano-kbd :xy '(0 0)))
      (c (make-instance 'piano-key :xy '(0 0) :color 'white :black-group 'b2 :finger 'r1))
      (d (make-instance 'piano-key :xy '(1 0) :color 'white :black-group 'b2 :finger 'r2))
      (e (make-instance 'piano-key :xy '(2 0) :color 'white :black-group 'b2 :finger 'r3))
      (f (make-instance 'piano-key :xy '(3 0) :color 'white :black-group 'b3 :finger 'r4))
      (g (make-instance 'piano-key :xy '(4 0) :color 'white :black-group 'b3 :finger 'r5))
      (a (make-instance 'piano-key :xy '(5 0) :color 'white :black-group 'b3))
      (b (make-instance 'piano-key :xy '(6 0) :color 'white :black-group 'b3)))
  (setf (surface-collection kbd) (list c d e f g a b))

  (clear-all)
  (setf (device-instance "kbd") kbd)

  (define-model test
    (define-chunks b2 b3 r1 r2 r3 r4 r5)
    (dolist (visobj (append (list (device-instance "kbd")) 
                            (surface-collection (device-instance "kbd"))))
      (define-chunk-types visobj))

    (add-to-visicon (device-instance "kbd"))

    (p find-rh-f3
       =visual-search>
       finger r3
       screen-x =x
       ==>
       !output! (r3 is on screen-x =x)
       !stop!
       ))
   
  (with-model test
   
    (run-n-events 5)
    (print-visicon)

    (run 3))

  (setf (device-instance "kbd") nil))

(delete-model test)
(unintern 'piano-kbd)
(unintern 'piano-key)


;;; eof