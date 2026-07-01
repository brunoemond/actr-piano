;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;;
;;; piano-bimanual-models.lisp
;;;
;;; 2026-06-29
;;; Bruno Emond bruno.emond@icloud.com
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file contains a set of models to show some functionality of piano-bimanual. 
;;;
;;; The example models focus on finger movements defined in chunks. These models do 
;;; not include any declarative memory processes. 
;;;
;;; The file piano-bimanual.lisp needs to be placed in the ACT-R user-loads directory. 
;;; The current file piano-bimanual-models.lisp could be anywhere. 
;;; If used with the environment, simply use the load button to load the file. 
;;; You can ignore the undefined function warnings caused by compilation. 
;;; Just call (run-model1), (run-model2), or (run-model3), in the environment command window.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To do:
;;; Include temporal offset in finger movements. 

(defun include-two-hands-productions ()
  "These productions are generic productions for moving, pressing and releasing fingers. 
   They can be imported in piano-bimanual models by inserting the finction call in a model.
   See example models below."

  (p move-fingers->next
     =goal>
     cmd move-fingers
     next =next

     ?manual>
     preparation free

     ==>
     +goal> =next

     +manual> =goal)

  (p move-fingers->stop
     =goal>
     cmd move-fingers
     next stop

     ?manual>
     preparation free

     ==>
     -goal>

     +manual> =goal)

  (p press-fingers->next
     =goal>
     cmd press-fingers
     next =next

     ?manual>
     preparation free

     ==>
     +goal> =next

     +manual> =goal)

  (p press-fingers->stop
     =goal>
     cmd press-fingers
     next stop

     ?manual>
     preparation free

     ==>
     -goal>

     +manual> =goal)

  (p release-fingers->next
     =goal>
     cmd release-fingers
     next =next

     ?manual>
     preparation free

     ==>
     +goal> =next

     +manual> =goal)

  (p release-fingers->stop
     =goal>
     cmd release-fingers
     next stop

     ?manual>
     preparation free

     ==>
     -goal>

     +manual> =goal)
  )


(defun run-model1 ()
  (model1)
  (run 10)
   ;; Fingers are located on keys. Note however that the visicon does not allow to have two fingers on the same key.
  (print-visicon)
  ;; But the model knows about the finger positions from the hand-tracker.
  (show-hands-on-piano))
 
(defun model1 ()
  "A minimal model to have a piano device installed and hands moved to the keyboard.
   The hands are moved as 2 separate movements but they could be moved as one movement
   using a dfferent chunk. See model 6."
  (clear-all)

  ;; Creates a piano compomnent and piano device
  (make-piano)

  (define-model foo
    (sgp :trace-detail high)

    ;; Installs the piano device for the motor module
    (install-device '("motor" "piano"))

    ;; Fills the visison with piano keys.  
    (make-piano-visible)

    ;; Defines chunk types for placing hands on the piano with thumbs as anchors.
    ;; Adds a next slot to the move-fingers style chunk-type.
    (chunk-type (move-rh-with-thumb (:include move-fingers))
                (r-index  (white 2 up r-thumb))
                (r-middle (white 2 up r-index))
                (r-ring   (white 2 up r-middle))                         
                (r-pinkie (white 2 up r-ring))
                next)
    (chunk-type (move-lh-with-thumb (:include move-fingers))
                (l-index  (white 2 down l-thumb))
                (l-middle (white 2 down l-index))
                (l-ring   (white 2 down l-middle))                         
                (l-pinkie (white 2 down l-ring))
                next)

    ;; Makes all note names available to the the model. 
    (define-chunks-fct (all-note-names))

    (define-chunks stop)

    (define-chunks 
     ;; Hand-on-piano movement styles, just placing the hands on the piano.
     (place-lh-on-piano isa move-lh-with-thumb l-thumb c4 next place-rh-on-piano)
     (place-rh-on-piano isa move-rh-with-thumb r-thumb c4 next stop)
     )
      
    (goal-focus place-lh-on-piano)

    (include-two-hands-productions)
    )
  )

(defun run-model2 ()
  (model2)
  (run 10)
  (print-visicon)
  (show-hands-on-piano))

(defun model2 ()
  "A minimal model to have a piano device installed and hands moved to the keyboard.
   Contrary to model1, this model uses the visual-location of a key to place hands."

   (clear-all)

   (make-piano)

  (define-model foo
    (sgp :trace-detail high)
    (install-device '("motor" "piano"))
    (make-piano-visible)
    (chunk-type (move-rh-with-thumb (:include move-fingers))
                (r-index  (white 2 up r-thumb))
                (r-middle (white 2 up r-index))
                (r-ring   (white 2 up r-middle))                         
                (r-pinkie (white 2 up r-ring))
                next)

    ;; a goal chunk to visually search the keyboard
    (chunk-type visual-key-search octave visual-grp group-pos color step)

    (define-chunks look-for found?)

    (define-chunks stop)

    (define-chunks 
     (start isa visual-key-search step look-for 
            octave o4 
            visual-grp black2 
            group-pos left
            color white))

    
    (goal-focus start)

    (include-two-hands-productions)

    (p look-for-key
       =goal>
       step look-for
       octave     =octave
       visual-grp =visual-grp 
       group-pos  =group-pos 
       color      =color

       ?visual-location>
       state free
       buffer empty

       ==>
       =goal>
       step found?

       +visual-location> 
       octave     =octave
       visual-grp =visual-grp 
       group-pos  =group-pos 
       color      =color
       )

    (p found-key
       =goal>
       step found?

       ?visual-location>
       state free
       buffer full

       =visual-location>
 
       ?manual>
       state free

       ==>
       +goal>
       isa move-rh-with-thumb
       r-thumb =visual-location
       next    stop
       )  
    ) 
  )


(defun run-model3 ()
  (model3)
  (run 10)
  (show-hands-on-piano))
 
(defun model3 ()
  "A model that plays a C major scale in contrary motion."
  (clear-all)

  (make-piano)

  (define-model foo
    (sgp :trace-detail high)
    (install-device '("motor" "piano"))
    (make-piano-visible)

    ;; Defines chunk types for placing hands on the piano with thumbs as anchors.
    ;; Adds a next slot to the move-fingers style chunk-type.
    (chunk-type (move-both-with-thumb (:include move-fingers))
                (r-index  (white 2 up r-thumb))
                (r-middle (white 2 up r-index))
                (r-ring   (white 2 up r-middle))                         
                (r-pinkie (white 2 up r-ring))
                (l-index  (white 2 down l-thumb))
                (l-middle (white 2 down l-index))
                (l-ring   (white 2 down l-middle))                         
                (l-pinkie (white 2 down l-ring))
                next)
    (chunk-type (press-hand-fingers (:include press-fingers))
                next)
    (chunk-type (release-hand-fingers (:include release-fingers))
                next)


    ;; Makes all note names available to the the model. 
    (define-chunks-fct (all-note-names))

    (define-chunks stop)

    (define-chunks 
     ;; moving the thumb under the hand
     (place-hands-on-piano  isa move-both-with-thumb   r-thumb c4   l-thumb c4    next press-thumb1)


     (press-thumb1       isa press-hand-fingers   r-thumb t      next release-thumb1)
     (release-thumb1     isa release-hand-fingers r-thumb t      next press-index1)

     (press-index1       isa press-hand-fingers   r-index t  l-index t   next release-index1)
     (release-index1     isa release-hand-fingers r-index t  l-index t   next press-middle1)

     (press-middle1      isa press-hand-fingers   r-middle t l-middle t   next release-middle1)
     (release-middle1    isa release-hand-fingers r-middle t l-middle t   next roll-thumb-at-ring)

     (roll-thumb-at-ring isa move-both-with-thumb r-thumb r-ring l-thumb l-ring next press-thumb2)

     (press-thumb2       isa press-hand-fingers   r-thumb t  l-thumb t    next release-thumb2)
     (release-thumb2     isa release-hand-fingers r-thumb t  l-thumb t    next press-index2)

     (press-index2       isa press-hand-fingers   r-index t  l-index t    next release-index2)
     (release-index2     isa release-hand-fingers r-index t  l-index t    next press-middle2)

     (press-middle2      isa press-hand-fingers   r-middle t l-middle t   next release-middle2)
     (release-middle2    isa release-hand-fingers r-middle t l-middle t    next press-ring1)

     (press-ring1        isa press-hand-fingers   r-ring t   l-ring t    next release-ring1)
     (release-ring1      isa release-hand-fingers r-ring t   l-ring t    next press-pinkie1)

     (press-pinkie1      isa press-hand-fingers   r-pinkie t l-pinkie t   next release-pinkie1)
     (release-pinkie1    isa release-hand-fingers r-pinkie t  l-pinkie t    next stop)
     )
      
    (goal-focus place-hands-on-piano)

    (include-two-hands-productions)
    )
  )

;;; eof