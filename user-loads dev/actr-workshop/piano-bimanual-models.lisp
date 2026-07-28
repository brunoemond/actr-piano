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

(setf *fs-bin* "/opt/homebrew/Cellar/fluid-synth/2.5.2/bin/fluidsynth")
(setf *soundfont* "/library/audio/sounds/banks/FluidR3_GM/FluidR3_GM.sf2")


;;;
;;; temporal
;;;

(defparameter *time-master-start-increment* 0.011)
(defparameter *time-mult* 1.1)

(defun tick-length-no-noise (tick)
  "Duration of the TICKth tick."
  (if (zerop tick)
      *time-master-start-increment*
    (* *time-mult*
       (tick-length-no-noise (1- tick)))))

(defun elapsed-sec-from-ticks-no-noise-recusion (ticks)
  "Total elapsed time (seconds) after TICKS ticks."
  (if (zerop ticks)
      0.0
    (+ (tick-length-no-noise (1- ticks))
       (elapsed-sec-from-ticks-no-noise-recusion (1- ticks)))))

(defun elapsed-sec-from-ticks-no-noise (ticks)
  "Total elapsed time (seconds) after ticks."
  (/ (* *time-master-start-increment*
        (- (expt *time-mult* ticks) 1))
     (- *time-mult* 1)))

(defun ticks-from-sec-no-noise (seconds)
  "Number of ticks whose summed durations reach SECONDS."
  (/ (log (1+ (/ (* seconds (1- *time-mult*))
                        *time-master-start-increment*)))
     (log *time-mult*)))

(defun estimate-ticks (secs)
  (ticks-from-sec-no-noise secs))

(defparameter *bpm* 60 "Beats per minute.")
(defparameter *beats-per-measure* 4 "Top number of time signature.")  
(defparameter *beat-subdivision*  1 "1 = quarter notes, 2 = eighth notes, 4 = sixteenth notes.")

(defparameter *last-count*      (* *beats-per-measure* *beat-subdivision*) "Total number of counts.")
(defparameter *ticks-per-count* (estimate-ticks (/ (/ 60 *bpm*) ;beat duration
                                                   *beat-subdivision*)))

(defun set-timing ()
  (setf *last-count* (* *beats-per-measure* *beat-subdivision*)
        *ticks-per-count* (estimate-ticks (/ (/ 60 *bpm*) *beat-subdivision*))))
  

(defparameter *counter* 0)

(defun initialize-counter ()
  (car (define-chunks-fct 
        `((isa counter 
               count      ,(setf *counter* 1) 
               duration   ,*ticks-per-count*)))))

(defun no-chunk-generated ()
  nil)

(defun update-counter (slot)
  (schedule-mod-buffer-chunk 
   'imaginal (list slot (if (< *counter* *last-count*)
                            (setf *counter* (1+ *counter*))
                          (setf *counter* 1))) 
   0.05 :module 'imaginal)
  (schedule-event-relative 0.05 'set-imaginal-free :module 'imaginal :priority -10))

;;;
;;; generic-bimanual-productions
;;;
(defun include-bimanual-types ()
  (chunk-type counter count duration)
  (chunk-type start-counter at-count next))



(defun include-bimanual-productions ()
  "These productions are generic productions for moving, pressing and releasing fingers. 
   They can be imported in piano-bimanual models by inserting the function call in a model.
   See example models below."  
   
  ;;
  ;; no counting
  ;;
  (p bimanual-action
     ?imaginal>
     state free

     ?manual>
     state free

     =goal>
     at-count nil
     next     =next-goal

     ==>
     +goal>   =next-goal

     +manual> =goal
     )

  ;;
  ;; with counting
  ;;
  (p no-action-just-count
     ?imaginal>
     state free

     =goal>
     at-count   =at-count

     =imaginal>
     - count    =at-count
     duration   =duration

     =temporal>
     >= ticks   =duration

     ==>
     =goal> 

     +imaginal-action>
     isa    generic-action
     action update-counter
     slots  (count)

     +temporal> ticks 0)

  (p bimanual-action-on-count
     ?imaginal>
     state free

     ?manual>
     preparation free ;state or preparation

     =goal>
     at-count   =at-count
     next       =next-goal

     =imaginal>
     count      =at-count
     duration   =duration

     =temporal>
     >= ticks   =duration

     ==>
     +goal>     =next-goal

     +manual>   =goal

     +imaginal-action>
     isa    generic-action
     action update-counter
     slots  (count)

     +temporal> ticks 0)

  (p bimanual-action-keep-count
     ?imaginal>
     state free
     buffer full

     ?manual>
     preparation free ;state or preparation

     =goal>
     at-count   =at-count
     next       =next-goal
     keep-count t
 
     =imaginal>
     count      =at-count
     duration   =duration

     =temporal>
     >= ticks   =duration

     ==>
     +goal> =next-goal

     +manual> =goal
     )

  )

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

    (include-bimanual-types)

    ;; Defines chunk types for placing hands on the piano with thumbs as anchors.
    ;; Adds a next slot to the move-fingers style chunk-type.
    (chunk-type (move-rh-with-thumb (:include move-fingers))
                (r-index  (white 2 up r-thumb))
                (r-middle (white 2 up r-index))
                (r-ring   (white 2 up r-middle))                         
                (r-pinkie (white 2 up r-ring))
                at-count keep-count next)
    (chunk-type (move-lh-with-thumb (:include move-fingers))
                (l-index  (white 2 down l-thumb))
                (l-middle (white 2 down l-index))
                (l-ring   (white 2 down l-middle))                         
                (l-pinkie (white 2 down l-ring))
                at-count keep-count next)

    ;; Makes all note names available to the the model. 
    (define-chunks-fct (all-note-names))

    (define-chunks stop)

    (define-chunks 
     ;; Hand-on-piano movement styles, just placing the hands on the piano.
     (place-lh-on-piano isa move-lh-with-thumb l-thumb c4 next place-rh-on-piano)
     (place-rh-on-piano isa move-rh-with-thumb r-thumb c4 next stop)
     )
      
    (goal-focus place-lh-on-piano)

    (include-bimanual-productions)
    )
  )

(defun run-model1 ()
  (model1)
  (run 10)
   ;; Fingers are located on keys. Note however that the visicon does not allow to have two fingers on the same key.
  (print-visicon)
  ;; But the model knows about the finger positions from the hand-tracker.
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

    (include-bimanual-types)

    (chunk-type (move-rh-with-thumb (:include move-fingers))
                (r-index  (white 2 up r-thumb))
                (r-middle (white 2 up r-index))
                (r-ring   (white 2 up r-middle))                         
                (r-pinkie (white 2 up r-ring))
                at-count keep-count next)

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

    (include-bimanual-productions)

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

(defun run-model2 ()
  (model2)
  (run 10)
  (print-visicon)
  (show-hands-on-piano))


 
(defun model3 ()
  "A model that plays a C major scale in contrary motion. No timing on keys."
  (clear-all)

  (make-piano)

  (define-model foo
    (sgp :trace-detail high)
    (install-device '("motor" "piano"))
    (make-piano-visible)

    (include-bimanual-types)

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
                 at-count keep-count next)
    (chunk-type (press-fingers-goal (:include press-fingers))
                 at-count keep-count next)
    (chunk-type (release-fingers-goal (:include release-fingers))
                 at-count keep-count next)

    ;; Makes all note names available to the the model. 
    (define-chunks-fct (all-note-names))

    (define-chunks stop)

    (define-chunks 

     (place-hands-on-piano  isa move-both-with-thumb   r-thumb c4   l-thumb c4    next press-thumb1)

     (press-thumb1       isa press-fingers-goal   r-thumb t      next release-thumb1)

     (release-thumb1     isa release-fingers-goal r-thumb t      next press-index1)
     (press-index1       isa press-fingers-goal   r-index t  l-index t   next release-index1)

     (release-index1     isa release-fingers-goal r-index t  l-index t   next press-middle1)
     (press-middle1      isa press-fingers-goal   r-middle t l-middle t   next release-middle1)

     (release-middle1    isa release-fingers-goal r-middle t l-middle t   next roll-thumb-at-ring)
     (roll-thumb-at-ring isa move-both-with-thumb r-thumb r-ring l-thumb l-ring next press-thumb2)

     (press-thumb2       isa press-fingers-goal   r-thumb t  l-thumb t    next release-thumb2)

     (release-thumb2     isa release-fingers-goal r-thumb t  l-thumb t    next press-index2)
     (press-index2       isa press-fingers-goal   r-index t  l-index t    next release-index2)

     (release-index2     isa release-fingers-goal r-index t  l-index t    next press-middle2)
     (press-middle2      isa press-fingers-goal   r-middle t l-middle t   next release-middle2)

     (release-middle2    isa release-fingers-goal r-middle t l-middle t    next press-ring1)
     (press-ring1        isa press-fingers-goal   r-ring t   l-ring t    next release-ring1)

     (release-ring1      isa release-fingers-goal r-ring t   l-ring t    next press-pinkie1)
     (press-pinkie1      isa press-fingers-goal   r-pinkie t l-pinkie t   next release-pinkie1)

     (release-pinkie1    isa release-fingers-goal r-pinkie t  l-pinkie t    next stop)
     )
      
    (goal-focus place-hands-on-piano)

    (include-bimanual-productions)
   
    )
  )

(defun run-model3 ()
  (model3)
  (run 10)
  (show-hands-on-piano))


 
(defun model4 ()
  "A model that plays a C major scale in contrary motion. With timing on keys."
  (clear-all)

  (make-piano)

  (define-model foo
    (sgp :do-not-harvest imaginal :trace-detail low)
    (install-device '("motor" "piano"))
    (setf *bpm* 120
          *beats-per-measure* 4 
          *beat-subdivision*  1)
    (set-timing)

    (make-piano-visible)

    (include-bimanual-types)

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
                 at-count keep-count next)

    (chunk-type (press-fingers-goal (:include press-fingers))
                 at-count keep-count next)
    (chunk-type (release-fingers-goal (:include release-fingers))
                 at-count keep-count next)

    

    ;; Makes all note names available to the the model. 
    (define-chunks-fct (all-note-names))

    (define-chunks stop)

    (define-chunks 
     (place-hands-on-piano  isa move-both-with-thumb   r-thumb c4   l-thumb c4    next start-counter)

     (start-counter      isa start-counter at-count 0 next press-thumb1)

     (press-thumb1       isa press-fingers-goal   r-thumb t              next release-thumb1     at-count 1)

     (release-thumb1     isa release-fingers-goal r-thumb t              next press-index1       at-count 2 keep-count t)
     (press-index1       isa press-fingers-goal   r-index t  l-index t   next release-index1     at-count 2)

     (release-index1     isa release-fingers-goal r-index t  l-index t   next press-middle1      at-count 3 keep-count t)
     (press-middle1      isa press-fingers-goal   r-middle t l-middle t  next release-middle1    at-count 3)

     (release-middle1    isa release-fingers-goal r-middle t l-middle t  next roll-thumb-at-ring at-count 4 keep-count t)

     (roll-thumb-at-ring isa move-both-with-thumb r-thumb r-ring l-thumb l-ring next press-thumb2)

     (press-thumb2       isa press-fingers-goal   r-thumb t  l-thumb t   next release-thumb2     at-count 4)

     (release-thumb2     isa release-fingers-goal r-thumb t  l-thumb t   next press-index2       at-count 1 keep-count t)
     (press-index2       isa press-fingers-goal   r-index t  l-index t   next release-index2     at-count 1)

     (release-index2     isa release-fingers-goal r-index t  l-index t   next press-middle2      at-count 2 keep-count t)
     (press-middle2      isa press-fingers-goal   r-middle t l-middle t  next release-middle2    at-count 2)

     (release-middle2    isa release-fingers-goal r-middle t l-middle t  next press-ring1        at-count 3 keep-count t)
     (press-ring1        isa press-fingers-goal   r-ring t   l-ring t    next release-ring1      at-count 3)

     (release-ring1      isa release-fingers-goal r-ring t   l-ring t    next press-pinkie1      at-count 4 keep-count t)
     (press-pinkie1      isa press-fingers-goal   r-pinkie t l-pinkie t  next release-pinkie1    at-count 4 keep-count t)

     (release-pinkie1    isa release-fingers-goal r-pinkie t  l-pinkie t next stop               at-count 1)

     )
      
    (goal-focus place-hands-on-piano)

    (include-bimanual-productions)

    (p start-counter
       ?imaginal>
       state free
       buffer empty

       ?manual>
       state free

       =goal>
       at-count 0
       next =next-goal

       ==>
       +goal> =next-goal

       +imaginal-action>
       isa simple-action
       action initialize-counter

       +temporal> ticks 0)
    )
  )

(defun model4b ()
  "A model that plays a C major scale in contrary motion. With timing on keys."
  (clear-all)

  (make-piano)

  (define-model foo
    (sgp :do-not-harvest imaginal :trace-detail low)
    (install-device '("motor" "piano"))
    (setf *bpm* 120
          *beats-per-measure* 4 
          *beat-subdivision*  1)
    (set-timing)

    (make-piano-visible)

    (include-bimanual-types)

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
                 at-count keep-count next)

    (chunk-type (keys-action-goal (:include keys-action))
                at-count keep-count next)   

    ;; Makes all note names available to the the model. 
    (define-chunks-fct (all-note-names))

    (define-chunks stop)

    (define-chunks 
     (place-hands-on-piano  isa move-both-with-thumb   r-thumb c4   l-thumb c4    next start-counter)

     (start-counter      isa start-counter at-count 0 next c)

     (c    isa keys-action-goal   r-thumb press                                                    next db     at-count 1)
     (db   isa keys-action-goal   r-thumb release  r-index press l-index press                     next ea     at-count 2)
     (ea   isa keys-action-goal   r-index release  l-index release r-middle press l-middle press   next roll   at-count 3 keep-count t)
     (ea   isa keys-action-goal   r-index release  l-index release r-middle press l-middle press   next roll   at-count 3 keep-count t)
     (roll isa move-both-with-thumb r-thumb r-ring l-thumb l-ring next fg at-count 3)



     (db     isa release-fingers-goal r-thumb t              next press-index1       at-count 2 keep-count t)
     (press-index1       isa press-fingers-goal   r-index t  l-index t   next release-index1     at-count 2)

     (release-index1     isa release-fingers-goal r-index t  l-index t   next press-middle1      at-count 3 keep-count t)
     (press-middle1      isa press-fingers-goal   r-middle t l-middle t  next release-middle1    at-count 3)

     (release-middle1    isa release-fingers-goal r-middle t l-middle t  next roll-thumb-at-ring at-count 4 keep-count t)

     (roll-thumb-at-ring isa move-both-with-thumb r-thumb r-ring l-thumb l-ring next press-thumb2)

     (press-thumb2       isa press-fingers-goal   r-thumb t  l-thumb t   next release-thumb2     at-count 4)

     (release-thumb2     isa release-fingers-goal r-thumb t  l-thumb t   next press-index2       at-count 1 keep-count t)
     (press-index2       isa press-fingers-goal   r-index t  l-index t   next release-index2     at-count 1)

     (release-index2     isa release-fingers-goal r-index t  l-index t   next press-middle2      at-count 2 keep-count t)
     (press-middle2      isa press-fingers-goal   r-middle t l-middle t  next release-middle2    at-count 2)

     (release-middle2    isa release-fingers-goal r-middle t l-middle t  next press-ring1        at-count 3 keep-count t)
     (press-ring1        isa press-fingers-goal   r-ring t   l-ring t    next release-ring1      at-count 3)

     (release-ring1      isa release-fingers-goal r-ring t   l-ring t    next press-pinkie1      at-count 4 keep-count t)
     (press-pinkie1      isa press-fingers-goal   r-pinkie t l-pinkie t  next release-pinkie1    at-count 4 keep-count t)

     (release-pinkie1    isa release-fingers-goal r-pinkie t  l-pinkie t next stop               at-count 1)

     )
      
    (goal-focus place-hands-on-piano)

    (include-bimanual-productions)

    (p start-counter
       ?imaginal>
       state free
       buffer empty

       ?manual>
       state free

       =goal>
       at-count 0
       next =next-goal

       ==>
       +goal> =next-goal

       +imaginal-action>
       isa simple-action
       action initialize-counter

       +temporal> ticks 0)
    )
  )

(defun run-model4 ()
  (model4)
  (run 8)
  (show-hands-on-piano))

(defun run-c-scale ()
  (fs-start)
  (model4)
  (run 8 t)
  (fs-stop)
  )


(defun model5a ()
  "Ode to joy."
  (clear-all)

  (make-piano)

  (define-model foo
    (sgp :do-not-harvest imaginal :trace-detail low)
    (install-device '("motor" "piano"))
    (setf *bpm* 120
          *beats-per-measure* 4 
          *beat-subdivision*  1)
    (set-timing)

    (make-piano-visible)

    (include-bimanual-types)

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
                 at-count keep-count next)

    (chunk-type (press-fingers-goal (:include press-fingers))
                 at-count keep-count next)
    (chunk-type (release-fingers-goal (:include release-fingers))
                 at-count keep-count next)

    

    ;; Makes all note names available to the the model. 
    (define-chunks-fct (all-note-names))

    (define-chunks stop)

    (define-chunks 
     (place-hands-on-piano  isa move-both-with-thumb   r-thumb c4   l-thumb g3    next start-counter)

     (start-counter      isa start-counter at-count 0 next measure-1-1)

     (measure-1-1        isa press-fingers-goal   l-pinkie t l-thumb t r-middle t  next measure-1-2r     at-count 1)

     (measure-1-2r       isa release-fingers-goal r-middle t              next measure-1-2p      at-count 2 keep-count t)
     (measure-1-2p       isa press-fingers-goal   r-middle t              next measure-1-3r      at-count 2)

     (measure-1-3r       isa release-fingers-goal r-middle t              next measure-1-3p      at-count 3 keep-count t)
     (measure-1-3p       isa press-fingers-goal   r-ring t                next measure-1-4r      at-count 3)

     (measure-1-4r       isa release-fingers-goal r-ring t                next measure-1-4p      at-count 4 keep-count t)
     (measure-1-4p       isa press-fingers-goal   r-pinkie t              next measure-2-1r      at-count 4)

     (measure-2-1r       isa release-fingers-goal  l-pinkie t l-thumb t r-pinkie t  next stop     at-count 1)

     )
      
    (goal-focus place-hands-on-piano)

    (include-bimanual-productions)

    (p start-counter
       ?imaginal>
       state free
       buffer empty

       ?manual>
       state free

       =goal>
       at-count 0
       next =next-goal

       ==>
       +goal> =next-goal

       +imaginal-action>
       isa simple-action
       action initialize-counter

       +temporal> ticks 0)
    )
  )

(defun run-model5a ()
  (fs-start)
  (model5a)
  (run 4 t)
  (show-hands-on-piano)
  )


(defun model5b ()
  "A model that plays a C major scale in contrary motion. With timing on keys."
  (clear-all)

  (make-piano)

  (define-model foo
    (sgp :do-not-harvest imaginal :trace-detail low)
    (install-device '("motor" "piano"))
    ;(setf *bpm* 120
     ;     *beats-per-measure* 4 
     ;     *beat-subdivision*  1)
    (set-timing)

    (make-piano-visible)

    (include-bimanual-types)

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
                 at-count keep-count next)

    (chunk-type (keys-action-goal (:include keys-action))
                 at-count keep-count next)

 
    ;; Makes all note names available to the the model. 
    (define-chunks-fct (all-note-names))

    (define-chunks stop)

    (define-chunks 
     (place-hands-on-piano  isa move-both-with-thumb   r-thumb c4   l-thumb g3    next start-counter)

     (start-counter      isa start-counter at-count 0 next measure-1-1)

     (measure-1-1        isa keys-action-goal l-pinkie press l-thumb press r-middle press  next measure-1-2r     at-count 1)

     (measure-1-2r       isa keys-action-goal r-middle release               next measure-1-2p      at-count 2 keep-count t)
     (measure-1-2p       isa keys-action-goal r-middle press                 next measure-1-3       at-count 2)

     (measure-1-3        isa keys-action-goal r-middle release r-ring press  next measure-1-4       at-count 3 keep-count t)
 
     (measure-1-4        isa keys-action-goal r-ring release  r-pinkie press  next measure-2-1      at-count 4 keep-count t)
 
     (measure-2-1        isa keys-action-goal l-pinkie release l-thumb release r-pinkie release  next stop     at-count 1)

     )
      
    (goal-focus place-hands-on-piano)

    (include-bimanual-productions)

    (p start-counter
       ?imaginal>
       state free
       buffer empty

       ?manual>
       state free

       =goal>
       at-count 0
       next =next-goal

       ==>
       +goal> =next-goal

       +imaginal-action>
       isa simple-action
       action initialize-counter

       +temporal> ticks 0)
    )
  )

(defun run-ode120 ()
  (setf *bpm* 120
          *beats-per-measure* 4 
          *beat-subdivision*  1)
  ;(sleep 2)
  (fs-start)
  (model5b)
  (run 4 t)
  (fs-stop)
  )

(defun run-ode60 ()
  (setf *bpm* 60
          *beats-per-measure* 4 
          *beat-subdivision*  1)
  ;(sleep 2)
  (fs-start)
  (model5b)
  (run 6 t)
  (fs-stop)
  )



(defun model6 ()
  "no-chord vs chord."
  (clear-all)

  (make-piano)

  (define-model foo
    (sgp :do-not-harvest imaginal :trace-detail medium)
    (install-device '("motor" "piano"))
    (setf *bpm* 60
          *beats-per-measure* 4 
          *beat-subdivision*  1)
    (set-timing)

    (make-piano-visible)

    (include-bimanual-types)

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
                 at-count keep-count next)

    (chunk-type (keys-action-goal (:include keys-action))
                 at-count keep-count next)

    ;; Makes all note names available to the the model. 
    (define-chunks-fct (all-note-names))

    (define-chunks stop)

    (define-chunks 
     (place-hands-on-piano  isa move-both-with-thumb   r-thumb c4   l-thumb g3    next start-counter)

     (start-counter      isa start-counter at-count 0 next measure-1-1)

     (measure-1-1         isa keys-action-goal   l-pinkie press  next measure-1-1a     at-count 1 keep-count t)
     (measure-1-1a        isa keys-action-goal   l-thumb press   next measure-1-1b     at-count 1 keep-count t)
     (measure-1-1b        isa keys-action-goal   r-middle press  next measure-2-1      at-count 1)

     (measure-2-1         isa keys-action-goal   l-pinkie release l-thumb release r-middle release next measure-2-1a  at-count 2 keep-count t )
     (measure-2-1a        isa keys-action-goal   l-pinkie press l-thumb press r-middle press next measure-3  at-count 2 )

     (measure-3           isa keys-action-goal   l-pinkie release l-thumb release r-middle release next measure-4  at-count 3 )

     (measure-4           isa keys-action-goal   next stop  at-count 4 )
  
     )
      
    (goal-focus place-hands-on-piano)

    (include-bimanual-productions)

    (p start-counter
       ?imaginal>
       state free
       buffer empty

       ?manual>
       state free

       =goal>
       at-count 0
       next =next-goal

       ==>
       +goal> =next-goal

       +imaginal-action>
       isa simple-action
       action initialize-counter

       +temporal> ticks 0)
    )
  )

(defun run-chord ()
  ;(sleep 2)
  (fs-start)
  (model6)
  (run 4 t)
  (fs-stop)
  )



;;; eof