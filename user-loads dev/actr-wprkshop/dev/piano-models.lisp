;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;;
;;; piano-models.lisp
;;;
;;; 2026-06-07
;;; Bruno Emond bruno.emond@icloud.com
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The purpose of this code is to enable cognitive modelling of 
;;; piano learning and performance using the ACT-R cognitive architecture. 
;;; The code has been tested for LispWorks ans Steel Bank Common Lisp.
;;;
;;; What gets loaded in DM
;;; What can you do with requests

(defun run-model1 ()
  (model1)
  (run 10)
   ;; Fingers are located on keys. Note however that the visicon does not allow to have two fingers on the same key.
  (print-visicon)
  ;; Howver the model knows about the finger posisions from the hand-tracker.
  (show-hands-on-piano))
 
(defun model1 ()
  "A minimal model to have a piano device installed and hands moved to the keyboard."
  (clear-all)

  ;; Creates a piano compomnent and piano device
  (make-piano)

  (define-model foo

    ;; Installs the piano device for the motor module
    (install-device '("motor" "piano"))

    ;; Fill the visison with piano keys.  
    (make-piano-visible)

    (chunk-type goal task)

    ;; Makes all note names available to the the model. 
    (define-chunks-fct (all-note-names))

    (define-chunks 
     ;; Hand-on-piano movement styles, just placing the hands on the piano.
     (right-hand-on-piano isa hand-to-piano cmd hand-to-piano hand right thumb c4 index d4 middle e4 ring f4 pinkie g4)
     (left-hand-on-piano  isa hand-to-piano cmd hand-to-piano hand left  thumb c4 index b3 middle a3 ring g3 pinkie f3)
      
     (start isa goal task left-hand-on-piano))

    (goal-focus start)

    (p left-hand-on-piano
       =goal>
       task left-hand-on-piano
       task =hand-on-piano

       ?manual>
       state free

       ==>

       =goal>
       task right-hand-on-piano

       +manual> =hand-on-piano)

    (p right-hand-on-piano
       =goal>
       task right-hand-on-piano
       task =hand-on-piano

       ?manual>
       state free

       ==>

       -goal>

       +manual> =hand-on-piano)
   
    ) 
  )

(defparameter x nil)

(defun run-model2 ()
  (model2)

  (run 10)
  )

(defun model2 ()
  "A model where C4 is identifed from its visual property and then played with the right thumb."
  (clear-all)

  ;; Creates a piano compomnent and piano device
  (make-piano)

  (define-model foo

    ;(sgp :auto-attend t)

    ;; Installs the piano device for the motor module
    (install-device '("motor" "piano"))

    ;; Fill the visison with piano keys.  
    (make-piano-visible)

    (chunk-type visual-note step octave visual-grp group-pos color)

    (define-chunks look-for found play)

    (define-chunks 
     (goal isa visual-note step look-for octave o4 visual-grp black2 group-pos left color white))

    (goal-focus goal)

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
       step found

       +visual-location> 
       octave     =octave
       visual-grp =visual-grp 
       group-pos  =group-pos 
       color      =color
       )

    (p found-key
       =goal>
       step found

       ?visual-location>
       state free
       buffer full

       =visual-location>
       screen-x =x
       screen-y =y

       !safe-bind! =xy (vector =x =y)

       ?manual>
       state free

       ==>
       =goal>
       step next1

       +manual>
       cmd press-keys-bloc
       hand right
       thumb =xy     
       )

    (p next1
       =goal>
       step next1

       ?manual>
       state free

       ==>
      ; =goal>
      ; step next2

       -goal>

       +manual>
       cmd press-keys-bloc
       hand right
       middle e4     
       )

    (p next2
       =goal>
       step next2

       ?manual>
       state free

       ==>
       -goal>

       +manual>
       cmd release-keys
       hand right
       thumb c4
       )
   
    ) 
  )


(defun run-model3 ()
  (model3)

  (run 10)
  )

(defun model3 ()
  "Playing a chord."
  (clear-all)

  ;; Creates a piano compomnent and piano device
  (make-piano)

  (define-model foo

    ;(sgp :auto-attend t)

    ;; Installs the piano device for the motor module
    (install-device '("motor" "piano"))

    ;; Fill the visison with piano keys.  
    (make-piano-visible)

    (chunk-type visual-note step octave visual-grp group-pos color)

    (define-chunks look-for found play)

    (define-chunks 
     (goal isa visual-note step look-for octave o4 visual-grp black2 group-pos left color white))

    (goal-focus goal)

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
       step found

       +visual-location> 
       octave     =octave
       visual-grp =visual-grp 
       group-pos  =group-pos 
       color      =color
       )

    (p found-key-right
       =goal>
       step found

       ?visual-location>
       state free
       buffer full

       =visual-location>
       screen-x =x
       screen-y =y

       !safe-bind! =xy (vector =x =y)

       ?manual>
       preparation free

       ?manual-right>
       processor free
       execution free
       

       ==>
       =goal>

       =visual-location>

       +manual>
       cmd press-keys-bloc
       hand right
       thumb =xy
       middle (white 3 up thumb)
       pinkie (white 3 up middle)

       )

    (p found-key-left
       =goal>
       step found

       ?visual-location>
       state free
       buffer full

       =visual-location>
       screen-x =x
       screen-y =y

       !safe-bind! =xy (vector =x =y)

       ?manual>
       preparation free

       ?manual-right>
       processor busy
       execution busy
       

       ==>
       -goal>

       +manual>
       cmd press-keys-bloc
       hand left
       thumb =xy
       middle (white 3 down thumb)
       pinkie (white 3 down middle)
       )

    

    
   
    ) 
  )







#|
(defun model1 ()
  (clear-all)
  (make-piano)
  (define-model foo
    (sgp :trace-detail high)
    (install-device '("motor" "piano"))
    (make-piano-visible)

    (chunk-type goal task)

    

    (define-chunks 
     (g0 isa goal task start)
     (move-right-hand-home isa goal task right-home)
     (move-left-hand-home  isa goal task left-home))

     (goal-focus g0)

    (p start
       =goal>
       task start
       ==>
       =goal>
       task left-home)

    (p end
       =goal>
       task end
       ==>
       -goal>)

    (p left-hand-at-home
       =goal>
       task left-home
       task =task

       ?manual>
       state free
       ==>
       =goal>
       task right-home

       +manual> =task)

    (p right-hand-at-home
       =goal>
       task right-home
       task =task

       ?manual>
       state free
       ==>
       =goal>
       task next1

       +manual> =task)


    (p next1
       =goal>
       task next1
       
       ?manual>
       state free
       ==>
       =goal>
       task end

       +manual>
       cmd press-keys-broken
       duration 1
       press-offset 1
       hand right
       thumb c4
       middle e4
       pinkie g4


       )

 #|  
    (p next2
       =goal>
       task next2
       
       ?manual>
       state free
       ==>
       =goal>
       task next3

       +manual>
       cmd release-fingers
       hand right
       thumb t
       middle t
       pinkie t
       )

    (p next3
       =goal>
       task next3
       
       ?manual>
       state free
       ==>
       =goal>
       task end

       +manual>
       cmd press-keys
       hand right
       thumb c4
       index d4is
       pinkie g4
       )
|#
    ))

(defun model2 ()
  (clear-all)
  (make-piano)
  (define-model foo
    (sgp :trace-detail high
         :overstuff-aural-location t)
    (install-device '("motor" "piano"))
    (make-piano-visible)

    (chunk-type goal task)

    (define-chunks 
        (right-home isa hand-to-piano cmd hand-to-piano hand right thumb c4 index d4 middle e4 ring f4 pinkie g4)
        (left-home  isa hand-to-piano cmd hand-to-piano hand left  thumb c4 index b3 middle a3 ring g3 pinkie f3))

    (define-chunks 
     (g0 isa goal task start)
     (move-right-hand-home isa goal task right-home)
     (move-left-hand-home  isa goal task left-home))

     (goal-focus g0)

    (p start
       =goal>
       task start
       ==>
       =goal>
       task left-home)

    (p end
       =goal>
       task end
       ==>
       -goal>)

    (p left-hand-at-home
       =goal>
       task left-home
       task =task

       ?manual>
       state free
       ==>
       =goal>
       task right-home

       +manual> =task)

    (p right-hand-at-home
       =goal>
       task right-home
       task =task

       ?manual>
       state free
       ==>
       =goal>
       task next1

       +manual> =task)
    
    (p next1
       =goal>
       task next1
       
       ?manual>
       state free
       ==>
       =goal>
       task next2

       +manual>
       cmd press-keys-bloc
       ;duration 1
       ;press-offset 1
       hand right
       thumb c4
       middle e4
       pinkie g4


       )

    (p next2
       =goal>
       task next2
       
       ?aural>
       state free

       =aural-location>
       event =event
       ==>
       =goal>
       task end

       !output! (=event)

       )

   

    ))

|#
;;; eof