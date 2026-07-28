
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
(defparameter *beat-duration*    (/ 60 *bpm*))
(defparameter *beats-per-measure* 4 "Top number of time signature.")  
(defparameter *beat-subdivision*  1 "1 = quarter notes, 2 = eighth notes, 4 = sixteenth notes.")

(defparameter *last-count*      (* *beats-per-measure* *beat-subdivision*) "Total number of counts.")
(defparameter *ticks-per-count* (estimate-ticks (/ *beat-duration* *beat-subdivision*)))

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
   0 :module 'imaginal)
  (schedule-event-relative 0 'set-imaginal-free :module 'imaginal :priority -10))

(clear-all)

(define-model test-imaginal-action
    
  (sgp :do-not-harvest imaginal :trace-detail high)
  
  (chunk-type counter count duration)
  (chunk-type goal at-count keep-count next)

  (define-chunks
   (start isa goal at-count 0 next say-1)
   (say-1 isa goal at-count 1 next say-3a)
   ;(say-2 isa goal at-count 2 next say-3)
   (say-3a isa goal at-count 3 keep-count t next say-3b)
   (say-3b isa goal at-count 3 next say-4)
   (say-4 isa goal at-count 4 next say-1))

  (goal-focus start)
  
  (p start
     ?imaginal>
     state free
     buffer empty
     error nil

     =goal>
     at-count 0
     next =next-goal

     ==>
     +goal> =next-goal

     +imaginal-action>
     isa simple-action
     action initialize-counter

     +temporal>
     ticks 0)
  
  (p say-count
     ?imaginal>
     state free

     =goal>
     at-count =at-count
     next =next-goal

     =imaginal>
     isa counter
     count      =at-count
     duration   =duration

     =temporal>
     >= ticks =duration

     ==>

     !output! (say-count =at-count)

     +goal> =next-goal

     +imaginal-action>
     isa generic-action
     action update-counter
     slots (count)

     +temporal>
     ticks 0)

  (p keep-count
     ?imaginal>
     state free

     =goal>
     at-count =at-count
     next =next-goal
     keep-count t

     =imaginal>
     isa counter
     count      =at-count
     duration   =duration

     =temporal>
     >= ticks =duration

     ==>

     !output! (say-count =at-count)

     +goal> =next-goal

     )

  (p just-count
     ?imaginal>
     state free

     =goal>
     at-count =at-count

     =imaginal>
     isa counter
     - count    =at-count
     count      =count
     duration   =duration

     =temporal>
     >= ticks =duration

     ==>

     !output! (just-count =count)

     +imaginal-action>
     isa generic-action
     action update-counter
     slots (count)

     +temporal>
     ticks 0)
  
  
  (p stop
     ?imaginal>
     error t

     ==>

     +imaginal>)
  )


#|
(define-model foo

  (sgp :trace-detail high)

  (chunk-type-fct `(timed-goal at-count (duration ,*ticks-per-count*)))
  (chunk-type (say-count (:include timed-goal)) next)
  (chunk-type counter count)

  (define-chunks 
   (start isa say-count at-count 0 next say-1)
   (say-1 isa say-count at-count 1 next say-2)
   (say-2 isa say-count at-count 2 next say-3)
   (say-3 isa say-count at-count 3 next say-4)
   (say-4 isa say-count at-count 4 next say-1))
                 
  (goal-focus start)

  (p start
     =goal>
     at-count 0
     next =next-goal

     ?imaginal>
     state free

     ==>
     +goal> =next-goal

     +imaginal>
     count 1

     +temporal>
     ticks 0)

  (p just-count-next
     =goal>
     at-count =count
     duration =ticks

     =temporal>
     >= ticks =ticks

     ?imaginal>
     state free

     =imaginal>
     - count =count

     ==>
     !safe-bind! =next-count (1+ =count)

     =imaginal>
     count =next-count

     +temporal>
     ticks 0)

  


  (p say-count
     =goal>
     at-count =count
     duration =ticks
     next =next-goal

     ?imaginal>
     state free

     =imaginal>
     count =count

     =temporal>
     >= ticks =ticks

     ==>
     !output! (count =count)


     +temporal>
     ticks 0)



  )
|#