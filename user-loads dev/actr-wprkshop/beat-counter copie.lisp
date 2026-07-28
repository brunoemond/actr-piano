
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


(clear-all)
(require-extra "threads")


(define-model foo

  (sgp :trace-detail high)

  (chunk-type counter count last-count duration)
  (chunk-type goal say-count count)

  (define-chunks start)

  (define-chunks-fct 
   `((say-count isa goal say-count t)
     (counter isa counter count start last-count ,*last-count* duration ,*ticks-per-count*)))

  (goal-focus counter)
  (goal-focus say-count)
  

  (p start-counter
     =goal>
     count start

     ?imaginal>
     state free

     ==>
     =goal>
     count 1

     +imaginal>
     count 0

     +temporal>
     ticks 0)

  (p count+1
     =goal>
     count =count
     - last-count =count
     duration =ticks
     
     ?imaginal>
     state free

     =imaginal>

     =temporal>
     >= ticks =ticks

     ==>

     !safe-bind! =next-count (1+ =count)

     =goal>
     count =next-count

     =imaginal>
     count =count

     ; !output! (count =count)
    
     ;+temporal>
    ;ticks 0
     )

  (p count-last
     =goal>
     count =count
     last-count =count
     duration =ticks
     
     ?imaginal>
     state free

     =imaginal>

     =temporal>
     >= ticks =ticks

     ==>
          
     =goal>
     count 1
   
     =imaginal>
     count =count

     ;+temporal>
    ; ticks 0
     )


  (p say-1
     =goal>
     say-count t

     ?imaginal>
     state free

     =imaginal>
     count 1

     ==>

     =imaginal>
     count 0
     
     !output! (count 1)

     +temporal>
     ticks 0

     )

  (p say-2
     =goal>
     say-count t

     ?imaginal>
     state free

     =imaginal>
     count 2

     ==>

     =imaginal>
     count 0
     
     !output! (count 2)

     +temporal>
     ticks 0
)

  (p say-3
     =goal>
     say-count t

     ?imaginal>
     state free

     =imaginal>
     count 3

     ==>

     =imaginal>
     count 0
     
     !output! (count 3)

     +temporal>
     ticks 0
)

  (p say-4
     =goal>
     say-count t

     ?imaginal>
     state free

     =imaginal>
     count 4

     ==>

     =imaginal>
     count 0
     
     !output! (count 4)

     +temporal>
     ticks 0
)

)
