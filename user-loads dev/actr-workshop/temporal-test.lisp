


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
  (round (/ (log (1+ (/ (* seconds (1- *time-mult*))
                        *time-master-start-increment*)))
            (log *time-mult*))))

(defun estimate-ticks (secs)
  (ticks-from-sec-no-noise secs))

(defparameter *bpm* 120)
(defparameter *beat-duration* (/ 60 *bpm*))

(defparameter *quater-note* *beat-duration*)
(defparameter *half-note*   (* 2 *beat-duration*))
(defparameter *whole-note*  (* 4 *beat-duration*))


(clear-all)

(define-model foo

  (sgp :record-ticks t :trace-detail low)

  (chunk-type goal next (offset 0))

  (define-chunks start end)

  (define-chunks-fct 
   `((g0 isa goal next g1  offset start)
     (g1 isa goal next g2  offset ,(estimate-ticks *quater-note*))
     (g2 isa goal next end offset 0)))

  (goal-focus g0)

  (p start
     =goal>
     offset start
     next   =next

     ==>
     +temporal>
     ticks 0

     +goal> =next)
#|
  (p wait 
     =goal>
     offset =offset
     - offset start
     next =next
     
     =temporal>
     ticks =ticks
     < ticks =offset

     ==>
     =goal>

     !output! (=ticks =offset))
|#

  (p do
     =goal>
     offset =offset
     - offset start
     next =next

     =temporal>
     ticks =ticks
     >= ticks =offset

     ==>
     +temporal>
     ticks 0

     +goal> =next)

  (p end
     =goal> 
     next end

     ==>
     -goal>

     +temporal>
     cmd clear
     )

)
