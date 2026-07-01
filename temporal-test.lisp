

(defparameter *time-master-start-increment* 0.011)
(defparameter *time-mult* 1.1)

(defun estimate-ticks (secs)
  (floor 
   (/ (log (1+ (/ (* secs (1- *time-mult*)) 
                  *time-master-start-increment*)))
      (log *time-mult*))))

(clear-all)

(define-model foo

  (sgp :record-ticks t :trace-detail high)

  (chunk-type goal next (offset 0))

  (define-chunks start end)

  (define-chunks-fct 
   `((g0 isa goal next g1  offset start)
     (g1 isa goal next g2  offset ,(estimate-ticks .25))
     (g2 isa goal next end offset 0)))

  (goal-focus g0)

  (p start
     =goal>
     offset start
     next   =next

     ==>
     +temporal>
     ticks t

     +goal> =next)

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
     ticks t

     +goal> =next)

  (p end
     =goal> 
     next end

     ==>
     -goal>

     +temporal>
     clear t
     )

)
