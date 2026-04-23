;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;;
;;; hands-buffer
;;;
;;; 2026-03-28
;;;
(provide "extended-motor-actions")

(clear-all)


(define-model hand-buf
  (install-device '("motor" "keyboard"))
  (define-chunks start process end)
  (chunk-type goal status)
  (define-chunks (goal1 isa goal status start))
  
  (goal-focus goal1)


  (p send-press-key-request-right
     =goal>
     isa goal
     status start

     ?manual>
     preparation free
     processor   free
     execution   free
     ==>
     =goal>
     status process 

     +manual>              
     cmd      press-key     
     key      "Space"
     )

  (p receiving
   ?manual>
   preparation busy
   processor   busy
   execution   free
   ==>

  )

  (p end-preparation-start-initiation
   ?manual>
   preparation free
   processor   busy
   execution   busy
   ==>

  )

  (p end-initiation-start-execute
   ?manual>
   preparation free
   processor   free
   execution   busy
   ==>

  )

(p end-process
   =goal>
   status process
   
   ?manual>
   preparation free
   processor   free
   execution   free
   ==>
   =goal>
   status end

  )

(p end
   =goal>
   status end
   
   ==>
   -goal>
  )



  )


#|
(run 1)
(print (buffer-read 'manual))


|#