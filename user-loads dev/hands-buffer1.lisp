;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;;
;;; hands-buffer
;;;
;;; 2026-03-28
;;;
;(provide "extended-motor-actions")

(clear-all)


(define-model hand-buf
  (install-device '("motor" "keyboard"))
  ;(hand-to-home)

  (chunk-type motor-state state)

  (define-chunks requesting preparing preparing-end executing executing-end)

  (p start
     ?goal>
     buffer empty

     ==>
     +goal>
     state requesting)

  (p motor-request
     =goal>
     state requesting

     ?manual>
     preparation free
     processor   free
     execution   free

     ==>
     =goal>
 
     +manual>              
     cmd      press-key     
     key      "Space")

  (p motor-preparing
     =goal>
     state requesting

     ?manual>
     preparation busy
     processor   busy
     execution   free

     ==>

     =goal>
     state preparing
    
     )

  (p motor-preparing-end
     =goal>
     state preparing

     ?manual>
     preparation free
     processor   busy
     execution   busy

     ==>

     =goal>
     state preparing-end
    
     )

  (p motor-executing
     =goal>
     state preparing-end

     ?manual>
     preparation free
     processor   free
     execution   busy

     ==>

     =goal>
     state executing
    
     )

  (p motor-executing-end
     =goal>
     state executing

     ?manual>
     preparation free
     processor   free
     execution   free

     ==>

     =goal>
     state executing-end
    
     )

  (p end
     =goal>
     state executing-end

     ==>

     !stop!
   
     )



  )
  
#|

  (p send-press-key-request
   ?manual>
   preparation free
   processor   free
   execution   free
   ==>
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



  )


(run 1)



|#