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


(defun mod-foo1 ()
  (clear-all)
  (make-piano)
  (define-model foo
    (sgp :trace-detail high)
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

(defun foo1 ()
  (mod-foo1)
  (run 10))




;;; eof