;Model Version 9.1a 
; This version implements only one main strategy: construct-chord 
; a special additional strategy is 'make-progression  for sixth-chord-chains
; will be used for constructing a knowledge base of chords and fingerings - to build on this new strategies

;Model Version 9.1
; Version 9 was based on V8.3 whereas this version is based on V8.4
; I'll try to work with specific goal-chunks now, because the solutions with workingg are too convoluted
; step by step, Bruno's visicon in included
; analyze positions will be abandoned because it has not worked well. Maybe I'll replace it later again.
; Took the MIDI connection from V8.4

;Partial-Model Version 9
; tries to marry the BC model with Bruno's visicon
; makes it during the ana-positions strategy
; vorher laden

(clear-all)

(defparameter *models-dir*
  (make-pathname :directory (pathname-directory *load-truename*)))

(unless (and (boundp '*m-loaded*) *m-loaded*)
  (load (merge-pathnames "music01.lisp" *models-dir*))
  (load (merge-pathnames "lisp-model-interface.lisp" *models-dir*))
  (load (merge-pathnames "music-vars-2.lisp" *models-dir*))
  (load (merge-pathnames "midi-connection-dummy.lisp" *models-dir*))
  ;(load (merge-pathnames "midi-connection.lisp" *models-dir*))
  ;(load "c:/home/visicon/visicon-object.lisp")
  ;(load "c:/home/visicon/piano-keyboard.lisp")


   (set-exercise *ex01*)

  (defvar ofi1 t)
  (defvar ofi2 t))

(setf *m-loaded* T)

(setf *counter* 0)
(setf *temp-diat* (harmony-context-diatones *harmony*))


(define-model BC-learning-9.1a

(sgp :cbct nil 
     :esc t 
	 :ol t 
	 :bll .5 
	 :mas 3 
	 :ga 1.0 
	 :nsji nil 
	 :ul t
     :egs 0.5	 
	 :lf .05 
	 :er t 
	 :trace-detail medium
	 :record-ticks nil
	 :auto-attend t)

;limitations: chords are only considered up to 4 parts (seventh-chords)

;(visicon-object-chunk-types piano-key)
;(visicon-object-chunk-types piano-hand)
;(visicon-object-chunk-types finger)

(chunk-type tone picl dnum cnum acc oct prev next)
(chunk-type chord name bn fig r1 r2 r3 prev)

; chords are generic, when their slots contain pitch classes, otherwise, they are specific

(chunk-type figure text)
(chunk-type key name number acc)
(chunk-type interval next number)

(chunk-type transition bnc bnp figc figp dnp bassmove discmove res1 res2)
(chunk-type workingg meth state anch mvm ls1 ls2 res1 res2 stack)
(chunk-type chordschema fig c1 c2 c3 type)

(chunk-type searchgoal type direction anchor probe offset pc1 pc2 pc3 stack)


(add-dm
 (stay isa chunk)
 (up isa chunk)
 (down isa chunk)
 (done isa chunk)
 (succ isa chunk)
 (pred isa chunk)
 (acc isa chunk)
 (nearest isa chunk)
 (nearest-up isa chunk)
 (nearest-down isa chunk)
 (step-up isa chunk)
 (step-down isa chunk)
 (leap-up isa chunk)
 (leap-down isa chunk)
 (jump-up isa chunk)
 (jump-down isa chunk)
 (no-jump isa chunk)
 (follow-bass isa chunk)
 (sharp isa chunk)
 (flat isa chunk)
 (focus isa chunk)
 (start isa chunk)
 (match isa chunk)
 (wait isa chunk)
 
 (nofig isa figure text "")
 (f-sharp-sign isa figure text "+")
 (f-flat-sign isa figure text "-")
 (f-six isa figure text "6")
 (f-six-four isa figure text "64")
 (f-second isa figure text "2")
 (f-six-five isa figure text "65")
 (f-seven isa figure text "7")
 (f-four isa figure text "4")
 (f-three isa figure text "3")
 (f-two isa figure text "2")
 
 (octave isa interval number 8)
 (seventh isa interval next octave number 7)
 (sixth isa interval next seventh number 6)
 (fifth isa interval next sixth number 5)
 (fourth isa interval next fifth number 4)
 (third isa interval next fourth number 3)
 (thirdp isa interval next fourth number 3.5)
 (thirdm isa interval next fourth number 2.5)
 (second isa interval next third number 2)
 (prime isa interval next second number 1)
 
 (cs-nofig isa chordschema fig nofig c1 third c2 fifth c3 prime type 'schema)
 (cs-four isa chordschema fig f-four c1 fourth c2 fifth c3 prime type 'schema)
 (cs-six isa chordschema fig f-six c1 third c2 sixth c3 leer type 'schema)
 (cs-six-four isa chordschema fig f-six-four c1 fourth c2 sixth c3 prime type 'schema)
 (cs-second isa chordschema fig f-two c1 second c2 fourth c3 sixth type 'schema)
 (cs-three isa chordschema fig f-three c1 third c2 fifth c3 prime type 'schema)
 (cs-six-five isa chordschema fig f-six-five c1 fifth c2 sixth c3 third type 'schema)
 (cs-seventh isa chordschema fig f-seven c1 third c2 fifth c3 seventh type 'schema)
 (cs-sharp isa chordschema fig f-sharp-sign c1 thirdp c2 fifth c3 prime type 'schema)
 (cs-flat isa chordschema fig f-flat-sign c1 thirdm c2 fifth c3 prime type 'schema)
 

; I translated the parameters of the tones to Bruno's standard:
; middle c is octave 4 and cnum 60 
 (leer isa tone picl "nix" dnum 0 oct 2 cnum -72)
 ( c2 isa tone picl "c" dnum 1 oct 2 cnum 36)
 ( c3 isa tone picl "c" dnum 1 oct 3 cnum 48 prev b2)
 ( c4 isa tone picl "c" dnum 1 oct 4 cnum 60 prev b3)
 ( c5 isa tone picl "c" dnum 1 oct 5 cnum 72 prev b4)
 ( cis2 isa tone picl "cis" dnum 1 oct 2 cnum 37 )
 ( cis3 isa tone picl "cis" dnum 1 oct 3 cnum 49 )
 ( cis4 isa tone picl "cis" dnum 1 oct 4 cnum 61 )
 ( cis5 isa tone picl "cis" dnum 1 oct 5 cnum 73 )
 ( d2 isa tone picl "d"  dnum 2 oct 2 cnum 38 prev c2)
 ( d3 isa tone picl "d"  dnum 2 oct 3 cnum 50 prev c3)
 ( d4 isa tone picl "d"  dnum 2 oct 4 cnum 62 prev c4)
 ( d5 isa tone picl "d"  dnum 2 oct 5 cnum 74 prev c5)
; ( dis2 isa tone picl "dis" dnum 2 oct 2 cnum 39 )
; ( dis3 isa tone picl "dis" dnum 2 oct 3 cnum 51 )
; ( dis4 isa tone picl "dis" dnum 2 oct 4 cnum 63 )
; ( dis5 isa tone picl "dis" dnum 2 oct 5 cnum 75 )
 ( es2 isa tone picl "es"  dnum 3 oct 2 cnum 39 )
 ( es3 isa tone picl "es"  dnum 3 oct 3 cnum 51 )
 ( es4 isa tone picl "es"  dnum 3 oct 4 cnum 63 )
 ( es5 isa tone picl "es"  dnum 3 oct 5 cnum 75 )
 ( e2 isa tone picl "e"  dnum 3 oct 2 cnum 40 prev d2)
 ( e3 isa tone picl "e"  dnum 3 oct 3 cnum 52 prev d3)
 ( e4 isa tone picl "e"  dnum 3 oct 4 cnum 64 prev d4)
 ( e5 isa tone picl "e"  dnum 3 oct 5 cnum 76 prev d5)
 ( f2 isa tone picl "f"  dnum 4 oct 2 cnum 41 prev e2)
 ( f3 isa tone picl "f"  dnum 4 oct 3 cnum 53 prev e3)
 ( f4 isa tone picl "f"  dnum 4 oct 4 cnum 65 prev e4)
 ( f5 isa tone picl "f"  dnum 4 oct 5 cnum 77 prev e5)
 ( fis2 isa tone picl "fis" dnum 4 oct 2 cnum 42 )
 ( fis3 isa tone picl "fis" dnum 4 oct 3 cnum 54 )
 ( fis4 isa tone picl "fis" dnum 4 oct 4 cnum 66 )
 ( fis5 isa tone picl "fis" dnum 4 oct 5 cnum 78 )
 ( g2 isa tone picl "g"  dnum 5 oct 2 cnum 43 prev f2)
 ( g3 isa tone picl "g"  dnum 5 oct 3 cnum 55 prev f3)
 ( g4 isa tone picl "g"  dnum 5 oct 4 cnum 67 prev f4)
 ( g5 isa tone picl "g"  dnum 5 oct 5 cnum 79 prev f5)
 ( gis2 isa tone picl "gis" dnum 5 oct 2 cnum 44 )
 ( gis3 isa tone picl "gis" dnum 5 oct 3 cnum 56 )
 ( gis4 isa tone picl "gis" dnum 5 oct 4 cnum 68 )
 ( gis5 isa tone picl "gis" dnum 5 oct 5 cnum 80 )
 ( as2 isa tone picl "as" dnum 6 oct 2 cnum 44 )
 ( as3 isa tone picl "as" dnum 6 oct 3 cnum 56 )
 ( as4 isa tone picl "as" dnum 6 oct 4 cnum 68 )
 ( as5 isa tone picl "as" dnum 6 oct 5 cnum 80 )
 ( a2 isa tone picl "a" dnum 6 oct 2 cnum 45 prev g2)
 ( a3 isa tone picl "a" dnum 6 oct 3 cnum 57 prev g3)
 ( a4 isa tone picl "a" dnum 6 oct 4 cnum 69 prev g4)
 ( a5 isa tone picl "a" dnum 6 oct 5 cnum 81 prev g5)
 ( bes2 isa tone picl "bes" dnum 7 oct 2 cnum 46 )
 ( bes3 isa tone picl "bes" dnum 7 oct 3 cnum 58 )
 ( bes4 isa tone picl "bes" dnum 7 oct 4 cnum 70 )
 ( bes5 isa tone picl "bes" dnum 7 oct 5 cnum 82 )
 ;( ais2 isa tone picl "ais" dnum 6 oct 2 cnum 46 )
 ;( ais3 isa tone picl "ais" dnum 6 oct 3 cnum 58 )
 ;( ais4 isa tone picl "ais" dnum 6 oct 4 cnum 70 )
 ;( ais5 isa tone picl "ais" dnum 6 oct 5 cnum 82 )
 ( b2 isa tone picl "b" dnum 7 oct 2 cnum 47 prev a2)
 ( b3 isa tone picl "b" dnum 7 oct 3 cnum 59 prev a3)
 ( b4 isa tone picl "b" dnum 7 oct 4 cnum 71 prev a4)
 ( b5 isa tone picl "b" dnum 7 oct 5 cnum 83 prev a5)
 
 (cmajor isa key number 0 acc nil)
 (gmajor isa key number 1 acc '(sh 4))
 (dmajor isa key number 2 acc '(sh 1 4))
 (amajor isa key number 3 acc '(sh 1 4 5))
 (fmajor isa key number -1 acc '(fl 7))
 (besmajor isa key number -2 acc '(fl 3 7))
 (esmajor isa key number -3 acc '(fl 3 6 7))

; add minor keys: - here the keys aren't used anyway
 
(start-goal isa transition dnp d5)

)

(sdp :creation-time -10000 :reference-count 100)


(p a-start-new-task
  =goal>
    isa transition
    bnc nil
	bnp nil
	dnp =dum
  ?imaginal>
    buffer empty  
 ==>
  !eval! (make-timestamp ofi1)
  !eval! (make-timestamp ofi2)
  =goal>
    bnc focus
	discmove nearest
)

; this production terminates a task properly
(p a-task-done
  =goal>
    isa transition
	bnc focus 
	res1 -1
 ==>
  =goal>
    s1 nil 
	anch nil 
	mvm nil 
	ls1 nil 
	res1 nil
	res2 nil
	stack nil
  -goal>
  +temporal>
    isa clear
)


; these productions read bass notes and figures

(p b-read-basstone
  =goal>
    isa transition
	bnc focus
	figc nil
  ?retrieval>
    state free
	buffer empty
 ==>
  !bind! =tn (read-next-tone *dietoene*)
  !bind! =cho (if (>= *counter* (length *dietoene*)) -1 (if (tone-cho (nth *counter* *dietoene*)) T 'free))  
  =goal>
    res1 =tn
	res2 =cho
  +retrieval>
    isa tone
	cnum =tn 
)

(p b-extract-basstone
  =goal>
    isa transition
	bnc focus 
  ?retrieval>
    buffer full
  =retrieval>
    isa tone
	cnum =num
	picl =nam
 ==>
  =goal>
	bnc =retrieval
	figc focus
	res1 =nam
  -retrieval>
)

; Alternative for passing notes
(p b-no-chord
  =goal>
    isa transition
	bnc =dum1
	res2 free
  ?retrieval>
    state free
    buffer empty
 ==>
  !eval! (setf *counter* (+ 1 *counter*))
  !eval! (format ofi1 "Counter: ~A" *counter*)
  !eval! (format ofi1 " play ~A~%" =dum1) 
;  !eval! (make-midi-entry-bn =dum1 ofi2)
  !eval! (make-midi-command-bn =dum1 *con*)
  =goal>
 ;   bnp =dum1    I keep the bassnote of the last chord as bnp
	bnc focus
	figc nil
	res1 nil
	res2 nil
	bassmove nil
	discmove nil
  -retrieval>	   
)

(p b-read-figure
  =goal>
    isa transition
	bnc =dum1
	figc focus
  -	res2 free
  ?retrieval>
    state free
    buffer empty
 ==>
  !bind! =fig (get-current-figure *dietoene*)
  +retrieval>
    isa figure
	text =fig
)

(p b-extract-fig
  =goal>
    isa transition
	figc focus
  ?retrieval>
    buffer full
  =retrieval>
 ==>
  =goal>
    figc =retrieval
	res1 nil
	res2 nil
  -retrieval>
)


; could be extended with productions that manage the playing of chords; e.g. keeping a chord with changing bass

; these productions analyse the bass progression (except for the first chord)
; 27.1.26 changed nearest to nearest-up
(p c-first-bassnote
  =goal>
    isa transition
	bnp nil
  - bnc nil	
  -	bnc focus
	bassmove nil
 ==>
  =goal>
    bassmove leer
	discmove nearest-up
)

;  Analysis of bass movement

(p c-leap-up
  =goal>
    isa transition
	bnp =t1
	bnc =t2
  -	bnc focus
  - figc focus
	bassmove nil
	res1 nil
  !eval! (> (- (chunk-slot-value-fct =t2 'cnum) (chunk-slot-value-fct =t1 'cnum)) 2)
  ?retrieval>
    - state busy
 ==>
  -retrieval>
  =goal>
    bassmove leap-up
	discmove focus
)	

(p c-step-up
  =goal>
    isa transition
	bnp =t1
	bnc =t2
  -	bnc focus
  - figc focus
	bassmove nil
	res1 nil
  !eval! (and (> (- (chunk-slot-value-fct =t2 'cnum) (chunk-slot-value-fct =t1 'cnum)) 0) (< (- (chunk-slot-value-fct =t2 'cnum) (chunk-slot-value-fct =t1 'cnum)) 3))
  ?retrieval>
    - state busy
 ==>
  -retrieval>
  =goal>
    bassmove step-up
	discmove focus
)

(p c-stay
  =goal>
    isa transition
	bnp =t1
	bnc =t2
	figp =f1
	figc =f2
  -	bnc focus
  - figc focus
	bassmove nil
	res1 nil
  !eval! (equal (mod (- (chunk-slot-value-fct =t2 'cnum) (chunk-slot-value-fct =t1 'cnum)) 12) 0)
  !eval! (equal =f1 =f2)
  ?retrieval>
    - state busy
 ==>
  =goal>
    res2 free
    bassmove stay
)

(p c-stay-w-changed-figure
  =goal>
    isa transition
	bnp =t1
	bnc =t2
	figp =f1
	figc =f2
  -	bnc focus
  - figc focus
	bassmove nil
	res1 nil
  !eval! (equal (mod (- (chunk-slot-value-fct =t2 'cnum) (chunk-slot-value-fct =t1 'cnum)) 12) 0)
  !eval! (not (equal =f1 =f2))
  ?retrieval>
    - state busy
 ==>
  =goal>
    discmove nearest-down
    bassmove stay
)

(p c-step-down
  =goal>
    isa transition
	bnp =t1
	bnc =t2
  -	bnc focus
  - figc focus
	bassmove nil
	res1 nil
  !eval! (and (> (- (chunk-slot-value-fct =t2 'cnum) (chunk-slot-value-fct =t1 'cnum)) -3) (< (- (chunk-slot-value-fct =t2 'cnum) (chunk-slot-value-fct =t1 'cnum)) 0))
  ?retrieval>
    - state busy
 ==>
  =goal>
    bassmove step-down
	discmove focus
)

(p c-leap-down
  =goal>
    isa transition
	bnp =t1
	bnc =t2
  -	bnc focus
  - figc focus
	bassmove nil
	res1 nil
  !eval! (< (- (chunk-slot-value-fct =t2 'cnum) (chunk-slot-value-fct =t1 'cnum)) -2)  
  ?retrieval>
    - state busy
 ==>
  =goal>
    bassmove leap-down
	discmove focus
)	

; these productions decide the progression in the discant from the bass progression
;  (at that point the analysis and realisation of sequences could be implemented later)
;  Priority is given to the analysis of proper distance between the hands -> jump?

(p d-standard-sixths
  =goal>
    isa transition
	bnc =bn
	dnp =anchor
    figc f-six
  - figp f-six	
    discmove focus
  ?retrieval>
    state free
    buffer empty
 ==>
  !bind! =min (+ 19 (chunk-slot-value-fct =bn 'cnum))
  !bind! =max (+ =min 4)
  !bind! =nam (diainterval (chunk-slot-value-fct =bn 'picl) 6 *temp-diat*)
  +retrieval>
    isa tone
	picl =nam
  >	cnum =min
  < cnum =max
  =goal>
    bassmove stay
    discmove wait
	res1 =nam
)

(p d-standard-sixths-backup  ; low utility
  =goal>
    isa transition
	bnc =bn
	dnp =anchor
    figc f-six
    discmove focus
  ?retrieval>
    state free
    buffer empty
 ==>
  !bind! =min (+ 19 (chunk-slot-value-fct =bn 'cnum))
  !bind! =max (+ =min 4)
  !bind! =nam (diainterval (chunk-slot-value-fct =bn 'picl) 6 *temp-diat*)
  +retrieval>
    isa tone
	picl =nam
  >	cnum =min
  < cnum =max
  =goal>
    bassmove stay
    discmove wait
	res1 =nam
)

(p d-standard-sixths-set-dis
  =goal>
    isa transition
	bnc =bn
	dnp =dum
    figc f-six
    discmove wait
	res1 =nam
  =retrieval>
    isa tone
	picl =nam
 ==>
  =goal>
    dnp =retrieval
    discmove nearest-down
    res1 nil
)
	
;visual-connect
(p d-jump-up
  =goal>
    isa transition
	bnc =bn
	dnp =anchor
  - figc nil
  - figc f-three
  - figc f-six
    discmove focus
  !eval! (< (- (chunk-slot-value-fct =anchor 'cnum)(chunk-slot-value-fct =bn 'cnum)) 13)
 ==>
  =goal>
    discmove jump-up
	res1 focus
    res2 'construct-chord	
)

;visual-connect
(p d-jump-down
  =goal>
    isa transition
	bnc =bn
	dnp =anchor
  - figc nil
  - figc f-three
  - figc f-six
    discmove focus
  !eval! (> (- (chunk-slot-value-fct =anchor 'cnum)(chunk-slot-value-fct =bn 'cnum)) 30)
 ==>
  =goal>
    discmove jump-down
	res1 focus
    res2 'construct-chord	
)

;visual-connect
(p d-no-jump
  =goal>
    isa transition
	bnc =bn
	dnp =anchor
  - figc nil
  - figc f-six
    discmove focus
  !eval! (<= (- (chunk-slot-value-fct =anchor 'cnum)(chunk-slot-value-fct =bn 'cnum)) 30)
  !eval! (>= (- (chunk-slot-value-fct =anchor 'cnum)(chunk-slot-value-fct =bn 'cnum)) 13)
 ==>
  =goal>
    discmove no-jump
)

; can be transferred to other situations of resolving suspensions; in any case discmove <- nearest-down
(p d-no-jump-f-three   
  =goal>
    isa transition
	bnc =bn
	dnp =anchor
	figc f-three
    discmove focus
  !eval! (or (> (- (chunk-slot-value-fct =anchor 'cnum)(chunk-slot-value-fct =bn 'cnum)) 30)
             (< (- (chunk-slot-value-fct =anchor 'cnum)(chunk-slot-value-fct =bn 'cnum)) 13))
 ==>
  =goal>
    discmove nearest-down
)

; Problem: a bass note without chord can pretend a falsobordone
; e.g. Akk1: fis 6 a d, bnsolo: d , Akk2 e 6 --> is no FB, but looks like one
; so I do no longer set the previous bassnote to a passing bassnote
 
(p d-falsobordone-up
  =goal>
    isa transition
	bnp =bn
	dnp =anchor
	figp f-six
    figc f-six
	bassmove step-up
    discmove focus
  !bind! =diff (mod (- (chunk-slot-value-fct =anchor 'cnum) (chunk-slot-value-fct =bn 'cnum)) 12)
  !eval! (and (> =diff 7) (< =diff 10))	
 ==>
  =goal>
    discmove step-up
	res1 done  ;focus
    res2 'make-progression
)

(p d-falsobordone-down
  =goal>
    isa transition
	bnp =bn
	dnp =anchor
	figp f-six
    figc f-six
	bassmove step-down
    discmove focus
  !bind! =diff (mod (- (chunk-slot-value-fct =anchor 'cnum) (chunk-slot-value-fct =bn 'cnum)) 12)
  !eval! (and (> =diff 7) (< =diff 10))	
 ==>
  =goal>
    discmove step-down
	res1 done  ;focus
    res2 'make-progression
)

; 22.1.26: I simplified all those differentiations. When the bass goes down, the discant stays or goes up,
;  when the bass goes up, the discant stays or goes down; the present discant pitch is always checked first

(p d-disc-nearest-up-step
  =goal>
    isa transition
	discmove no-jump
    bassmove step-down
 ==>
  =goal>
    discmove nearest-up
)

(p d-disc-nearest-up-leap
  =goal>
    isa transition
	discmove no-jump
    bassmove leap-down
 ==>
  =goal>
    discmove nearest-up
)

(p d-disc-nearest-down-step
  =goal>
    isa transition
	discmove no-jump
   	bassmove step-up
 ==>
  =goal>
    discmove nearest-down
)

(p d-disc-nearest-down-leap
  =goal>
    isa transition
	discmove no-jump
    bassmove leap-up
 ==>
  =goal>
    discmove nearest-down
)

(p d-disc-fb-up
  =goal>
    isa transition
    figp f-six
    figc f-six
	discmove no-jump
  	bassmove step-up
 ==>
  =goal>
    discmove step-up
)

(p d-disc-fb-down
  =goal>
    isa transition
    figp f-six
    figc f-six
	discmove no-jump
  	bassmove step-down
 ==>
  =goal>
    discmove step-down
)

(p d-disc-stay
  =goal>
    isa transition
	discmove no-jump
  	bassmove stay
 ==>
  =goal>
    discmove stay
)

;visual-connect  motor-connect

(p d-newstart
  =goal>
    isa transition
	bnc =dum1
	dnp =dum2
  - figc nil
    res1 nil
	bnp nil
 ==>
  =goal>
	res1 focus
    res2 'construct-chord	
)


(p d-start-construct-chord
  =goal>
    isa transition
	bnc =bn
	dnp =anchor
  - figc nil
    res1 nil
    bassmove =dum1
    discmove =dum2
  - discmove focus	
 ==>
  -retrieval>
  =goal>
	res1 focus     
    res2 'construct-chord
)

; as this is a version without 'anapos I restrict get-chordschema to the strategy 'construct-chord' 
(p d-get-chordschema
  =goal>
    isa transition
	figc =fig
	dnp =top
	res1 focus
    res2 'construct-chord
  ?retrieval>
    state free
    buffer empty
 ==>
  +retrieval> 
    fig =fig
    type 'schema 
)

(p d-extract-chordschema
  =goal>
    isa transition
	bnc =bn
	dnp =anchor
	figc =fig
	discmove =dm
	res1 focus
	res2 'construct-chord
  =retrieval>
    isa chordschema
	fig =fig
	c1 =c1
	c2 =c2
	c3 =c3
	type 'schema
 ==>
  +imaginal>
    isa chord
	bn =bn
	fig =fig
    r1 =c1
    r2 =c2
    r3 =c3
  =goal>
    res1 done  
)


(p d-extract-chordschema-wo-strat-cons
  =goal>
    isa transition
	bnc =bn
	dnp =anchor
	figc =fig
	discmove =dm
	res1 focus
	res2 nil
  =retrieval>
    isa chordschema
	fig =fig
	c1 =c1
	c2 =c2
	c3 =c3
	type 'schema
 ==>
  +imaginal>
    isa chord
	bn =bn
	fig =fig
    r1 =c1
    r2 =c2
    r3 =c3
  =goal>
    res2 'construct-chord  
)


(p d-push-construct-goal  
  =goal>
    isa transition
	bnc =bn
	dnp =anchor
	figc =fig
	discmove =dm
	res1 done
	res2 =strat
 ==>
  =goal> 
    res1 nil
	res2 nil
  +goal>
    isa workingg
	meth =strat
	state 0
	anch =anchor
	mvm =dm
    ls1 =bn
    ls2 =fig
	stack =goal
    res1 nil
)

; the following productions are needed for both main strategies (also 'anapos)
; they translate the interval names to pitch-classes - could be implemented visually?
; visual-connect ?

(p e-int-to-pitch-r1-1
  =goal>
    isa workingg
	meth =meth
  - meth 'make-progression
	state 0
    res1 nil
  =imaginal>
    isa chord
    r1 =int
  ?retrieval>
    state free
    buffer empty
 ==>
  =goal>
    ls1 nil
    ls2 nil
  +retrieval>
    =int
  =imaginal>	
)

(p e-int-to-pitch-r1-2
  =goal>
    isa workingg
	meth =meth
  - meth 'make-progression
	state 0
	res1 nil
  =imaginal>
    isa chord
    bn =bn
    r1 =int
  =retrieval>
    isa interval
	number =num
 ==>
  !bind! =stri (diainterval (chunk-slot-value-fct =bn 'picl) =num *temp-diat*)
  -retrieval>
  =imaginal>
   r1 =stri
  =goal>
   state 1
)

(p e-int-to-pitch-r2-1
  =goal>
    isa workingg
	meth =meth
	state 1
	res1 nil
  =imaginal>
    isa chord
    r2 =int
  ?retrieval>
    state free
    buffer empty
 ==>
  +retrieval>
    =int
  =imaginal>	
)

(p e-int-to-pitch-r2-2
  =goal>
    isa workingg
	meth =meth
	state 1
	res1 nil
  =imaginal>
    isa chord
    bn =bn
    r2 =int
  =retrieval>
    isa interval
	number =num
 ==>
  !bind! =stri (diainterval (chunk-slot-value-fct =bn 'picl) =num *temp-diat*)
  -retrieval>
  =imaginal>
   r2 =stri
  =goal>
   state 2
)

(p e-int-to-pitch-r3-1
  =goal>
    isa workingg
	meth =meth
	state 2
	res1 nil
  =imaginal>
    isa chord
  - fig f-six	
    r3 =int
  ?retrieval>
    state free
    buffer empty
 ==>
  +retrieval>
    =int
  =imaginal>	
)

(p e-int-to-pitch-r3-s
  =goal>
    isa workingg
	meth =meth
	state 2
	res1 nil
  =imaginal>
    isa chord
    fig f-six	
    r1 =r1
	r2 =r2
 ==>
  =imaginal>
    r3 leer
  =goal>
   state 'prep
  !eval! (setf *temp-diat* (replace-alterations *temp-diat* (list =r1 =r2)))
  !eval! (format t "Harmonischer Kontext lautet jetzt ~A~%" *temp-diat*)
)

(p e-int-to-pitch-r3-2
  =goal>
    isa workingg
	meth =meth
	state 2
	res1 nil
  =imaginal>
    isa chord
	bn =bn
  - fig f-six
	r1 =r1
	r2 =r2
    r3 =int
  =retrieval>
    isa interval
	number =num
 ==>
  !bind! =stri (diainterval (chunk-slot-value-fct =bn 'picl) =num *temp-diat*)
  -retrieval>
  =imaginal>
   r3 =stri
  =goal>
   state 'prep
  !eval! (setf *temp-diat* (replace-alterations *temp-diat* (list =r1 =r2 =stri)))
  !eval! (format t "Harmonischer Kontext lautet jetzt ~A~%" *temp-diat*)
)

; begin 'make-progression (makes a falsobordone progression)
;visual-connect  (for many of the following productions)


(p e-mapro-get-prevchord
  =goal>
    isa workingg
    STATE  0  
    meth  'make-progression
	anch =dnp
	ls1 =bn
	ls2 =fig
  ?imaginal>
    state free
    buffer empty
  ?retrieval>
    state free
    buffer empty
 ==>
  +imaginal>
    isa chord  
	bn =bn
	fig =fig
  +retrieval>
    isa chord
	r3 =dnp
  =goal>
   ls1 nil
   ls2 nil
)

(p e-mapro-extract-prevchord
  =goal>
    isa workingg
    STATE 0 
    meth 'make-progression
	anch =dnp
  =retrieval>
    isa chord
    r3 =dnp
    r2 =alt
	r1 =ten
  =imaginal>
 ==>
  =goal>
    state 'dis
    res1 =dnp
    ls1 =ten
	ls2 =alt
  -retrieval>
  =imaginal>
    r1 =ten
) 

(p e-mapro-do-dis-up
  =goal>
    isa workingg
    meth 'make-progression
    state 'dis
    res1 =dnp
	mvm step-up
  =imaginal>
  ?retrieval>
    state free
	buffer empty
 ==>  
  !bind! =cnum (chunk-slot-value-fct =dnp 'cnum)
  !bind! =max (+ =cnum 4)
  !bind! =nam (diainterval (chunk-slot-value-fct =dnp 'picl) 2 *temp-diat*)
  +retrieval>
    isa tone
	picl =nam
  > cnum =cnum
  < cnum =max
  =goal>
    res1 =nam
  =imaginal>	
)  

(p e-mapro-do-dis-down
  =goal>
    isa workingg
    meth 'make-progression
    state 'dis
    res1 =dnp
	mvm step-down
  =imaginal>
  ?retrieval>
    state free
	buffer empty
 ==>  
  !bind! =cnum (chunk-slot-value-fct =dnp 'cnum)
  !bind! =min (- =cnum 4)
  !bind! =nam (diainterval (chunk-slot-value-fct =dnp 'picl) -2 *temp-diat*)
  +retrieval>
    isa tone
	picl =nam
  > cnum =min
  < cnum =cnum
  =goal>
    res1 =nam
  =imaginal>	
) 

(p e-mapro-do-alt-up
  =goal>
    isa workingg
    meth 'make-progression
    state 'alt
    ls2 =anp
	mvm step-up
  =imaginal>
  ?retrieval>
    state free
	buffer empty
 ==>  
  !bind! =cnum (chunk-slot-value-fct =anp 'cnum)
  !bind! =max (+ =cnum 4)
  !bind! =nam (diainterval (chunk-slot-value-fct =anp 'picl) 2 *temp-diat*)
  +retrieval>
    isa tone
	picl =nam
  > cnum =cnum
  < cnum =max
  =goal>
    ls2 =nam
  =imaginal>	
)  

(p e-mapro-do-alt-down
  =goal>
    isa workingg
    meth 'make-progression
    state 'alt
    ls2 =anp
	mvm step-down
  =imaginal>
  ?retrieval>
    state free
	buffer empty
 ==>  
  !bind! =cnum (chunk-slot-value-fct =anp 'cnum)
  !bind! =min (- =cnum 4)
  !bind! =nam (diainterval (chunk-slot-value-fct =anp 'picl) -2 *temp-diat*)
  +retrieval>
    isa tone
	picl =nam
  > cnum =min
  < cnum =cnum
  =goal>
    ls2 =nam
  =imaginal>	
) 

(p e-mapro-get-dis-pitch
  =goal>
    isa workingg
    meth 'make-progression
    state 'dis
    res1 =nam
  =imaginal>
    isa chord
  =retrieval>
    isa tone
	picl =nam
 ==>  
  =goal>
    state 'alt
    res1 nil
  =imaginal>
    r3 =retrieval
  -retrieval>
)

(p e-mapro-get-alt-pitch
  =goal>
    isa workingg
    meth 'make-progression
    state 'alt
    ls2 =nam
  =imaginal>
    isa chord
  =retrieval>
    isa tone
	picl =nam
 ==>  
  =goal>
    state done
	meth  'construct-chord  ; preliminary jump back
    res1 nil
  =imaginal>
    r2 =retrieval
  -retrieval>
)

;;; these productions respond to the probe's picl (current discant note) being in one of the imaginal slots or not 
;visual-connect ?
  
(p e-probe-is-in-r1
  =goal>
    isa workingg
    STATE  'dis
    meth  =meth
    anch  =probe
  =imaginal>
    isa chord
    r1 =pitchc
	!eval! (stringp =pitchc)
  !eval! (equal =pitchc (chunk-slot-value-fct =probe 'picl))
 ==>
  =goal>
    state 'swap
    res1 nil
    res2 nil
  =imaginal>
    r1 =probe
 )

(p e-probe-is-in-r2
  =goal>
    isa workingg
    STATE  'dis
    meth  =meth
    anch =probe
  =imaginal>
    isa chord
    r2 =pitchc
  !eval! (stringp =pitchc)
  !eval! (equal =pitchc (chunk-slot-value-fct =probe 'picl))
 ==>
  =goal>
    state 'swap
    res1 nil
    res2 nil
  =imaginal>
    r2 =probe
)

(p e-probe-is-in-r3
  =goal>
    isa workingg
    STATE  'dis
    meth  =meth
    anch  =probe
  =imaginal>
    isa chord
    r3 =pitchc
  !eval! (stringp =pitchc)
  !eval! (equal =pitchc (chunk-slot-value-fct =probe 'picl))
 ==>
  =goal>
    state 'swap
    res1 nil
    res2 nil
  =imaginal>
    r3 =probe
)

; make sure this is ok   
(p e-probe-not-in-schema
  =goal>
    isa workingg
    state  'dis 
    meth  =meth
    anch  =probe
  ?retrieval>
  - state busy 
  =imaginal>
    isa chord
  	r1 =dum1
  	r2 =dum2
    r3 =dum3
  !eval! (stringp =dum1)
  !bind! =pitchc (chunk-slot-value-fct =probe 'picl)
  !eval! (not (or (equal =pitchc =dum1) (equal =pitchc =dum2) (equal =pitchc =dum3)))
 ==>
  =goal>
;    ls1 'dis
    state 'search
	res1 nil
  =imaginal>
)

(p e-goto-dis
  =goal>
    isa workingg
    STATE  'prep 
    meth  'construct-chord
    anch  =probe
 ==>
  =goal>
    state 'dis
)


; productions that put the anchor pitch a fifth higher/lower 
; 15.2.24 it works, but isn't elegant!
(p e-respond-jump-up
  =goal>
    isa workingg
    anch  =anchor
    mvm  jump-up
	res2 nil
  =imaginal>
  !bind! =aneu (+ (chunk-slot-value-fct =anchor 'cnum) 7)
  ?retrieval>
    buffer empty
	state free
 ==>
  +retrieval>
    isa tone
    cnum =aneu	
  =imaginal>
)

(p e-respond-jump-down
  =goal>
    isa workingg
    anch  =anchor
    mvm  jump-down
	res2 nil
  =imaginal>
  !bind! =aneu (- (chunk-slot-value-fct =anchor 'cnum) 7)
  ?retrieval>
    buffer empty
	state free
 ==>
  +retrieval>
    isa tone
    cnum =aneu	
  =imaginal>
)

(p e-continue-jump-up
  =goal>
    isa workingg
    anch  =anchor
    mvm  jump-up
	res2 nil
  =imaginal>
  !bind! =aneu (+ (chunk-slot-value-fct =anchor 'cnum) 7)
  =retrieval>
    isa tone
	cnum =aneu
 ==>
  =goal>
    anch =retrieval
	mvm nearest-down
  =imaginal>
)

(p e-continue-jump-down
  =goal>
    isa workingg
    anch  =anchor
    mvm  jump-down
	res2 nil
  =imaginal>
  !bind! =aneu (- (chunk-slot-value-fct =anchor 'cnum) 7)
  =retrieval>
    isa tone
	cnum =aneu
 ==>
  =goal>
    anch =retrieval
	mvm nearest-up
  =imaginal>
)


; visual-connect  (for many of the following productions)
; 22.1.26: I simplify the search

(p f-start-search
  =goal>
    isa workingg
    STATE 'SEARCH
    anch  =anchor
  - anch leer	
    mvm  =move
	res1 nil
	res2 nil
;  !eval! (not (or (equal =move 'step-down) (equal =move 'step-up)))	
  =imaginal>
 ==>
  +temporal>
    isa time
  +goal>
    isa searchgoal
    type 'search 
    direction =move
    anchor =anchor
    probe nil
    offset 0
    stack =goal
  =imaginal>
)

(p f-search-up-1
  =goal>
    isa searchgoal
    type 'SEARCH
    direction =move
    anchor =anchor
    probe nil
    offset 0
  !eval! (or (equal =move 'nearest-up) (equal =move 'step-up))	
  =imaginal>
 ==>
  +temporal>
    isa time
  !bind! =probe (diainterval (chunk-slot-value-fct =anchor 'picl) 2 *temp-diat*)
  =goal>
    probe =probe
    offset 1
  =imaginal>
)

(p f-search-up-2
  =goal>
    isa searchgoal
    type 'SEARCH
    direction =move
    anchor =anchor
    probe nil
    offset 1
  !eval! (or (equal =move 'nearest-up) (equal =move 'step-up))	
  =imaginal>
 ==>
  +temporal>
    isa time
  !bind! =probe (diainterval (chunk-slot-value-fct =anchor 'picl) 3 *temp-diat*)
  =goal>
    probe =probe
    offset 2
  =imaginal>
)

(p f-search-down-1
  =goal>
    isa searchgoal
    type 'SEARCH
    direction =move
    anchor =anchor
    probe nil
    offset 0
  !eval! (or (equal =move 'nearest-down) (equal =move 'step-down))	
  =imaginal>
 ==>
  +temporal>
    isa time
  !bind! =probe (diainterval (chunk-slot-value-fct =anchor 'picl) -2 *temp-diat*)
  =goal>
    probe =probe
    offset -1
  =imaginal>
)

(p f-search-down-2
  =goal>
    isa searchgoal
    type 'SEARCH
    direction =move
    anchor =anchor
    probe nil
    offset -1
  !eval! (or (equal =move 'nearest-down) (equal =move 'step-down))	
  =imaginal>
 ==>
  +temporal>
    isa time
  !bind! =probe (diainterval (chunk-slot-value-fct =anchor 'picl) -3 *temp-diat*)
  =goal>
    probe =probe
    offset -2
  =imaginal>
)

(p f-probe-is-in-r1
  =goal>
    isa searchgoal
    type  'SEARCH
    direction =move
    anchor =anchor
    probe =pc
   =imaginal>
    isa chord
    r1 =pc
 ==>
  =goal>
    probe r1
    offset match
  =imaginal>
)

(p f-probe-is-in-r2
  =goal>
    isa searchgoal
    type  'SEARCH
    direction =move
    anchor =anchor
    probe =pc
   =imaginal>
    isa chord
    r2 =pc
 ==>
  =goal>
    probe r2
    offset match
  =imaginal>
)

(p f-probe-is-in-r3
  =goal>
    isa searchgoal
    type  'SEARCH
    direction =move
    anchor =anchor
    probe =pc
   =imaginal>
    isa chord
    r3 =pc
 ==>
  =goal>
    probe r3
    offset match
  =imaginal>
)

(p f-probe-not-in-schema
  =goal>
    isa searchgoal
    type  'SEARCH
    direction =move
    anchor =anchor
    probe =pc
  - offset match  
  ?retrieval>
    state free
  =imaginal>
    isa chord
  - r1 =pc
  - r2 =pc
  - r3 =pc
 ==>
  =goal>
    probe nil
  =imaginal>	
)

;  probe nil and offset <> 0 means: continue search
; --> here I need some backup tactic when the correct pitch has not been found one third up or down.

; now the production that retrieves the correct pitch to put it into the chord in imaginal - needs to be "visualized":
  (p f-retrieve-pitch
    =goal>
      isa searchgoal
      type 'search
      anchor =anchor
      offset match
      probe =slot
    =imaginal>
      =slot =picl
    ?retrieval>
      buffer empty
      state free
    !bind! =annum (chunk-slot-value-fct =anchor 'cnum)
 ==>
    !bind! =max (+ =annum 7)
    !bind! =min (- =annum 7)
    +retrieval>
      isa tone  
      picl =picl
    < cnum =max
    > cnum =min
  =imaginal>  
  )

(p f-get-retrieved-pitch
  =goal>
    isa searchgoal
    type 'search
    anchor =anchor
    offset match
    probe =slot
    stack =oldgoal
  =imaginal>
    =slot =picl
  =retrieval>
    isa tone
    picl =picl
  =temporal>
    isa time
  < ticks 20.0  
 ==>
  -temporal>
  =goal>
   ;neutralize goal here
    anchor nil
    offset nil
    direction nil
    probe nil
    stack nil
  =imaginal>
    =slot =retrieval
  +retrieval> =oldgoal ; so quick? yes! -- but do something with the goal so that it marks the change?
)


(p f-return-to-workingg
  =goal>
    isa searchgoal
    type 'search
    anchor nil
    offset nil
  =retrieval>
    isa workingg
    state 'search
 ==> 
  =retrieval>
    state 'swap
 -retrieval>
 +goal> =retrieval
)

 
; the following productions are important: they return the correct chord inversion
(p e-swap-1
  =goal>
    isa workingg
    state 'swap	
	meth =meth
  - mvm jump-up
  =imaginal>
    isa chord
    r1 =tone
    r2 =dum2
    r3 =dum3
  - r3 leer
  !eval! (not (stringp =tone))
  !eval! (stringp =dum2)
  !eval! (stringp =dum3)
  ?retrieval>
    buffer empty
 ==>
  =imaginal> 
    r1 =dum2
	r2 =dum3
	r3 =tone
  =goal>
    state 'alt
    res1 nil
	res2 nil
)

(p e-swap-2
  =goal>
    isa workingg
    state 'swap
	meth =meth
  - mvm jump-up	
  =imaginal>
    isa chord
    r1 =dum1
    r2 =tone
    r3 =dum3
  - r3 leer
  !eval! (not (stringp =tone))
  !eval! (stringp =dum1)
  !eval! (stringp =dum3)
  ?retrieval>
    buffer empty
 ==>
  =imaginal> 
    r1 =dum3
	r2 =dum1
	r3 =tone
  =goal>
    state 'alt
    res1 nil
	res2 nil
)

(p e-swap-not
  =goal>
    isa workingg
	state 'swap
	meth =meth
  - mvm jump-up	
  =imaginal>
    isa chord
    r1 =dum1
    r2 =dum2
    r3 =tone
  - r3 leer	
  !eval! (not (stringp =tone))
  !eval! (stringp =dum1)
  !eval! (stringp =dum2)
  ?retrieval>
    buffer empty
 ==>
  =imaginal>
  =goal>
    res1 nil
	res2 nil
    state 'alt
)

(p e-swap-s-1
  =goal>
    isa workingg
    state 'swap
	meth =meth
  - mvm jump-up	
  =imaginal>
    isa chord
    fig f-six
    r1 =tone
    r2 =dum1
    r3 leer
  !eval! (not (stringp =tone))
  !eval! (stringp =dum1)
  ?retrieval>
    buffer empty
 ==>
  =imaginal>
    r1 leer
	r3 =tone
  =goal>
    state 'alt
    res1 nil
	res2 nil
)

(p e-swap-s-2
  =goal>
    isa workingg
    state 'swap
	meth =meth
  - mvm jump-up	
  =imaginal>
    isa chord
    fig f-six
    r1 =dum1
    r2 =tone
    r3 leer
  !eval! (not (stringp =tone))
  !eval! (stringp =dum1)
  ?retrieval>
    buffer empty
 ==>
  =imaginal> 
    r1 leer
	r2 =dum1
	r3 =tone
  =goal>
    state 'alt
    res1 nil
	res2 nil
)


; The following productions search the correct positions for the middle voices (pitch-class already in imaginal)
; might be done visually, names need improvement
;visual-connect ?

(p e-look-in-r2
  =goal>
    isa workingg
	meth 'construct-chord
  	state 'alt
    res1 nil
  =imaginal>
    isa chord
	r3 =anchor
	r2 =picl
  !eval! (stringp =picl)	
  ?retrieval>
    buffer empty
	state free
 ==>
  !bind! =max (chunk-slot-value-fct =anchor 'cnum) 
  !bind! =min (- =max 12)
  +retrieval>
    isa tone
    picl =picl
  < cnum =max  
  > cnum =min  
  =imaginal>
)

(p e-look-in-r3
  =goal>
    isa workingg
	meth 'construct-chord
  	state 'ten
  =imaginal>
    isa chord
	r2 =anchor
	r1 =picl
  !eval! (not (stringp =anchor))
  !eval! (stringp =picl)	
  ?retrieval>
    buffer empty
	state free
 ==>
  !bind! =max (chunk-slot-value-fct =anchor 'cnum) 
  !bind! =min (- =max 12)
  +retrieval>
    isa tone
    picl =picl
  < cnum =max  
  > cnum =min  
  =imaginal>
)

(p e-get-retrieved-alt-pitch
  =goal>
    isa workingg
    state 'alt
	meth 'construct-chord 
	res1 nil
  =retrieval>
    isa tone
  =imaginal>	
 ==>
  -retrieval>
  =imaginal>
  =goal>
    res1 =retrieval
)

(p e-get-retrieved-ten-pitch
  =goal>
    isa workingg
    state 'ten
	meth 'construct-chord 
	res1 nil
  =retrieval>
    isa tone
  =imaginal>	
 ==>
  -retrieval>
  =imaginal>
  =goal>
    res1 =retrieval
)

(p e-pitch-to-r2
  =goal>
    isa workingg
	meth 'construct-chord
  	state 'alt
    res1 =pitch
  =imaginal>
    isa chord
  - fig f-six
	r3 =anchor
	r2 =picl
  !eval! (stringp =picl)	
 ==>
  =imaginal>
    r2 =pitch
  =goal>
    state 'ten
    anch =pitch
    res1 nil
)

(p e-pitch-to-r2-s-done
  =goal>
    isa workingg
	meth 'construct-chord
  	state 'alt
    res1 =pitch
  =imaginal>
    isa chord
    fig f-six
	r3 =anchor
	r2 =picl
  !eval! (stringp =picl)	
 ==>
  =imaginal>
    r2 =pitch
  =goal>
    state done
    anch =pitch
    res1 nil
)

(p e-pitch-to-r3
  =goal>
    isa workingg
	meth 'construct-chord
  	state 'ten
    res1 =pitch
  =imaginal>
    isa chord
	r2 =anchor
	r1 =picl
  !eval! (not (stringp =anchor))
  !eval! (stringp =picl)	
 ==>
  =imaginal>
    r1 =pitch
  =goal>
    anch =pitch
    res1 nil
	state done
	meth 'construct-chord   ; preliminary jump back
)


(p e-construction-done-and-play
  =goal>
    isa workingg
    meth 'construct-chord
	state done
	stack =stack
  =imaginal>
    isa chord
    bn =bn
    r1 =r1
    r2 =r2
	r3 =r3
  ?retrieval>
    state free  
 ==>
  !eval! (format ofi1 "Counter: ~A" *counter*)
  !eval! (format ofi1 " play ~A --- ~A ~A ~A~%" =bn =r1 =r2 =r3)
  !eval! (make-midi-command =imaginal *con*)
;  !eval! (make-lily-entry =imaginal ofi2)
  =goal>
    state 'next
  +retrieval>
    =stack  
  =imaginal>
)

(p e-construction-done-and-play-s
  =goal>
    isa workingg
    meth 'construct-chord
	state done
	stack =stack
  =imaginal>
    isa chord
    fig f-six
    bn =bn
    r2 =r2
	r3 =r3
  ?retrieval>
    state free  
 ==>
  !eval! (format ofi1 "Counter: ~A" *counter*)
  !eval! (format ofi1 " play ~A --- -- ~A ~A~%" =bn =r2 =r3)
  !eval! (make-midi-command =imaginal *con*)
; !eval! (make-lily-entry =imaginal ofi2)
  =goal>
    state 'next
  +retrieval>
    =stack  
  =imaginal>
)

(p e-return-to-goalgen-new
  =goal>
    isa workingg
	state 'next
	stack =stack
  =retrieval>
    isa transition
    bnc =bn
    figc =fig
  =imaginal>
    isa chord
    bn =bn
    fig =fig
	r3 =dn
 ==>
  -imaginal>
; I neutralise the goal-chunk
  =goal>
    state nil
	meth nil
    s1 nil 
	anch nil 
	mvm nil 
	ls1 nil 
	ls2 nil
	res1 nil
	res2 nil
	stack nil
  +goal>
    isa transition
	dnp =dn
	bnp =bn
	figp =fig
	bnc focus
  -retrieval>
  !eval! (setf *temp-diat* (harmony-context-diatones *harmony*))
)
	
	
(p d-fertig
  =goal> 
    isa transition
    bnc =bn
    figc =fig
	bassmove 0
  =imaginal>
 ==>
  !output! "Liegenlass-Analyse fertig"
  =goal>
  =imaginal>
)

(spp :u 10.0)
;(spp d-bsonst :u 5.0)
;(spp d-no-jump :u 10.0)
;(spp d-try-retrieve-chord :u 10.0)

(spp d-standard-sixths-backup :u 5.0)

(spp f-get-retrieved-pitch :reward 2.0)
(spp e-construction-done-and-play :reward 10.0) 
(spp e-construction-done-and-play-s :reward 10.0)

; mal ein Versuch:
(spp b-extract-fig :reward 8.0)

;(set-buffer-chunk 'imaginal 'start-imaginal)
(goal-focus start-goal)

)


