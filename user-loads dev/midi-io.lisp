;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;;
;;; midi-io.lisp
;;;
;;; 2026-04-24
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; constants, parameters and types
;;;
(defmacro check-type% (value type)
  "A type checking macro which returns the tested value if it is of the correct type."
  `(progn 
     (assert (typep ,value ,type) (,value) "Expected ~S, got ~S." ,type ,value)
     ,value))

;;; empty value for actr chunk slot values
(defconstant +nil+ 'none)

;;; midi numbers
(defconstant +min-midi-number+ 21)  ; F0
(defconstant +max-midi-number+ 108) ; C8
(defconstant +midi-number-type+ 
  `(integer ,+min-midi-number+ ,+max-midi-number+))
(defconstant +C4-number+ 60)
(defconstant +A4-number+ 69)
(defconstant +A4-frequency+ 440.0)


;;; midi number utilities
(defun number->tone (number)
  "Octave tone, from 1 to 12."
  (1+ (mod number 12)))

(defun number->octave (number)
  "An octave number, given a midi number value."
  (- (floor number 12) 1))

(defun number->pos-in-group (number)
  "The position of a key in a group by type (natural or accidental), given a midi number."
  (let ((tone (number->tone number)))
    (cond ((member tone '(1 2 6 7)) 1)
          ((member tone '(3 4 8 9)) 2)
          ((member tone '(5 10 11)) 3)
          ((eq tone 12)  4))))

(defun isa-natural (number)
  "Is the midi number value a natural tone."
  (member (number->tone number) 
          '(1 3 5 6 8 10 12)))

(defun count-naturals-before (number)
  "Counts the number of natural keys before a given midi number."
  (do* ((i +min-midi-number+ (1+ i))
        (isa-natural (isa-natural i)
                     (isa-natural i))
        (nb-naturals (if isa-natural 1 0)
                     (if isa-natural (1+ nb-naturals) nb-naturals)))
       ((eq i number) (1- nb-naturals))))

;;; midi frequencies
(defconstant +A4-frequency+ 440.0)

(defun number->frequency (number)
  (* +A4-frequency+ 
     (expt 2 (/ (- (check-type% number +midi-number-type+) 
                   +A4-number+) 
                12))))

;;; midi velocity and db amplitude.
;;; We assume constant amplitude following note onset, 
;;; since piano decay times usually exceed typical note and chord durations.
(defconstant +min-velocity+ 0)
(defconstant +max-velocity+ 127)
(defconstant +velocity-type+ 
  `(integer ,+min-velocity+ ,+max-velocity+))
(defconstant +piano-maximum-dB+ 100) 
(defconstant +room-dB+ 40) 

(defun velocity->midi-dB (velocity)
  "GENERAL MIDI SYSTEM LEVEL 1 DEVELOPER GUIDELINES, 1998, p.9"
  (if (> (check-type% velocity +velocity-type+) 0)
      (* 40 (log (/ velocity +max-velocity+) 10))
    :no-value))

(defun velocity->db (velocity)
  "dB Taking into account room noise level."
  (let* ((midi-dB (velocity->midi-dB velocity))
         (db (if (equal midi-dB :no-value)
                0 (+ +piano-maximum-dB+ midi-dB))))
    (if (> db +room-dB+) db 0)))


;;; act-r audio
(defconstant +time-in-ms+ t)
(defconstant +delay-time+ 50)
(defconstant +recode-time+ 285)

(defconstant +min-cyles-for-pitch-detection+ 3) ;between 2 and 4, need a ref. 
(defconstant +min-attentive-time+ 100) 

(defun pre-attentive-delay (hertz &optional (min-cycles +min-cyles-for-pitch-detection+))
  (max 50 (round (* 1000 (/ min-cycles hertz)))))

(defun attentive-delay (hertz &optional (min-cycles +min-cyles-for-pitch-detection+))
  (max (+ (pre-attentive-delay hertz min-cycles) 50)
       100))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; midi-note
;;;
(defstruct (midi-note
            (:constructor make-midi-note
             (number%)))
  (number (check-type% number% +midi-number-type+))
  (frequency (number->frequency number%))
  (amplitude 0)
  audicon-id)

(defun note-on (midi-note &optional (velocity 80))
  (with-slots (frequency amplitude audicon-id) midi-note
    (setf amplitude 
          (velocity->db velocity)
          audicon-id
          (new-ongoing-sound 
           'pitch 
           (pre-attentive-delay frequency +min-cyles-for-pitch-detection+)
           (attentive-delay frequency +min-cyles-for-pitch-detection+)
           (mp-time-ms) 'external 'tone +time-in-ms+
           `(:both frequency ,frequency)
           `(:both amplitude ,amplitude)))
    (values frequency amplitude audicon-id)))

(defun note-off (midi-note)
  (with-slots (frequency amplitude audicon-id) midi-note
    (end-ongoing-sound audicon-id)
    (setf amplitude 0
          audicon-id nil)
    (values frequency amplitude (midi-note-audicon-id midi-note))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; piano-key
;;;

;;;
;;; virtual coordinate
;;;
(defconstant +x-natural-offset+ 10)
(defconstant +x-accidental-offset+ 5)
(defconstant +y-off+ 0)
(defconstant +y-natural-near+ 10)
(defconstant +y-natural-far+ 20)
(defconstant +y-accidental-near+ 20)
(defconstant +y-accidental-far+ 30)

(defun xy-virtual (number where)
  (let ((isa-natural (isa-natural number)))
    (vector (+ (* (1+ (count-naturals-before number))
                +x-natural-offset+)
             (if isa-natural 0 +x-accidental-offset+))  
          (ecase where
            (:off +y-off+)
            (:near (if isa-natural +y-natural-near+ +y-accidental-near+))
            (:far (if isa-natural +y-natural-far+ +y-accidental-far+))))))




(defconstant +natural-colour+ 'white)
(defconstant +accidental-colour+ 'black)

(defun munber->colour (number)
  (if (isa-natural number)
      +natural-colour+
    +accidental-colour+))

;;; piano key sizes in mm
(defconstant +natural-width+      23) 
(defconstant +natural-height+    150)
(defconstant +accidental-width+   11)
(defconstant +accidental-height+  95)

(defun w (number)
  (if (isa-natural number) +natural-width+ +accidental-width+))

(defun h (number)
  (if (isa-natural number) +natural-height+ +accidental-height+))

;;; Key coordiantes as pressing touch points.
;;; Y touch oordinates are distance to the pianist. 
;;; Piano key y coordinate is the back of the key. 
;;; Near to pianist = higher y value. 
;;; Far to pianist = lower y value. 
(defconstant +far+  .5) ; 50% of the key length from the back.
(defconstant +near+ .8) ; 80% of the key length from the back.
(defconstant +y-natural-touch-far+     (* +natural-height+ +far+))
(defconstant +y-natural-touch-near+    (* +natural-height+ +near+))
(defconstant +y-accidental-touch-far+  (* +accidental-height+ +far+))
(defconstant +y-accidental-touch-near+ (* +accidental-height+ +near+))
;;; x touch coordinates (middle of key width).
(defconstant +x-natural-touch+    (/ +natural-width+ 2))
(defconstant +x-accidental-touch+ (/ +accidental-width+ 2))
;;; x coordinates of accidental keys depends on their group type
(defconstant +accidental-grp2-spacing+ (/ (* +natural-width+ 3) 5))
(defconstant +accidental-grp3-spacing+ (/ (* +natural-width+ 4) 7))
;;; Origin of piano keys layout
;;; x left most position of keys.
;;; y top or back position of all keys. 
(defparameter *piano-x* 0)
(defparameter *piano-y* 0)

(defun x-touch (number)
  (if (isa-natural number) +x-natural-touch+ +x-accidental-touch+))

(defun x-to-accidental (accidental-pos-in-group spacing)
  (- (* (ecase accidental-pos-in-group (1 1) (2 3) (3 5)) 
        spacing)
     (* (1- accidental-pos-in-group) +natural-width+)))

(defun number->key-group (number)
  "The accidental keys group, given a midi number."
  (if (< (number->tone number) 6)
      :2-accidental-keys 
    :3-accidental-keys))

(defun x-relative-to-natural-before (number)
  (if (isa-natural number) 0 
    (ecase (number->key-group number)
      (:2-accidental-keys 
       (x-to-accidental (number->pos-in-group number) 
                        +accidental-grp2-spacing+))
      (:3-accidental-keys
       (x-to-accidental (number->pos-in-group number) 
                        +accidental-grp3-spacing+)))))

(defun key-x (number)
  (+ *piano-x* ; layout x origin
     (* (count-naturals-before number) +natural-width+) ; all natural widths before current key
     (x-touch number) ; about half the key width
     (if (isa-natural number) 0 ; nothing else to add for a natural key
        (x-relative-to-natural-before number)) ; add rest to accidental key
     ))

(defun y-touch (number where)
  (ecase where
    (:near (if (isa-natural number) +y-natural-touch-near+ +y-accidental-touch-near+))
    (:far (if (isa-natural number) +y-natural-touch-far+ +y-accidental-touch-far+))))

(defun key-y (number where)
  (+ *piano-y* ; layout y origin
     (y-touch number where) ; near or far the pianist hand
     ))

;;; Notation properties
;;;
(defconstant +sharp+ (code-char #x266F))
(defconstant +flat+ (code-char #x266D))
(defconstant +natural+ (code-char #x266E))
(defun flat (str) (format nil "~A~A" str +flat+))
(defun natural (str) (format nil "~A~A" str +natural+))
(defun sharp (str) (format nil "~A~A" str +sharp+))

(defun tone->pitch-class-strings (tone)
  "Returns the pitch class names of a tone number. Optional notations include :scientific (string), :lilypond (string), or :combined (scientific: and :lilypond )."
  (case tone
    (1  `(:scientific ("C") 
          :lilypond   ("c")
          :combined   ("C")))
    (2  `(:scientific (,(sharp "C") ,(flat  "D")) 
          :lilypond   ("cis" "des")
          :combined   ("CIS" "DES")))
    (3  `(:scientific ("D") 
          :lilypond   ("d")
          :combined   ("D")))
    (4  `(:scientific (,(sharp "D") ,(flat  "E")) 
          :lilypond   ("dis" "es")
          :combined   ("DIS" "ES")))
    (5  `(:scientific ("E") 
          :lilypond   ("e")
          :combined   ("E")))
    (6  `(:scientific ("F") 
          :lilypond   ("f")
          :combined   ("F")))
    (7  `(:scientific (,(sharp "F") ,(flat  "G")) 
          :lilypond   ("fis" "ges")
          :combined   ("FIS" "GES")))
    (8  `(:scientific ("G") 
          :lilypond   ("g")
          :combined   ("G")))
    (9  `(:scientific (,(sharp "G") ,(flat  "A")) 
          :lilypond   ("gis" "as")
          :combined   ("GIS" "AS")))
    (10 `(:scientific ("A") 
          :lilypond   ("a")
          :combined   ("A")))
    (11 `(:scientific (,(sharp "A") ,(flat  "B")) 
          :lilypond   ("ais" "bes")
          :combined   ("AIS" "BES")))
    (12 `(:scientific ("B") 
          :lilypond   ("b")
          :combined   ("B")))))

(defun tone+octave->pitch-name-strings (tone octave)
  (let (pitch-strings
        (pitch-class-strings (tone->pitch-class-strings tone)))
    (dolist (notation '(:scientific :lilypond :combined) pitch-strings)
      (let ((class-strings (getf pitch-class-strings notation)))
        (setf pitch-strings
              (append (list notation 
                            (mapcar 
                             (case notation
                               (:lilypond
                                (lambda (class-string)
                                  (concatenate 'string class-string
                                               (cond ((eq octave 4) "")
                                                     ((< octave 4)
                                                      (make-string (- 4 octave) :initial-element #\,))
                                                     ((> octave 4)
                                                      (make-string (- octave 4) :initial-element #\'))))))

                               (:scientific
                                (lambda (class-string)
                                  (format nil "~A~A" class-string octave)))
                               (:combined
                                (lambda (class-string)
                                  (format nil "~A~A" class-string octave))))
                             class-strings))
                      pitch-strings))))))


;;;; visual names
(defconstant +natural-colour-prefix+ 'w)
(defconstant +accidental-colour-prefix+ 'b)
(defconstant +octave-prefix+ 'o)

(defun key-octave (number)
  (intern 
   (format nil "~S~S" 
           +octave-prefix+ (number->octave number))))

(defun key-group (number)
  (intern 
   (format nil "~S~S" 
           +accidental-colour-prefix+
           (ecase (number->key-group number)
             (:2-accidental-keys 2)
             (:3-accidental-keys 3)))))

(defun key-pos (number)
  (intern 
   (format nil "~S~S"
           (if (isa-natural number) 
               +natural-colour-prefix+ 
             +accidental-colour-prefix+)
           (number->pos-in-group number))))


;;; scalling mm to unit
;;; Assuming a coordinate system of about 2,3 mm per unit
;;; using the natural width as a reference
(defconstant +mm/unit+ 
  (/ +natural-width+ 10))

(defun scale-to-unit (mm &optional (offset 0))
  (round (/ (+ mm offset) +mm/unit+)))

(defconstant +actr-pixel-per-inch+ 72)
(defconstant +mm/inch+ 25.4)

;;; 
;;;  Piano-key
;;; 

(defstruct (piano-key (:include midi-note)
                      (:constructor make-piano-key
                       (number%
                        &aux
                        (tone% (number->tone number%))
                        (octave% (number->octave number%))
                        (pitch-strings% (tone+octave->pitch-name-strings tone% octave%)))))
  (pitch-symbols (mapcar #'intern (getf pitch-strings% :combined)))
  (xyv (xy-virtual number% :off))
  (xyv-near (xy-virtual number% :near))
  (xyv-far (xy-virtual number% :far))
  (xy-near (vector (scale-to-unit (key-x number%)) (scale-to-unit (key-y number% :near))))
  (xy-far (vector (scale-to-unit (key-x number%)) (scale-to-unit (key-y number% :far))))
  ;(w (scale-to-unit (w number%)))
  ;(h (scale-to-unit (h number%)))
  (key-octave (key-octave number%))
  (key-group (key-group number%))
  (key-pos (key-pos number%))
  (colour (munber->colour number%))
  (pitch-strings pitch-strings%) 
  (finger-over +nil+)
  visicon-id
  ;(tone tone%)
  ;(octave octave%)
  )

(defun x (vector) (svref vector 0))
(defun y (vector) (svref vector 1))

(defun finger-over (piano-key)
  (piano-key-finger-over piano-key))

(defun set-finger-over (piano-key hand-finger)
  (setf (piano-key-finger-over piano-key)
        hand-finger))

(defsetf finger-over set-finger-over)

(defun is-finger-over (piano-key)
  (with-slots (finger-over) piano-key
    (not (equal +nil+ finger-over))))

(defun visual-name (piano-key)
  (with-slots (key-octave key-group key-pos) piano-key
    (intern (format nil "~S-~S-~S" key-octave key-group key-pos))))

(defun visicon-feature (piano-key)
  (with-slots (xy-near colour key-octave key-group key-pos finger-over) piano-key ;w h 
    `(;isa (piano-key-feature piano-key)
          screen-x ,(x xy-near) 
          screen-y ,(y xy-near)
          ;width ,w
          ;height ,h
          color ,colour
          key-octave ,key-octave
          key-group ,key-group
          key-pos ,key-pos
          finger-over ,finger-over
          )))

(defun move-hf-to-key (hand-finger piano-key)
  (let ((value (setf (finger-over piano-key) hand-finger)))
    (with-slots (visicon-id) piano-key
      (modify-visicon-features
       `(,visicon-id finger-over (,value ,value))))))

(defun remove-hf-from-key (piano-key)
  (with-slots (visicon-id) piano-key
    (modify-visicon-features
     `(,visicon-id finger-over ,(setf (finger-over piano-key) +nil+)))))

(defun press-piano-key (piano-key &optional (velocity 80))
  (when (is-finger-over piano-key)
    (note-on piano-key velocity)))

(defun release-piano-key (piano-key)
  (when (is-finger-over piano-key)
    (note-off piano-key)))
              
;;;;;;;;;;;;;;;;;;;;;
;;;
;;; hands
;;;
(defconstant +hand-names+ '(left right))
(defconstant +finger-names+ '(thumb index middle ring pinkie))

(defun motor-module ()
  (get-module :motor))



; (undefine-component hf-on-keys-tracker)
;;; A component to track fingers on keys. Hand-tracker only tracks down and busy fingers.

(defstruct hf-on-keys-tracker 
  (over-keys (make-hash-table :test #'equalp)))

(defun hf-on-keys-tracker ()
  (get-component hf-on-keys-tracker))

(defun clear-hf-on-keys-tracker (hf-on-keys-tracker)
  (with-slots (over-keys) hf-on-keys-tracker
    (clrhash over-keys)))

(define-component 
 hf-on-keys-tracker
 :documentation "A component to track fingers on keys to complement Hand-tracker which tracks down and busy fingers."
 :version "1.0"
 :creation make-hf-on-keys-tracker
 :clear-all clear-hf-on-keys-tracker)

(defun hf-on-key (hand finger)
  (with-slots (over-keys) (hf-on-keys-tracker)
    (gethash (list hand finger) over-keys)))

(defun set-hf-on-key (hand finger key)
  (with-slots (over-keys) (hf-on-keys-tracker)
    (setf (gethash (list hand finger) over-keys)
          key)))

(defsetf hf-on-key set-hf-on-key)

(defun remove-hf-from-key-tracker (hand finger)
  (with-slots (over-keys) (hf-on-keys-tracker)
    (remhash (list hand finger) over-keys)))

       


  

(defun hand-tracker ()
  (extension (motor-module)))

(defun the-hand (hand-name)
  "Returns the motor module hand instance."
  (ecase hand-name
    (left (left-hand (motor-module)))
    (right (right-hand (motor-module)))))

(defun finger-down (hand finger)
  (with-slots (finger-down) (hand-tracker)
    (gethash (list hand finger) finger-down)))

(defun set-finger-down (hand finger)
  (with-slots (finger-down) (hand-tracker)
    (setf (gethash (list hand finger) finger-down) t)))




;;;;;;;;;;;;;;;;;;;;;
;;;
;;; motor styles
;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; piano
;;;
(defstruct (piano (:constructor make-piano 
                   (&aux 
                    (piano-keys% 
                     (let ((ht (make-hash-table)))
                       (do ((number +min-midi-number+ (incf number)))
                           ((> number +max-midi-number+) ht)
                         (setf (gethash number ht)
                               (make-piano-key number))))))))
  (number->piano-key piano-keys%)
  (xyv->piano-key 
   (let ((htable (make-hash-table :test #'equalp)))
     (maphash (lambda (number piano-key)
                (declare (ignore number))
                (setf (gethash (piano-key-xyv piano-key)
                               htable) 
                      piano-key))
              piano-keys%)
     htable))
  (symbol->piano-key 
   (let ((htable (make-hash-table)))
     (maphash (lambda (number piano-key)
                (declare (ignore number))
                (dolist (symbol (piano-key-pitch-symbols piano-key))
                  (setf (gethash symbol htable) piano-key)))
              piano-keys%)
     htable))
  )

(defun number->key (number piano)
  (with-slots (number->piano-key) piano
    (gethash number number->piano-key)))

(defun xyy->key (xy piano)
  (with-slots (xyv->piano-key) piano
    (gethash xy xyv->piano-key)))

(defun symbol->key (symbol piano)
  (with-slots (symbol->piano-key) piano
    (gethash symbol symbol->piano-key)))


(defun add-to-visicon (piano)
  (with-slots (number->piano-key) piano
    (maphash (lambda (number piano-key)
               (declare (ignore number))
               (setf (piano-key-visicon-id piano-key)
                     (car (add-visicon-features (visicon-feature piano-key)))))
             number->piano-key)))

(defvar *p* nil)

(defun demo2 ()
  (clear-all)
  (define-model test)
  (setf *p* (make-piano))
  (add-to-visicon *p*)
  (run-n-events 4))


(defun get-audicon ()
  (current-audicon (get-module :audio)))

;; need to find how the amplitude could be added as an audicon feature

(defun demo ()
  (let ((c4 (make-midi-note 60))
        (e4 (make-midi-note 64))
        (a0 (make-midi-note 21)))
    (clear-all)
    (define-model test-audicon
      (chunk-type note-on note)
      (chunk-type note-off note)

      (p listen-where
         ?goal>
         buffer empty

         ?aural-location>
         buffer empty

         ?aural>
         state free
         buffer empty

         ==>
         +aural-location>
         :attended nil
         )

      (p listen-attend
         ?goal>
         buffer empty

         ?aural>
         state free
         buffer empty

         ?aural-location>
         - buffer empty

         =aural-location>

         ==>
          =aural-location>
         +aural>
         event =aural-location

         )

      (p listen-what
         ?goal>
         buffer empty

         ?aural-location>
         - buffer empty

         ?aural>
         state free
         - buffer empty

         =aural>

         ==>
         -aural-location>
         -aural>
         !output! (=aural)

         )

      )

    (print-audicon)

    (note-on c4)
    (note-on e4)
    (note-on a0)
    (run 1.0)
    (print-audicon)
    (pprint (list c4 e4 a0))
#|

    (note-off e4)
    (run .5)
    (print-audicon)
    (pprint (list c4 e4))

|#
    
    ))


(define-model test
  (chunk-type goal slot)
  (define-chunks (g isa goal slot (a b)))
  (goal-focus g)
  (p test
     =goal>
     slot (a b)
     slot =slot

     ==>
     -goal>
     !output! (=slot)
     )
  )
    


;;; eof