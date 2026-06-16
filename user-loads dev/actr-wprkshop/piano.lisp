;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;;
;;; piano.lisp
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

#|
things to do:
refplace references with numbers
set finger locations
determine positions of unnamed fingers with a parameter to keep unnamed in place? (keep hand in place? 
using visual locations as references, 
sort of relative references taking into account dependenci chains. 
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type utility
;;;
(defun typep->value (value type &optional message)
  "Type check VALUE against TYPE, returning VALUE if successful.
   Optionally includes MESSAGE in the error output."
  (assert (typep value type) (value)
    "Got ~S of type ~S, expected type ~S~@[~%  ~A~]."
    value (type-of value) type message)
  value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; midi-number, semitone & octave
;;; 
(defparameter *min-midi-number* 21
  "21 is A0 on a 88 keys keyboard.")
(defparameter *max-midi-number* 108
  "108 is C8 on a 88 keys keyboard.")

(deftype midi-number () 
  "A range of integers between *min-midi-number* and *max-midi-number*"
  `(integer ,*min-midi-number* ,*max-midi-number*))

;; useful constants
(defconstant +C4-number+ 60)
(defconstant +A4-number+ 69)

(deftype semitone () 
  "A semitone value is between 1 and 12."
  '(integer 1 12))

(defun number->semitone (number)
  "Returns a semitone from a MIDI NUMBER."
  (1+ (mod (typep->value number 'midi-number) 12)))

(defun number->octave-number (number)
  "Returns an octave number between 0 and 8 on a 88 keys keyboard given a MIDI NUMBER."
  (- (floor (typep->value number 'midi-number) 12) 1))

(defun number->octave-symbol (number)
  "A symbol for an octave given a MIDI NUMBER. Used as a chunk name."
  (intern (format nil "O~S" (number->octave-number number))))

(defun all-octaves ()
  (let (octaves)
    (do ((i *min-midi-number* (1+ i)))
        ((> i *max-midi-number*) octaves)
      (setf octaves 
            (adjoin (number->octave-symbol i)
                    octaves)))))

(defun octave-chunks ()
  (chunk-type octave left-of right-of)
  (do* ((octaves (all-octaves) (cdr octaves))
        (chunk `(,(nth 0 octaves) isa octave right-of ,(nth 1 octaves) left-of nil)
               `(,(nth 0 octaves) isa octave right-of ,(nth 1 octaves) left-of ,previous-n))
        (previous-n (nth 0 octaves) (nth 0 octaves))
        (chunks (list chunk) (append chunks (list chunk))))
       ((null (cdr octaves)) (define-chunks-fct chunks))))

(deftype octave () 
  "A symbol for an octave between O0 and O8 for an 88 keys keyboard."
  `(member ,@(all-octaves)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; midi audio
;;; velocity & midi-hertz
;;; velocity -> dB
;;;
(defconstant +A4-frequency+ 440.0)
(defparameter *piano-maximum-dB* 100) 
(defparameter *room-dB* 40) 

(defconstant +min-velocity+ 0)
(defconstant +max-velocity+ 127)
(deftype velocity () 
  "A velocity value between +min-velocity+ and +max-velocity+."
  `(integer ,+min-velocity+ ,+max-velocity+))

(defun number->frequency (number)
  "Returns a frequecy in Hertz from a MIDI NUMBER, according to the midi Tuning Standard."
  (* +A4-frequency+ 
     (expt 2 (/ (- (typep->value number 'midi-number) +A4-number+) 12))))

(deftype midi-hertz ()
  "Frequency values betwwen the lowest and highest midi numnber values."
  `(real ,(number->frequency *min-midi-number*) 
         ,(number->frequency *max-midi-number*)))

(defun velocity->midi-dB (velocity)
  "Returns the Decibels for a given VELOCITY value. 
From: GENERAL MIDI SYSTEM LEVEL 1 DEVELOPER GUIDELINES, 1998, p.9"
  (if (> (typep->value velocity 'velocity) 0)
      (* 40 (log (/ velocity +max-velocity+) 10))
    0))

(defun velocity->db (velocity)
  "Returns the Decibels for a given VELOCITY value, taking into account room noise level."
  (let* ((midi-dB (velocity->midi-dB velocity))
         (db (if (equal midi-dB :no-value)
                0 (+ *piano-maximum-dB* midi-dB))))
    (if (> db *room-dB*) db 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; music note
;;; Music notations for midi-nimners
;;;
(defconstant +sharp+ (code-char #x266F))
(defconstant +flat+ (code-char #x266D))
(defconstant +natural+ (code-char #x266E))
(defun sharp (str) (format nil "~A~A" str +sharp+))
(defun flat (str) (format nil "~A~A" str +flat+))
(defun natural (str) (format nil "~A~A" str +natural+))

(defun semitone->note-class-strings (semitone)
  "Returns the note class names of a SEMITONE number. 
Optional notations include :scientific (string), :lilypond (string), 
or :combined (scientific: and :lilypond for sharps and flats)."
  (case semitone
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
          :lilypond   ("dis" "ees")
          :combined   ("DIS" "EES")))
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
          :lilypond   ("gis" "aes")
          :combined   ("GIS" "AES")))
    (10 `(:scientific ("A") 
          :lilypond   ("a")
          :combined   ("A")))
    (11 `(:scientific (,(sharp "A") ,(flat  "B")) 
          :lilypond   ("ais" "bes")
          :combined   ("AIS" "BES")))
    (12 `(:scientific ("B") 
          :lilypond   ("b")
          :combined   ("B")))))

(defun insert-octave (note-class-string octave)
  "Inserts an OCTAVE number in a NOTE-CLASS-STRING. Ex C -> C4; AES -> A3ES."
  (if (eq 1 (length note-class-string))
      (format nil "~A~A" note-class-string octave)
    (format nil "~A~A~A" 
            (subseq note-class-string 0 1) 
            octave 
            (subseq note-class-string 1))))
  
(defun semitone+octave->note-strings (semitone octave)
  "Returns a note string given a SEMITONE and OCTAVE number."
  (let (note-strings
        (note-class-strings (semitone->note-class-strings semitone)))
    (dolist (notation '(:scientific :lilypond :combined) note-strings)
      (let ((class-strings (getf note-class-strings notation)))
        (setf note-strings
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
                                  (insert-octave class-string octave)))
                               (:combined
                                (lambda (class-string)
                                  (insert-octave class-string octave))))
                             class-strings))
                      note-strings))))))

(defun number->note-class-strings (number format)
  "Returns a note class string in a given FORMAT for a MIDI NUMBER."
  (getf (semitone->note-class-strings (number->semitone number))
        (ecase format (:scientific :scientific) (:lilypond :lilypond) (:combined :combined))))

(defun number->note-class-symbols (number)
  "Returns a list of note class symbols given a MIDI number."
  (mapcar #'intern (number->note-class-strings number :combined)))
         
(defun number->note-strings (number format)
  "Returns a list of note string in a FORMAT given a MIDI NUMBER"
  (getf (semitone+octave->note-strings (number->semitone number) (number->octave-number number))
        (ecase format (:scientific :scientific) (:lilypond :lilypond) (:combined :combined))))

(defun number->note-symbols (number)
  "Returns a list of note symbols given a MIDI number."
  (mapcar #'intern (number->note-strings number :combined)))

(defun notes-hashtable ()
  "Makes a hash table of every notes, in every format for every mini numbers 
from *min-midi-number* to *max-midi-number*."
  (let ((ht (make-hash-table :test #'equalp)))
    (do ((i *min-midi-number* (1+ i)))
        ((> i *max-midi-number*) ht)
      (dolist (note-name 
               (append (number->note-strings i :scientific)
                       (number->note-strings i :lilypond)
                       (number->note-symbols i)))
        (setf (gethash note-name ht) i)))))

(defconstant +notes-hashtable+ (notes-hashtable))

(defun isa-note-name (note-name)
  (gethash note-name +notes-hashtable+))

(deftype note-name () 
  "A note name is one that is present in +notes-hashtable+."
  '(satisfies isa-note-name))

(defun all-note-names ()
  (let (names)
    (maphash (lambda (name key)
               (declare (ignore key))
               (when (symbolp name)
                 (push name names)))
             +notes-hashtable+)
    names))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; piano keys visual pattern
;;;
;;; Visual patterns are defined on the basis of key groups
;;; with 2 or 3 black keys. The pattern includes the key color
;;; and its position in the group. 
;;; An octave id is added to the visual class pattern to uniquely
;;; identify a note (midi number). 
;;;
(deftype key-color () '(member white black))

(deftype visual-group-pattern () 
  "Group key names: black2 (2 blacks + 3 whites) or black3 (3 blacks + 4 whites)."
  '(member black2 black3))

(deftype visual-group-pattern-position () 
  "Key relative positions are: left, right middle, midleft and midright. 
The last two for white keys in black3."
  '(member left middle right midleft midright))

(defun number->key-color (number)
  "Returns a color from a MIDI NUMBER."
  (if (member (number->semitone number) 
              '(2 4 7 9 11))
      'black 'white))

(defun number->visual-group-pattern (number)
  "Returns the group keys either 2 blacks or 3 blacks of a MIDI NUMBER."
  (if (< (number->semitone number) 6) 
      'black2 'black3))

(defun number->visual-group-pattern-position (number)
  "Returns the group position in a visual-group-pattern given a MIDI NUMBER."
  (case (number->semitone number)
    (1 'left)  (2 'left)  (3 'middle)  
    (4 'right) (5 'right)
    (6 'left) (7 'left) 
    (8 'midleft) (9 'middle) (10 'midright) 
    (11 'right) (12 'right)))

(defstruct (key-visual-pattern 
            (:constructor make-vis-pattern)
            (:type list) 
            (:conc-name key-visual-))
  "A structure as a list, for a key visual pattern including octave, group type, key color, 
and key relative position in the group."
  octave group color position)

(defun number->visual-pattern (number)
  (make-vis-pattern 
   :octave   (number->octave-symbol        number)
   :group    (number->visual-group-pattern number)
   :color    (number->key-color            number)
   :position (number->visual-group-pattern-position number)))

(defun isa-key-visual-pattern (object)
  (when (and (listp object) (equal 4 (length object)))
    (destructuring-bind (octave group color position) object
      (and (typep octave   'octave)
           (typep group    'visual-group-pattern)
           (typep color    'key-color)
           (typep position 'visual-group-pattern-position)))))
       
(deftype visual-pattern () '(satisfies isa-key-visual-pattern))                      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; xy coordinates as midi number names
;;; 
(deftype xy ()
  "A vector of 2 integers."
  '(vector integer 2))

(deftype list2 () 
  "A list of 2 integers."
  '(cons integer (cons integer null)))

(defun ->xy (object)
  (etypecase object
    (xy object)
    (list2 (apply #'vector object))))

(defun x (object)
  (aref (->xy object) 0))

(defun y (object)
  (aref (->xy object) 1))

(defparameter *piano-visual-xy-origin* #(0 0)
  "The pixels origin for the piano keyboard. 
Can be set in relationship to other visual elements such as a music sheet.")

;;; piano key sizes in mm
;;; the keys height is not considered. 
;;; However, for actr, the depth is the height. 
(defparameter *visual-white-width*   23) 
(defparameter *visual-white-depth*  150)
(defparameter *visual-black-width*   11)
(defparameter *visual-black-depth*   95)
(defconstant +mm/inch+ 25.4)
;;; actr parameters
(defparameter *default-pixel-per-inch* 72
  "Need to set (sgp :pixels-per-inch 72), if *default-pixel-per-inch* is different.")

(defun mm->pixels (mm)
  "Convert milimiters to pixels."
  (round (* *default-pixel-per-inch* 
            (/ mm +mm/inch+))))

;;; pixel values
(defparameter *visual-white-width-pixel* (mm->pixels *visual-white-width*))
(defparameter *visual-black-width-pixel* (mm->pixels *visual-black-width*))
(defparameter *visual-white-depth-pixel* (mm->pixels *visual-white-depth*))
(defparameter *visual-black-depth-pixel* (mm->pixels *visual-black-depth*))
;;; only one y coordinate associated to the near location at .8 like the tactile offset
(defparameter *visual-y-offset* .8)

;;; Black keys are assumed to be located in-between white keys. 
;;; This is an approximation which is not what an actual physical layout of a piano is.
;;;

(defun half-white-width ()
  (round (/ *visual-white-width-pixel* 2)))

(defun count-keys-of-color (start end% color%)
  "Counts the number of keys in a given color between start and end."
  (let ((end (typep->value end% 'midi-number))
        (color (typep->value color% 'key-color)))
    (do* ((i (typep->value start 'midi-number) (1+ i))
          (nb-keys (if (eq color (number->key-color i))
                       1 0)
                   (if (eq color (number->key-color i))
                       (1+ nb-keys) nb-keys)))
         ((eq i end) nb-keys))))

(defun nb-whites-before (number)
  "The number of white keys to the left of a given mini number."
  (1- (count-keys-of-color *min-midi-number* number 'white)))

(defun initial-x ()
  "The initial x coordinate is the x-origin + half the white key width."
  (+ (x *piano-visual-xy-origin*) (half-white-width)))

(defun x-white (number)
  "The x coordinate of a white key is the initial x value + the number of white widths before the key."
  (+ (initial-x) 
     (* (nb-whites-before number) *visual-white-width-pixel*)))

(defun x-black (number)
  "The x coordinate of an black key is coordinate of the previous white + half white width."
  (+ (x-white number) (half-white-width)))

(defun y-with-depth-offset (key-depth)
  "The y origin + an offset on the key depth."
  (+ (y *piano-visual-xy-origin*) 
     (round (* *visual-y-offset* key-depth))))
              
(defun number->xy (number)
  "Returns the xy vector position given a mini number."
  (let ((color (number->key-color number)))
    (case color
      (white (vector (x-white number) 
                     (y-with-depth-offset *visual-white-depth-pixel*)))
      (black (vector (x-black number) 
                     (y-with-depth-offset *visual-black-depth-pixel*))))))

(defun number->wh (number)
  "Returns the width and height vector given a mini number."
  (let ((color (number->key-color number)))
    (case color
      (white (vector *visual-white-width-pixel* 
                     *visual-white-depth-pixel*))
      (black (vector *visual-black-width-pixel* 
                     *visual-black-depth-pixel*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  hands and fingers
;;;
(defconstant +actr-nil+ 'none "Instead of nil values for chunk slot values.")
(defconstant +hand-names+ '(right left))
(defconstant +finger-names+ '(thumb index middle ring pinkie))

(deftype hand-name   () `(member ,@+hand-names+))
(deftype finger-name () `(member ,@+finger-names+))

(defstruct (hand-finger 
            (:constructor make-hf)
            (:type list)
            (:conc-name hf-))
  hand finger)

(defun isa-hand-finger (object)
  (destructuring-bind (hand finger) object
    (typep hand   'hand-name)
    (typep finger 'finger-name)))

(deftype hand-finger () '(satisfies isa-hand-finger))

(defun ->hand-finger (hand finger)
  (make-hf :hand   (typep->value hand   'hand-name)
           :finger (typep->value finger 'finger-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  relative reference
;;;
;;; Reference to keys under fingers require absolute reference to keys on the keyboard. 
;;; However, when refering to finger positions on keys, sometimes the reference is
;;; relative to an anchor key position.  
;;; The current implementation supports two relative references: key color references and 
;;; semitone references. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; relative reference by color
;;;
(deftype color-distance () 
  "Number of keys of the same color for counting interval range [1..8]. 
Aassuming 1 being the starting position to count."
  '(integer 1 8))

(deftype direction () '(member up down))

(defstruct (reference-by-color 
            (:constructor make-color-ref)
            (:type list)
            (:conc-name color-ref-))
  color distance direction from-finger)

(defun isa-color-reference (object)
  (destructuring-bind (color color-distance direction from-finger) object
    (and 
     (typep color          'key-color)
     (typep color-distance 'color-distance)
     (typep direction      'direction)
     (typep from-finger    'finger-name))))

(deftype color-reference () 
  '(satisfies isa-color-reference))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; relative reference by semitone (quality and quantity)
;;;
(deftype quality () 
  "Interval qualities: perfect, minor, major, diminished, augmented."
  `(member perf min maj dim aug))

(deftype quantity () 
  "Tone quantity, one besing the self tone."
  '(integer 1 8))

(defstruct (reference-by-semitone 
            (:constructor make-semitone-ref)
            (:type list)
            (:conc-name semitone-ref-))
  quality quantity direction from-finger)

(defun isa-semitone-reference (object)
  (destructuring-bind (quality quantity direction from-finger) object
    (and 
     (typep quality     'quality)
     (typep quantity    'quantity)
     (typep direction   'direction)
     (typep from-finger 'finger-name))))

(deftype semitone-reference () 
  "A semitone-reference has 4 elements (quality quantity direction from-finger)."
  '(satisfies isa-semitone-reference))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; sorting absolute and relative references
;;; sorting based on the number of reference jumps required to resolve all references. 
;;;

(deftype absolute-reference ()
  '(or midi-number xy visual-pattern note-name))
(deftype relative-reference ()
  '(or color-reference semitone-reference))

(defstruct (finger-reference 
            (:constructor finger-ref)
            (:type list)
            (:conc-name figref-))
  finger reference)

(defun find-figref (finger list)
  (let ((found (find finger list :key #'figref-finger)))
    (if found found
      (error "Finger ~S is not in the list ~S." finger list))))

(defun relative-from-finger (reference)
  (etypecase reference
    (color-reference    (color-ref-from-finger reference))
    (semitone-reference (semitone-ref-from-finger reference))))

(defun count-ref-jumps (finger-reference list)
  (let ((reference (figref-reference finger-reference)))
    (etypecase reference
      (absolute-reference 0)
      (relative-reference 
       (+ 1 (count-ref-jumps 
             (find-figref (relative-from-finger reference) list)
             list))))))

(defun sort-finger-references (list)
  (let ((sorted-list (copy-seq list)))
    (sort sorted-list #'< :key (lambda (x) (count-ref-jumps x list)))))
         
; (sort-finger-reference '((pinkie (white 3 up middle)) (thumb 60) (middle (white 3 up thumb)) ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; piano-key
;;; 
(defstruct (piano-key           
            (:constructor make-piano-key (number%))
            (:conc-name key-))
  "The basic structure for a piano key with midi number and audio frequency."
  (number      number%)
  ;; audtio properties
  (frequency   (number->frequency number%))
  audicon-id
  ;; visual properties
  (xy          (number->xy number%))
  (wh          (number->wh number%))
  (octave      (number->octave-symbol number%))
  (color       (number->key-color number%))
  (visual-grp  (number->visual-group-pattern number%))
  (group-pos   (number->visual-group-pattern-position number%))
  (finger-over +actr-nil+)
  visicon-id
  ;; notation properties
  (note-name   (number->note-symbols number%))
  (note-lily   (number->note-strings number% :lilypond))
  (note-sci    (number->note-strings number% :scientific))
  ;; key state
  (is-down nil)
  )

(defun make-keys ()
  "Generates a hash table of piano keys."
  (let ((ht  (make-hash-table :test #'equalp)))
    (do ((i *min-midi-number* (1+ i)))
        ((> i *max-midi-number*) ht)
      (setf (gethash i ht) (make-piano-key i)))))

(defun visual-key-features (piano-key)
  (with-slots (xy wh octave color visual-grp group-pos finger-over) piano-key
    `(;; visicon features
      isa (piano-key-features piano-key)
      screen-x    ,(x xy) 
      screen-y    ,(y xy)
      width       ,(x wh)
      height      ,(y wh)
      octave      ,octave
      color       ,color
      visual-grp  ,visual-grp
      group-pos   ,group-pos
      finger-over ,finger-over
      )))
   
(defun piano-key-to-visicon (piano-key)
  (setf (key-visicon-id piano-key)
        (car (add-visicon-features (visual-key-features piano-key)))))

(defun modify-piano-key-finger-over (piano-key hand-finger)
  (let ((hf (typep->value hand-finger 'hand-finger)))
    (with-slots (visicon-id) piano-key
      (modify-visicon-features `(,visicon-id finger-over (,hf ,hf))))))

(defun visual-pattern (piano-key)
  (with-slots (octave visual-grp color group-pos) piano-key
    (typep->value (list octave visual-grp color group-pos)
                  'visual-pattern)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; act-r piano-component
;;; 
;;; set-hand-device get-hand-device?

(defparameter *piano-component-name* 'piano)
(defparameter *piano-device-name* "piano")
(defparameter *piano-version* "1.0")
(defparameter *piano-documentation* "A piano compoment/device for actr models.")

(defun component-p (name)
  (assoc (typep->value name 'symbol)
         (meta-p-component-list (current-mp))))

(defun act-r-component (name)
  (cdr (component-p name)))

(defun component-instance (name)
  (get-component-fct (typep->value name 'symbol)))

(defun device-p (name)
  (member (typep->value name 'string) 
          (defined-devices) :test #'equalp))

(defun make-component-device ()
  (when (component-p *piano-component-name*) 
    (undefine-component-fct *piano-component-name*))
  (define-component-fct
   *piano-component-name* 
   :version *piano-version* 
   :documentation *piano-documentation*)
  (unless (device-p *piano-device-name*)
    (define-device *piano-device-name*))
  (act-r-component *piano-component-name*))

(defun piano ()
  "Returns the piano component installed for the motor interface."
  (let ((piano (component-instance *piano-component-name*)))
    (if piano
        (if (member *piano-device-name* (current-devices "motor") 
                    :test #'equalp :key #'second)
            piano
          (error "Device ~S is not installed for the ~S interface." *piano-device-name* "motor"))
      (error "Component ~S is not defined." *piano-component-name*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; piano
;;; 
(defstruct (piano 
            (:constructor make-piano%)
            (:conc-name piano-))
  (number->key (make-keys))
  xy->key
  visual->key
  note->key)

(defun index-hash-table (source-ht index-fct &optional (index-ht (make-hash-table :test #'equalp)))
  (let ((ht (typep->value source-ht 'hash-table))
        (fct (typep->value index-fct 'function))
        (indx (typep->value index-ht 'hash-table)))
    (maphash (lambda (key value)
               (declare (ignore key))
               (let ((index-key-value (funcall fct value)))
                 (etypecase index-key-value
                   ; simple-vector, strings, and symbols are atom
                   (atom           (setf (gethash index-key-value indx) value))
                   (visual-pattern (setf (gethash index-key-value indx) value))
                   ; Note names are in lists
                   (list           (dolist (i index-key-value)
                                     (setf (gethash i indx) value))))))
             ht)
    indx))

(defun make-piano ()
  "Function to call to make a piano component before defining a model."
  (let ((component-device (make-component-device)))
    (when component-device
      (let ((piano (make-piano%)))
        (setf (act-r-component-instance component-device) piano)
        (with-slots (number->key xy->key visual->key note->key) piano
          (setf xy->key     (index-hash-table number->key #'key-xy)
                visual->key (index-hash-table number->key #'visual-pattern)
                note->key   (index-hash-table number->key #'key-note-name)
                note->key   (index-hash-table number->key #'key-note-lily note->key)
                note->key   (index-hash-table number->key #'key-note-sci note->key)))
        piano))))

(defun make-piano-visible ()
  "To call early in the model definition to make all piano keys in the visicon."
  (chunk-type (piano-key-features (:include visual-location)) octave visual-grp group-pos finger-over)
  (chunk-type (piano-key (:include visual-object)) octave visual-grp group-pos finger-over)
  (octave-chunks)
  (define-chunks-fct '(piano-key none black2 black3 midleft midright))
  (maphash (lambda (number piano-key)
             (declare (ignore number))
             (piano-key-to-visicon piano-key))
           (piano-number->key (piano))))
                                
(defun number->key (number)
  (gethash (typep->value number 'midi-number)
           (piano-number->key (piano))))

(defun xy->key (xy)
  (gethash (typep->value xy 'xy)
           (piano-xy->key (piano))))

(defun note->key (note)
  (gethash (typep->value note 'note-name)
           (piano-note->key (piano))))

(defun visual->key (visual-pattern)
  (gethash (typep->value visual-pattern 'visual-pattern)
           (piano-visual->key (piano))))

;; to do visual-location->key
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; piano-hand-style
;;; generic class, not indended to be instantiated.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass piano-hand-style (dual-hand-movement-style)
  ((hand :initarg :hand :accessor hand :initform nil
         :documentation "A hand name: right or left.")
   (adjust-hand :accessor adjust-hand :initform t
                :documentation "If non-nil, hand-pos-loc is computed based on hand finger positions, otherwise it is left unchanged." )
   (thumb :initarg :thumb :accessor thumb :initform nil)
   (index :initarg :index :accessor index :initform nil)
   (middle :initarg :middle :accessor middle :initform nil)
   (ring :initarg :ring :accessor ring :initform nil)
   (pinkie :initarg :pinkie :accessor pinkie :initform nil)
   (finger-down-state :initform nil :initarg :finger-down-state
                      :accessor finger-down-state
                      :documentation "The finger down state applied to style fingers."))
  (:default-initargs
   :two-exec-p nil
   :finger-based-style t
   :release-if-down nil
   :feature-slots nil
   :style-name 'piano-hand-style
   :fprep-time 0.05
   :exec-time 0.05
   :exec2-time 0
   :finish-time 0.05
   ))

;;;
;;; motor module utilities
;;;
(defun motor-module ()
  (get-module :motor))

(defun the-hand (hand-name)
  (ecase hand-name
    (right (right-hand (motor-module)))
    (left  (left-hand (motor-module)))))

(defun motor-extension ()
  (extension (motor-module)))

(defun fingers-down ()
  (hand-tracker-finger-down (motor-extension)))

(defun finger-loc->key (hand finger)
  "Returns the key associated to the current motor module hand finger location."
  (gethash (finger-loc (the-hand hand) finger :current t) 
           (piano-xy->key (piano))))

(defun hand-pos->fingers (hand)
  (hand-pos-fingers (loc (the-hand hand))))

;;;
;;; motor request utilities
;;;
(defun request-slots (act-r-chunk-spec)
  (act-r-chunk-spec-slots act-r-chunk-spec))

(defun request-slot (act-r-chunk-spec slot-name)
  (find slot-name (request-slots act-r-chunk-spec) 
        :key #'act-r-slot-spec-name))

(defmethod request-slot-names ((act-r-chunk-spec act-r-chunk-spec))
  (mapcar #'act-r-slot-spec-name 
          (request-slots act-r-chunk-spec)))

(defmethod request-slot-names ((style piano-hand-style))
  (request-slot-names (request-spec style)))

(defmethod request-slot-value ((act-r-chunk-spec act-r-chunk-spec) slot-name)
  (let ((slot (request-slot act-r-chunk-spec slot-name)))
    (if slot (act-r-slot-spec-value slot)
      (error "Slot name ~S is not one of ~S." 
             slot-name (request-slot-names act-r-chunk-spec)))))

(defmethod request-slot-value ((style piano-hand-style) slot-name)
  (let ((spec (request-spec style)))
    (when spec (request-slot-value spec slot-name))))

(defun make-dummy-slot (slot-spec)
  "Used for testing."
  (destructuring-bind (name &optional value) slot-spec
    (make-act-r-slot-spec :name name :value value)))
    
(defun make-dummy-request (slot-specs)
  "Used for testing."
  (let (actr-slots)
    (dolist (slot-spec slot-specs
                       (make-act-r-chunk-spec :slots actr-slots))
      (push-end (make-dummy-slot slot-spec) actr-slots))))

;;;
;;; piano-hand-style utilities
;;;
(defun collect-finger-values (piano-hand-style &key (not-null t))
  "Collects finger values, default to only non-null values."
  (let (collection)
    (dolist (finger +finger-names+ collection)
      (let ((value (slot-value piano-hand-style finger)))
        (cond ((and not-null value)
               (push (list finger value) collection))
              ((and not-null (null value))
               'ignore)
              ((null value)
               (push (list finger value) collection)))))))

(defun fingers-in-request (style)
  (let ((slot-names (request-slot-names style))
        fingers)
    (dolist (slot-name slot-names fingers)
      (when (member slot-name +finger-names+)
        (push-end slot-name fingers)))))

;;;
;;; resolving finger references in motor requests
;;; 
(defun absolute-reference->key (reference)
  (etypecase reference
    (midi-number    (number->key reference))
    (xy             (xy->key reference))
    (visual-pattern (visual->key reference))
    (note-name      (note->key reference))))

(defun color-reference->key (reference style)
  (destructuring-bind (color color-distance direction from-finger) reference
    (do* ((i (key-number (slot-value from-finger style))
             (ecase direction (up (1+ i)) (down (1- i))))
          (nb-color (if (equal color (number->key-color i))
                        1 0)
                    (if (equal color (number->key-color i))
                        (1+ nb-color) nb-color)))
         ((eq nb-color color-distance) 
          (number->key (typep->value i 'midi-number))))))

(defconstant +interval-semitone-table+
  (let ((table (make-hash-table :test 'equal)))
    (dolist (entry '(;; (quantity quality semitones)
                     (1 perf 0) (1 aug   1)
                     (2 dim  0) (2 min   1) (2 maj 2)  (2 aug 3)
                     (3 dim  2) (3 min   3) (3 maj 4)  (3 aug 5)
                     (4 dim  4) (4 perf  5) (4 aug 6)
                     (5 dim  6) (5 perf  7) (5 aug 8)
                     (6 dim  7) (6 min   8) (6 maj 9)  (6 aug 10)
                     (7 dim  9) (7 min  10) (7 maj 11) (7 aug 12)
                     (8 dim 11) (8 perf 12) (8 aug 13))
                   table)
      (destructuring-bind (quantity quality semitones) entry
        (setf (gethash `(,quantity ,quality) table) semitones))))
  "Maps (quantity quality) to semitone count for simple intervals [unison..octave].")

(defun quality+quantity->semitones (quality quantity)
  (gethash (list quantity quality) +interval-semitone-table+))

(defun with-direction (number direction)
  (ecase direction (up number) (down (- number))))

(defun semitone-reference->key (reference style)
  (destructuring-bind (quality quantity direction from-finger) reference
    (number->key
     (typep->value 
      (+ (key-number (slot-value style from-finger))
         (with-direction
          (quality+quantity->semitones quality quantity)
          direction))
      'midi-number))))

(defun relative-reference->key (reference style)
  (etypecase reference
    (color-reference    (color-reference->key reference style))
    (semitone-reference (semitone-reference->key reference style))))

(defun reference->key (finger-reference style)
  (let ((reference (figref-reference finger-reference)))
    (etypecase reference
      (absolute-reference (absolute-reference->key reference))
      (relative-reference (relative-reference->key reference style)))))

(defun resolve-references (style)
  (let ((finger-references (sort-finger-references (collect-finger-values style))))
    (dolist (finger-reference finger-references style)
      (setf (slot-value style (figref-finger finger-reference))
            (reference->key finger-reference style)))))
;;;
;;; style hand-position (updated-pos slot value)
;;; 
(defun initialize-style-hand-pos (style)
  (let* ((hand-pos (make-hand-pos))
         (hand (hand style))
         (current-fingers (hand-pos->fingers hand))
         fingers)
    (dolist (finger +finger-names+                    
                    (setf (hand-pos-fingers hand-pos) fingers
                          (updated-pos style) hand-pos))
      (when (finger-loc->key hand finger)
        (push-last (copy-seq (find finger current-fingers :key 'first)) fingers)))))

(defun hand-xy (style)
  (let ((hand-pos (updated-pos style)))
    (when hand-pos (hand-pos-loc hand-pos))))

(defun set-hand-xy (style xy)
  (let ((hand-pos (updated-pos style)))
    (when hand-pos
      (setf (hand-pos-loc hand-pos)
            (typep->value xy 'xy)))))

(defsetf hand-xy set-hand-xy)

(defun finger-offsets (style)
  (let ((hand-pos (updated-pos style)))
    (when hand-pos (hand-pos-fingers hand-pos))))

(defun set-finger-offsets (style offsets)
  (let ((hand-pos (updated-pos style)))
    (when hand-pos
      (setf (hand-pos-fingers hand-pos) offsets))))

(defsetf finger-offsets set-finger-offsets)

(defun adjust-hand-xy (style)
  "If the style is to adjust the hand position (default), then the hand position is set to the 
closest to the middle finger xy location."
  (when (adjust-hand style)
    (with-slots (thumb index middle ring pinkie) style
      (setf (hand-xy style)
            (typep->value 
             (or (when middle (key-xy middle))
                 (when index (key-xy index))
                 (when ring (key-xy ring))
                 (when thumb (key-xy thumb))
                 (when pinkie (key-xy pinkie)))
             'xy "None of the fingers in the style have xy values to set the hand location.")))))

(defun xy-offset (hand-xy finger-xy)
  (->xy (list (- (x finger-xy) (x hand-xy)) 
              (- (y finger-xy) (y hand-xy)))))

(defun adjust-finger-offsets (style)
  (let ((hand-xy        (hand-xy style))
        (finger-offsets (finger-offsets style))
        new-finger-offsets)
    (dolist (finger +finger-names+ 
                    (setf (finger-offsets style) new-finger-offsets))                 
      (let ((key           (slot-value style finger))
            (finger-offset (find finger finger-offsets :key #'first)))
        (if key
            (push-last (list finger (xy-offset hand-xy (key-xy key)))
                       new-finger-offsets)
          (when finger-offset (push-last finger-offset new-finger-offsets)))))))

;;;
;;; finger down state utilities
;;;
(defmethod adjust-finger-down-states ((style piano-hand-style))
  "Called by every piano-hand styles."
  (let* ((hand (hand style))
         (hand-pos (copy-hand-pos (updated-pos style)))
         (fingers-in-request (fingers-in-request style))
         (style-finger-down-state (finger-down-state style))
         updated-down-states)
    (dolist (finger +finger-names+ 
                    (setf (hand-pos-other hand-pos) `(extended-fingers ,@updated-down-states)
                          (updated-pos style) hand-pos))
      (push-last 
       (if (member finger fingers-in-request)
           (list finger style-finger-down-state)
         (list finger (finger-down (motor-extension) hand finger)))
       updated-down-states))))

;;;
;;; motor request -> style object
;;; 
(defun verify-feature-slots (piano-hand-style)
  (dolist (slot-name (feature-slots piano-hand-style) piano-hand-style)
    (unless (slot-value piano-hand-style slot-name)
      (error "A value for slot ~S is required." slot-name))))

(defun make-piano-hand-style (request-spec)
  (let* ((style-name         (request-slot-value request-spec 'cmd))
         (style              (make-instance style-name :request-spec request-spec))
         (request-slot-names (remove 'cmd (request-slot-names request-spec))))
    (dolist (slot-name request-slot-names (verify-feature-slots style))
      (setf (slot-value         style        slot-name)
            (request-slot-value request-spec slot-name)))))

;;;
;;; motor module request process
;;; 
(defun process-piano-hand-style-request (motor-module request-spec)
  "Function to use with the extend-manual-requests macro.
Ex. (extend-manual-requests (request-command) process-piano-hand-style-request)."
  (let ((style (make-piano-hand-style request-spec)))
    (unless (check-jam motor-module)
      (prepare-movement motor-module style))))

(defgeneric prepare-additional-features (style))

(defmethod prepare-features ((mtr-mod motor-module) (style piano-hand-style))
  "Called by PM. Common for all piano-hand styles. 
Specific style features to be handled with prepare-additional-features."
  (resolve-references style)
  (initialize-style-hand-pos style)
  (adjust-hand-xy style)
  (adjust-finger-offsets style)
  (adjust-finger-down-states style)
  (prepare-additional-features style))

;;;
;;; piano-hand-style methods
;;; 
(defmethod prepare-additional-features ((style piano-hand-style)))

(defun count-style-differences (s1 s2)
  (let ((count (if (eq (hand s1) (hand s2))
                   0 1)))
    (dolist (finger +finger-names+ count)
      (when (not (equal (key-number (slot-value s1 finger))
                        (key-number (slot-value s2 finger))))
        (incf count)))))

(defmethod feat-differences ((s1 piano-hand-style) (s2 piano-hand-style))
  (count-style-differences s1 s2))

(defmethod compute-exec-time ((mtr-mod motor-module) (style piano-hand-style))
  .05)

(defmethod compute-finish-time ((mtr-mod motor-module) (style piano-hand-style))
  .05)

(defmethod compute-second-exec-time ((mtr-mod motor-module) (style piano-hand-style))
  .05)

(defmethod queue-output-events ((mtr-mod dual-execution-motor-module) (style piano-hand-style)))


;;; before method: set hand position and update hand tracker
(defun updated-finger-down-states (style)
  (remove 'extended-fingers
          (hand-pos-other (updated-pos style))))

(defun update-hand-tracker (style)
  "To be called in queue-output-events methods."
  (let ((extension (motor-extension))
        (hand (hand style)))
    (bt:with-recursive-lock-held ((hand-tracker-lock extension))
      (dolist (finger-state (updated-finger-down-states style))
        (setf (gethash (list hand (first finger-state))
                       (hand-tracker-finger-down extension))
              (second finger-state))))))

(defmethod queue-output-events :before ((mtr-mod dual-execution-motor-module) (style piano-hand-style))
  "Every style requires to set a hand finger positions, and update the motor module hand tracler."
  (schedule-event-relative (seconds->ms (exec-time style)) 'set-hand-position :time-in-ms t :module :motor
                           :destination :motor :params (list (hand style) (updated-pos style))
                           :details (format nil "--> Setting ~S hand position" (hand style)) :output 'high)
  (schedule-event-relative (seconds->ms (exec-time style)) 'update-hand-tracker :time-in-ms t :module :motor
                           :params (list style) :details (format nil "--> Tracking ~S hand" (hand style)) :output 'high))

;;; after method: update the visicon
(defun hands-on-piano ()
  "Which keys are associated to current finger locations."
  (let ((ht (make-hash-table :test #'equalp)))
    (dolist (hand +hand-names+ ht)
      (let ((hand-pos-fingers (hand-pos->fingers hand)))
        (dolist (finger +finger-names+)
          (setf (gethash (list hand finger) ht)
                (when (member finger hand-pos-fingers :key #'first)
                  (gethash (finger-loc (the-hand hand) finger :current t) 
                           (piano-xy->key (piano))))))))))

(defun finger-is-down (hand-finger) 
  (gethash hand-finger (hand-tracker-finger-down (motor-extension))))

(defun show-hands-on-piano ()
  "Usefull to verify where the fingers are."
  (let (fingers)
    (maphash (lambda (hand-finger key)
               (when key
                 (push (list hand-finger
                             (if (finger-is-down hand-finger) 'down 'up)
                             (key-xy key) 
                             (key-note-name key)) fingers)))
             (hands-on-piano))
    (pprint (sort fingers #'< :key (lambda (lst) (x (third lst)))))))
    
(defun update-visicon ()
  (let ((hands-on-piano (hands-on-piano)))
    (maphash (lambda (hand-finger piano-key)
               (when piano-key
                 (with-slots (finger-over) piano-key
                   (modify-piano-key-finger-over
                    piano-key
                    (setf finger-over hand-finger)))))
             hands-on-piano)
    (maphash (lambda (num piano-key)
               (with-slots (finger-over) piano-key
                 (when (not (equal +actr-nil+ finger-over))
                   (when (not (equal num (key-number (gethash finger-over hands-on-piano))))
                     (modify-piano-key-finger-over
                      piano-key
                      (setf finger-over +actr-nil+))))))
             (piano-number->key (piano)))))

(defmethod queue-output-events :after ((mtr-mod dual-execution-motor-module) (style piano-hand-style))
  "Every style updates the visicon to reflect both hands finger positions."
  (schedule-event-relative (seconds->ms (exec-time style)) 'update-visicon :time-in-ms t :module :none
                            :params nil :details "--> Updating the visicon" :output 'high))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; hand-to-piano
;;; Style used to place the hands on the piano, no key pressed.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass hand-to-piano-style (piano-hand-style)
  ()
  (:default-initargs
   :finger-down-state nil))

(defstyle hand-to-piano hand-to-piano-style hand thumb index middle ring pinkie)
(extend-manual-requests 
 (hand-to-piano hand thumb index middle ring pinkie) 
 process-piano-hand-style-request)

(defmethod queue-output-events ((mtr-mod dual-execution-motor-module) (style hand-to-piano))
  "Before and after methods of piano-hand-style are applied.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; release-keys
;;; Style used to release multiple keys simultaneously using one hand.
;;; The difference with the hand-to-piano style is that not all fingers 
;;; of the hand need to release keys.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstyle release-keys hand-to-piano-style hand)
(extend-manual-requests 
 (release-keys hand thumb index middle ring pinkie) 
 process-piano-hand-style-request)

(defun midi-key-off (piano-key)
  (with-slots (audicon-id) piano-key
    (end-ongoing-sound audicon-id)
    (setf audicon-id nil)))

(defmethod queue-output-events ((mtr-mod dual-execution-motor-module) (style release-keys))
  "Before and after methods of piano-hand-style are applied."
  (dolist (key (mapcar (lambda (finger) (slot-value style finger))
                       (fingers-in-request style)))
    (schedule-event-relative (seconds->ms (exec-time style)) 'midi-key-off :time-in-ms t :output nil
                               :params (list key))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; press-keys
;;; Style used to press multiple keys using one hand.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass press-keys-style (piano-hand-style)
  ((velocity :initarg :velocity :accessor velocity :initform 100)
   (duration :initarg :duration :accessor duration :initform nil))
  (:default-initargs
   :finger-down-state t))

;;; act-r audio
(defconstant +time-in-ms+ t)  
(defconstant +delay-time+ 50) ; min 50ms production execution time
(defconstant +recode-time+ 285) ;current act-r parameter value
(defconstant +min-cyles-for-pitch-detection+ 3) ;between 2 and 4, need a ref. 
(defconstant +min-attentive-time+ 100) ; 2 productions where (50) + what (50)

(defun pre-attentive-delay (hertz &optional (min-cycles +min-cyles-for-pitch-detection+))
  "Lower hertz requires more cycle to detect a semitone, result in ms."
  (max 50 (round (* 1000 (/ min-cycles (typep->value hertz 'midi-hertz))))))

(defun attentive-delay (hertz &optional (min-cycles +min-cyles-for-pitch-detection+))
  (max (+ (pre-attentive-delay hertz min-cycles) 50)
       +min-attentive-time+))

(defun midi-key-on (piano-key &optional (velocity 80))
  (with-slots (frequency audicon-id) piano-key
    (let ((amplitude (velocity->db velocity)))
      (setf audicon-id
            (new-ongoing-sound 
             'pitch 
             (pre-attentive-delay frequency +min-cyles-for-pitch-detection+)
             (attentive-delay frequency +min-cyles-for-pitch-detection+)
             (mp-time-ms) 'external 'tone +time-in-ms+
             `(:both frequency ,frequency)
             `(:both amplitude ,amplitude))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; press-keys-bloc
;;; Style used to press multiple keys simultaneously using one hand.
;;; to do : Keys can be released within the style if the duration is short.
;;; Otherwise a call to the release style is required. 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstyle press-keys-bloc press-keys-style hand)
(extend-manual-requests 
 (press-keys-bloc hand thumb index middle ring pinkie velocity) 
 process-piano-hand-style-request)

(defmethod queue-output-events ((mtr-mod dual-execution-motor-module) (style press-keys-bloc))
  "Before and after methods of piano-hand-style are applied."
  (dolist (key (mapcar (lambda (finger) (slot-value style finger))
                       (fingers-in-request style)))
    (schedule-event-relative (seconds->ms (exec-time style)) 'midi-key-on :time-in-ms t :output nil
                             :params (list key (velocity style)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; press-keys-broken
;;; Style used to press multiple keys one at a time using one hand.
;;; fingers order in the motor request determine the order of finger pressing.
;;; keys are released automatically after a duration. 
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass press-keys-broken-style (press-keys-style)
  ((fingers-order :accessor fingers-order :initform nil)
   (press-offset :accessor press-offset :initform 0)))

(defstyle press-keys-broken press-keys-broken-style hand duration press-offset)
(extend-manual-requests 
 (press-keys-broken hand thumb index middle ring pinkie velocity duration press-offset) 
 process-piano-hand-style-request)

(defmethod prepare-additional-features ((style press-keys-broken))
  (let (fingers-order)
    (dolist (finger (fingers-in-request style) 
                    (setf (fingers-order style)
                          fingers-order))
      (when (member finger +finger-names+)
        (push-end finger fingers-order)
        (setf (key-is-down (slot-value style finger))
              t)))))

(defun set-finger-is-up (hand finger)
  (let ((extension (motor-extension)))
    (bt:with-recursive-lock-held ((hand-tracker-lock extension))
      (setf (gethash (list hand finger)
                     (hand-tracker-finger-down extension))
            nil))))

(defmethod queue-output-events ((mtr-mod dual-execution-motor-module) (style press-keys-broken))
  (let ((duration (duration style))
        (exec-time (exec-time style))
        (press-offset (press-offset style)))
    (do* ((keys (mapcar (lambda (finger) (slot-value style finger))
                        (fingers-in-request style))
                (cdr keys))
          (key (car keys) 
               (car keys))
          (press-time exec-time 
                      (+ press-time press-offset))
          (release-time (+ exec-time duration)
                        (+ release-time duration)))
         ((null key) t)
      (schedule-event-relative (seconds->ms press-time) 'midi-key-on :time-in-ms t :output 'high
                               :params (list key (velocity style)) :details "--> midi-key-on")
      (schedule-event-relative (seconds->ms release-time) 'midi-key-off :time-in-ms t :output 'high
                               :params (list key) :details "--> midi-key-off")
      (schedule-event-relative (seconds->ms release-time) 'set-finger-is-up :time-in-ms t :output 'high
                               :params (list (hand style) key) :details "--> set-finger-is-up"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; press-keys-overlapping
;;; Style used to press multiple keys one at a time using one hand.
;;; fingers order in the motor request determine the order of finger pressing.
;;; keys, once pressed, are hold until a key-release issue is requested. 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







;;; eof