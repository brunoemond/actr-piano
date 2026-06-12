;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;;
;;; piano.lisp
;;;
;;; 2026-05-27
;;; Bruno Emond bruno.emond@icloud.com
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The purpose of this code is to enable cognitive modelling of 
;;; piano learning and performance using the ACT-R cognitive architecture. 
;;; The code has been tested for LispWorks ans Steel Bank Common Lisp.
;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type utilities
;;;
(defmacro typep->value (value type)
  "A type checking macro which returns the tested value if it is of the correct type." 
  `(let ((value% ,value) (type% ,type))
     (assert (typep value% type%) (value%) 
       "Got ~S of type ~S, expected type ~S :type-doc ~S." 
       value% (type-of value%) type% (documentation type% 'type))
     value%))

;;; type conversion
(defun ->symbol (object)
  (etypecase object
    (symbol object)
    (string (intern (string-upcase object)))))

(defun ->string (object)
  (etypecase object
    (symbol (string-downcase (symbol-name object)))
    (string object)))

(defun ->vector (object)
  (etypecase object
    (simple-vector object)
    (list (apply #'vector object))))

(defun ->list (object)
  (etypecase object
    (list object)
    (simple-vector (coerce object 'list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; hash-table
;;;
(defun index-hash-table (source-ht index-fct &optional (index-ht (make-hash-table :test #'equalp)))
  (let ((ht (typep->value source-ht 'hash-table))
        (fct (typep->value index-fct 'function))
        (indx (typep->value index-ht 'hash-table)))
    (maphash (lambda (key value)
               (declare (ignore key))
               (setf (gethash (funcall fct value) indx) value))
             ht)
    indx))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  xy-coordinate 
;;;
(defun isa-xy (object)
  (and (or (typep object 'list)
           (typep object 'simple-vector))
       (eq (length object) 2)
       (every (lambda (i) (typep i 'number)) object)))

(deftype xy ()
  "A list or simple-vector of 2 numbers."
  '(satisfies isa-xy))

(defun x (object)
  (etypecase (typep->value object 'xy)
    (simple-vector (aref object 0))
    (list          (first object))))

(defun y (object)
  (etypecase (typep->value object 'xy)
    (simple-vector (aref object 1))
    (list          (second object))))

(defun w (object) (x object))
(defun h (object) (y object))

(defun xy-distance (xy1 xy2)
  "Distance between 2 cartesian coordinates."
  (let ((v1 (->vector xy1)) (v2 (->vector xy2)))
    (sqrt (+ (expt (- (x v2) (x v1)) 2) 
             (expt (- (y v2) (y v1)) 2)))))

(defun xy-radians (xy-origin xy)
  "Angle in radians between a source and cartesian coordinates."
  (let ((v1 (->vector xy-origin)) (v2 (->vector xy)))
    (atan (- (y v2) (y v1)) 
          (- (x v2) (x v1)))))

(defun xy-offset (xy-origin xy)
  (let ((v1 (->vector xy-origin)) (v2 (->vector xy)))
    (->vector (list (- (x v2) (x v1)) 
                    (- (y v2) (y v1))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; midi utilities
;;; 
(defparameter *min-midi-number* 21)  ; A0 in an 88 keys keyboard
(defparameter *max-midi-number* 108) ; C8 in an 88 keys keyboard

(deftype midi-number () 
  "A range of integers between *min-midi-number* and *max-midi-number*"
  `(integer ,*min-midi-number* ,*max-midi-number*))

(defconstant +C4-number+ 60)
(defconstant +A4-number+ 69)
(defconstant +A4-frequency+ 440.0)

(defun midi-number->frequency (midi-number)
  "MIDI Tuning Standard."
  (* +A4-frequency+ 
     (expt 2 (/ (- (typep->value midi-number 'midi-number) +A4-number+) 12))))

(deftype tone () '(integer 1 12))

(defun midi-number->tone (midi-number)
  "Midi number -> octave tone, 1 to 12."
  (1+ (mod (typep->value midi-number 'midi-number) 12)))

(defun midi-number->octave (midi-number)
  "Midi number -> octave, 0 to 8."
  (- (floor (typep->value midi-number 'midi-number) 12) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  hands and fingers
;;;
(defconstant +actr-nil+ 'none "Instead of nil values for chunk slot values.")
(defconstant +hand-names+ '(right left))
(defconstant +finger-names+ '(thumb index middle ring pinkie))

(deftype hand-name   () `(member ,@+hand-names+))
(deftype finger-name () `(member ,@+finger-names+))

(defun isa-hand-finger (object)
  (or (equal object +actr-nil+)
      (and (listp object)
           (eq (length object) 2)
           (typep (first object) 'hand-name)
           (typep (second object) 'finger-name))))

(deftype hand-finger () '(satisfies isa-hand-finger))

(defun ->hand-finger (hand finger)
  (typep->value (list hand finger) 'hand-finger))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  action-on-hand-fingers
;;;
(defun isa-midi-or-nil (object)
  (or (null object)
      (typep object 'midi-number)))

(deftype midi-or-nil () '(satisfies isa-midi-or-nil))

(defstruct (hand-motor-action
            (:constructor make-hma
             (&optional (fct% #'dummy-motor-action)
                        (hand% 'right)
                        thumb% index% middle% ring% pinkie%))
            (:conc-name hma-))
  "A structure to hold information on the action to be performed with a hand and fingers.
This structure is created initially at motor request time, and used later at motor execution time.
This allows to separate the hand motor module operations from the actions on the piano device."
  (action  (typep->value fct%    'function))
  (hand    (typep->value hand%   'hand-name))
  (thumb   (typep->value thumb%  'midi-or-nil))
  (index   (typep->value index%  'midi-or-nil))
  (middle  (typep->value middle% 'midi-or-nil))
  (ring    (typep->value ring%   'midi-or-nil))
  (pinkie  (typep->value pinkie% 'midi-or-nil)))

(defun execute-hand-motor-action (hand-motor-action)
  (with-slots (action hand thumb index middle ring pinkie) hand-motor-action
    (apply action (list hand thumb index middle ring pinkie))))

(defun dummy-motor-action (hand thumb index middle ring pinkie)
  (declare (ignore hand thumb index middle ring pinkie))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reference to keys under fingers require absolute reference to keys on the keyboard. 
;;; However, when refering to finger positions on keys, sometimes the reference is
;;; relative to an anchor key position.  
;;; The current implementation supports two relative references: key color references and 
;;; semitone references. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; key-color intervals
;;;

(defparameter *key-colors* '(white black))
(deftype key-color () `(member ,@*key-colors*))
(deftype adjacent-color-key-range () '(integer 1 8)
  "Number of keys of the same color for counting interval range [1..8].")

(defun tone->color (tone)
  (if (member tone '(2 4 7 9 11))
      'black 'white))

(defun isa-color-reference (object)
  (and (listp object)
       (eq (length object) 3)
       (typep (nth 0 object) 'key-color)
       (typep (nth 1 object) 'adjacent-color-key-range) 
       (typep (nth 2 object) 'direction)))

(deftype color-reference () '(satisfies isa-color-reference))

(defun count-keys-of-color (start end% color%)
  (let ((end (typep->value end% 'midi-number))
        (color (typep->value color% 'key-color)))
    (do* ((i (typep->value start 'midi-number) (1+ i))
          (nb-keys (if (eq color (tone->color (midi-number->tone i)))
                       1 0)
                   (if (eq color (tone->color (midi-number->tone i)))
                       (1+ nb-keys) nb-keys)))
         ((eq i end) nb-keys))))

(defun count-white-keys-before (midi-number)
  "Counts the number of white keys (white) before a given midi number."
  (let ((end (typep->value midi-number 'midi-number)))
    (do* ((i *min-midi-number* (1+ i))
          (isa-white-key (isa-white-key i)
                       (isa-white-key i))
          (nb-whites (if isa-white-key 1 0)
                       (if isa-white-key (1+ nb-whites) nb-whites)))
         ((eq i end) 
          (1- nb-whites)))))

(defun midi-number-after-n-color (start color% tot-color direction)
  (let ((color (typep->value color% 'key-color)))
    (do* ((i (typep->value start 'midi-number)
             (ecase direction (up (1+ i)) (down (1- i))))
          (nb-color (if (equal color (tone->color (midi-number->tone i)))
                        1 0)
                    (if (equal color (tone->color (midi-number->tone i)))
                        (1+ nb-color) nb-color)))
         ((eq nb-color tot-color) 
          (typep->value i 'midi-number)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; semitone intervals
;;;
(defconstant +qualities+ '(pref min maj dim aug)
  "Interval qualities: perfect, minor, major, diminished, augmented.")
(deftype quality () `(member ,@+qualities+))
(defconstant +directions+ '(up down))
(deftype direction () `(member ,@+directions+))

(defun isa-semitone-reference (object)
  (and (listp object)
       (eq (length object) 3)
       (typep (nth 0 object) 'quality)
       (typep (nth 1 object) 'tone)
       (typep (nth 2 object) 'direction)))

(deftype semitone-reference () 
  "A semitone-reference has 3 elements (quality number direction)."
  '(satisfies isa-semitone-reference))

(defconstant +interval-semitone-table+
  (let ((table (make-hash-table :test 'equal)))
    (dolist (entry '(;; (number quality semitones)
                     (1 perf 0) (1 aug   1)
                     (2 dim  0) (2 min   1) (2 maj 2)  (2 aug 3)
                     (3 dim  2) (3 min   3) (3 maj 4)  (3 aug 5)
                     (4 dim  4) (4 perf  5) (4 aug 6)
                     (5 dim  6) (5 perf  7) (5 aug 8)
                     (6 dim  7) (6 min   8) (6 maj 9)  (6 aug 10)
                     (7 dim  9) (7 min  10) (7 maj 11) (7 aug 12)
                     (8 dim 11) (8 perf 12) (8 aug 13))
                   table)
      (destructuring-bind (number quality semitones) entry
        (setf (gethash `(,number ,quality) table) semitones))))
  "Maps (number quality) to semitone count for simple intervals [unison..octave].")

(defun with-direction (number direction)
  (ecase direction (up number) (down (- number))))
     
(defun midi-number-after-semitone-interval (start quality tone direction)                    
  (typep->value 
   (+ start 
      (with-direction
       (gethash (list (typep-> tone 'tone)
                      (typep-> quality 'quality))
                +interval-semitone-table+)
       direction))
   'midi-number))

;;;
;;; relative reference main function
;;;
(defmethod reference->midi-number ((start integer) (relative-reference list))
  (etypecase relative-reference
    (semitone-reference 
     (midi-number-after-semitone-interval
      (typep->value start 'midi-number)
      (nth 0 relative-reference) (nth 1 relative-reference) (nth 2 relative-reference)))
    (color-reference
     (midi-number-after-n-color
      (typep->value start 'midi-number)
      (nth 0 relative-reference) (nth 1 relative-reference) (nth 2 relative-reference)))))

(assert (eq (midi-number-after-semitone-interval 60 'maj 3 'up)
            (midi-number-after-n-color 60 'white 3 'up)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; piano-key-names
;;;

;;; midi-number

;;; color name

;;; notation

;;; xy



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions to compute xy coordinates
;;;
;;; Black keys are assumed to be located in-between white keys. 
;;; This is an approximation which is not what an actual physical layout of a piano is.
;;;
;;; defvars are dynamically set in function calls, they should not be set manually.
(defvar *x-origin*)
(defvar *y-origin*)
(defvar *white-width*)
(defvar *half-white-width*)
(defvar *nb-whites-before*)

(defun initial-x ()
  "The initial x coordinate is the x-origin + half the white key width."
  (+ *x-origin* 
     *half-white-width*))

(defun x-white ()
  "The x coordinate of a white key is the initial x value + the number of white widths before rtge key."
  (+ (initial-x) 
     (* *nb-whites-before* *white-width*)))

(defun x-black ()
  "The x coordinate of an black key is coordinate of the previous white + half white width."
  (+ (x-white)
     *half-white-width*))

(defun y-with-offset (offset)
  "The y origin + an offset."
  (+ *y-origin* offset))

(defun x-coor (midi-number x-origin white-width)
  (let ((*x-origin* x-origin)       
        (*white-width* white-width)
        (*half-white-width* (round (/ white-width 2)))
        (*nb-whites-before* (1- (count-keys-of-color *min-midi-number* midi-number 'white))))
    (if (isa-white-key midi-number)
        (x-white)
      (x-black))))
                      
(defun y-coor (y-origin y-offset)
  (let ((*y-origin* y-origin))
    (y-with-offset y-offset)))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; act-r piano-component
;;; 
(defparameter *piano-type* 'midi-keys "The default piano structure name.")
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
;;; midi-key & midi-keys
;;; 
            
;;;
;;; midi-key
;;; 
(defstruct (midi-key           
            (:constructor make-midi-key% (number%))
            (:conc-name key-))
  "The basic structure for a piano key with midi number and audio frequency."
  (number    number%)
  (frequency (midi-number->frequency number%)))

(defun make-midi-key (midi-number)
  (let ((key (make-midi-key% (typep->value midi-number 'midi-number))))
       key))

(defun make-keys (key-constructor)
  "Generates a hash table of piano keys using the key structure constructor."
  (let ((fct (typep->value key-constructor 'function))
        (ht  (make-hash-table :test #'equalp)))
    (do ((i *min-midi-number* (incf i)))
        ((> i *max-midi-number*) ht)
      (setf (gethash i ht) 
            (funcall fct i)))))

(defmethod reference->midi-number ((midi-key midi-key) (relative-reference list))
  (reference->midi-number (key-number midi-key) relative-reference))


;;;
;;; midi-keys
;;; 
(defstruct (midi-keys 
            (:constructor make-midi-keys)
            (:conc-name piano-))
  "A minimal piano structure for using as an actr component and device."
  (number->key (make-keys #'make-midi-key)))

(defun number->key (midi-number)
  (gethash (typep->value midi-number 'midi-number)
           (piano-number->key (piano))))

(defmethod reference->key ((ref integer))
  (number->key ref))

(defun component-with-keys (piano-constructor)
  (let ((fct (typep->value piano-constructor 'function))
        (component-device (make-component-device)))
    (setf (act-r-component-instance component-device)
          (funcall fct))))
    
(defmethod make-piano-type ((piano-type (eql 'midi-keys)))
  (let ((piano (component-with-keys #'make-midi-keys)))
    piano))

(defun make-piano ()
  (make-piano-type *piano-type*))


;;; test midi-keys
(defun test-model1 ()
  (setf *piano-type* 'midi-keys)
  (clear-all)
  (make-piano)
  (define-model foo
    (install-device '("motor" "piano")))
  (values 
   (piano)
   (defined-devices)
   (current-devices "motor")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; visual-key & visual-piano
;;; 

;;;
;;; 
(defparameter *piano-visual-xy-origin* #(0 0)
  "The pixels origin of the piano keyboard.")

;;; piano key sizes in mm
;;; the keys height is not considered. 
;;; However, for actr, the depth is the height. 
(defparameter *visual-white-width*      23) 
(defparameter *visual-white-depth*     150)
(defparameter *visual-black-width*   11)
(defparameter *visual-black-depth*   95)
(defconstant +mm/inch+ 25.4)
;;; actr parameters
(defparameter *default-pixel-per-inch* 72
  "Need to set (sgp :pixels-per-inch 72), if *default-pixel-per-inch* is different.")

(defun mm->pixels (mm)
  (round (* *default-pixel-per-inch* 
            (/ mm +mm/inch+))))

;;; pixel values
(defparameter *visual-white-width-pixel* (mm->pixels *visual-white-width*))
(defparameter *visual-black-width-pixel* (mm->pixels *visual-black-width*))
(defparameter *visual-white-depth-pixel* (mm->pixels *visual-white-depth*))
(defparameter *visual-black-depth-pixel* (mm->pixels *visual-black-depth*))
;;; only one y coordinate associated to the near location at .8 like the tactile offset
(defparameter *visual-y-offset* .8)

(defun visual-xy (midi-number)
  (->vector
   (list
    (x-coor midi-number (x *piano-visual-xy-origin*) *visual-white-width-pixel*)
    (y-coor (y *piano-visual-xy-origin*)
            (if (isa-white-key midi-number)
                (round (* *visual-white-depth-pixel* *visual-y-offset*))
              (round (* *visual-black-depth-pixel* *visual-y-offset*)))))))

;;;; width and height (depth)
(defun visual-w (midi-number)
  (if (isa-white-key midi-number) 
      *visual-white-width-pixel*
    *visual-black-width-pixel*))

(defun visual-d (midi-number)
  "Depth is used for width in (w h) coordinate."
  (if (isa-white-key midi-number) 
       *visual-white-depth-pixel*
   *visual-black-depth-pixel*))

(defun isa-white-key (midi-number)
  "Is the midi number value a white tone?."
  (member (midi-number->tone midi-number) 
          '(1 3 5 6 8 10 12)))

(defun count-white-keys-before (midi-number)
  "Counts the number of white keys (white) before a given midi number."
  (let ((end (typep->value midi-number 'midi-number)))
    (do* ((i *min-midi-number* (1+ i))
          (isa-white-key (isa-white-key i)
                       (isa-white-key i))
          (nb-whites (if isa-white-key 1 0)
                       (if isa-white-key (1+ nb-whites) nb-whites)))
         ((eq i end) 
          (1- nb-whites)))))
;;;
;;; piano-key visual properties
;;;


(defparameter *key-groups* '(black2 black3))
(deftype key-group () `(member ,@*key-groups*)
  "Group key names: black2 (2 blacks + 3 whites) or black3 (3 blacks + 4 whites).")

(defun tone->key-group (tone)
  (if (< tone 6) 'black2 'black3))

(defparameter *group-positions* '(left middle right midleft midright))
(deftype group-position () `(member ,@*group-positions*)
  "Group positions are: left, right middle, midleft and midright. The last two for white kyes in black3.")

(defun tone->position (tone)
  (case tone
    (1 'left)  (2 'left)  (3 'middle)  
    (4 'right) (5 'right)
    (6 'left) (7 'left) 
    (8 'midleft) (9 'middle) (10 'midright) 
    (11 'right) (12 'right)))

(defparameter *octaves* '(o0 o1 o2 o3 o4 o5 o6 o7 o8))
(deftype octave () `(member ,@*octaves*)
  "Octave names range from O0 to O8.")

(defun midi-number>octave-name (midi-number)
  (intern (format nil "O~S" (midi-number->octave midi-number))))

(defun midi-number->visual-class (midi-number)
  "A symbol combining the key group, key position, and key color."
  (let ((tone (midi-number->tone midi-number)))
    (intern (format nil "~S-~S-~S"
                    (tone->key-group tone)
                    (tone->position  tone)
                    (tone->color     tone)))))

(defun visual-classes ()
  (let (visual-class-chunks)
    (do* ((i *min-midi-number* (incf i)))
         ((> i *max-midi-number*)
          visual-class-chunks)
      (setf visual-class-chunks
            (adjoin `(,(midi-number->visual-class i)
                      isa visual-class
                      group ,(tone->key-group (midi-number->tone i))
                      position ,(tone->position (midi-number->tone i))
                      color ,(tone->color (midi-number->tone i)))
                    visual-class-chunks
                    :test 'equalp)))))

(defparameter *visual-classes* (visual-classes))
(deftype visual-class () `(member ,@*visual-classes*))

(defun isa-octave-visclass (object)
  (and (listp object)
       (eq (length object) 2)
       (and (typep (first object) 'octave)
            (typep (second object) 'visual-class))))

(deftype octave-visclass () '(satisfies isa-octave-visclass))

(defun piano-key-visual-chunks ()
  (define-chunks-fct +actr-nil+)
  (define-chunks-fct *key-colors*)
  (define-chunks-fct *key-groups*)
  (define-chunks-fct *group-positions*)
  (define-chunks-fct *octaves*)
  (define-chunks-fct *visual-classes*))

(defun piano-visual-key-types ()
  (chunk-type visual-class group position color)
  (chunk-type (visual-key (:include visual-location))
              visual-class))

#|
;;; adjacent black keys (w when none)
(defun adj-black (midi-number)
  (let ((tone (midi-number->tone midi-number)))
    (cond ((member tone '(1 2  6  7)) 'wb)
          ((member tone '(3 8  9 10)) 'bb)
          ((member tone '(4 5 11 12)) 'bw))))

;;; position (left middle right) in black group (2 or 3 blacks)
(defun group-pos (midi-number)
  (let ((tone (midi-number->tone midi-number)))
    (cond ((member tone '(1 2))      'b2l)
          ((member tone '(3))        'b2m)
          ((member tone '(4 5))      'b2r)
          ((member tone '(6 7 8))    'b3l)
          ((member tone '(9))        'b3m)
          ((member tone '(10 11 12)) 'b3r))))
|#

;;;
;;; visual-key
;;;
(defstruct (visual-key
            (:include midi-key)
            (:conc-name key-)
            (:constructor make-visual-key 
             (number%
              &aux
              (tone% (midi-number->tone number%)))))
  "Structure for the visual properties of a piano key."
  (xy          (visual-xy number%))
  (wh          (->vector (list (visual-w number%) (visual-d number%))))
  (color       (tone->color tone%))
  (visclass    (midi-number->visual-class number%))
  (octave      (midi-number>octave-name number%))  
  (finger-over +actr-nil+)
  visicon-id)

;;; visicon
(defun visual-key-features (visual-key)
  (with-slots (xy wh color visclass octave finger-over) visual-key
    `(;; visicon features
      screen-x    ,(x xy) 
      screen-y    ,(y xy)
      width       ,(w wh)
      height      ,(h wh)
      color       ,color
      visclass    ,visclass
      octave      ,octave
      finger-over ,finger-over)))
   
(defun visual-key-to-visicon (visual-key)
  (setf (key-visicon-id visual-key)
        (car (add-visicon-features (visual-key-features visual-key)))))

(defun modify-visual-key-finger-over (visual-key hand-finger)
  (with-slots (visicon-id) visual-key
    (modify-visicon-features `(,visicon-id finger-over (,hand-finger ,hand-finger)))))

;;;
;;; visual-piano
;;;
(defstruct (visual-piano 
            (:include midi-keys
             (number->key (make-keys #'make-visual-key)))
            (:conc-name piano-)
            (:constructor make-visual-piano))
  (xy->key              (make-hash-table :test #'equalp))
  (octave-visclass->key (make-hash-table :test #'equalp))
  (hand-finger->key     (make-hash-table :test #'equalp)))

(defmethod make-piano-type ((piano-type (eql 'visual-piano)))
  (let ((piano (component-with-keys #'make-visual-piano)))
    (with-slots (number->key xy->key octave-visclass->key) piano
      (setf xy->key 
            (index-hash-table number->key #'key-xy)
            octave-visclass->key
            (index-hash-table 
             number->key 
             (lambda (key) (list (key-octave key) (key-visclass key))))))
    piano))

(defun xy->key (xy)
  (gethash (->vector (typep->value xy 'xy))
           (piano-xy->key (piano))))

(defun octave-visclass->key (octave-visclass)
  (gethash (typep->value octave-visclass 'octave-visclass)
           (piano-octave-visclass->key (piano))))

(defun hand-finger->key (hand finger)
  (gethash (->hand-finger hand finger)
           (piano-hand-finger->key (piano))))






;;; visual-key methods
(defmethod place-finger-over-key ((visual-key visual-key) hand finger)
  (with-slots (finger-over) visual-key
    (modify-visual-key-finger-over
     visual-key
     (setf finger-over (->hand-finger hand finger)))))

(defmethod place-finger-over-key ((midi-number integer) hand finger)
  (place-finger-over-key (number->key midi-number) hand finger))

(defmethod remove-finger-from-key ((visual-key visual-key))
  (with-slots (finger-over) visual-key
    (modify-visual-key-finger-over
     visual-key
     (setf finger-over +actr-nil+))))

(defmethod remove-finger-from-key ((midi-number integer))
  (remove-finger-from-key (number->key midi-number)))

;;;
;;; motor module utilities
;;;
(defun right-hand% ()
  (right-hand (get-module :motor)))

(defun left-hand% ()
  (left-hand (get-module :motor)))


(defmethod reference->key ((xy vector)) (xy->key xy))
(defmethod reference->key ((list list)) 
  (etypecase list
    (xy (xy->key list))
    (octave-visclass (octave-visclass->key list))))
    


(defun place-fingers-over-keys (hand &key thumb index middle ring pinkie)
  (place-finger-over-key (typep->value thumb 'midi-number) (typep->value hand 'hand-name) 'thumb)
  (place-finger-over-key (typep->value index 'midi-number) hand 'index)
  (place-finger-over-key (typep->value middle 'midi-number) hand 'middle)
  (place-finger-over-key (typep->value ring 'midi-number) hand 'ring)
  (place-finger-over-key (typep->value pinkie 'midi-number) hand 'pinkie))


(defun hand-to-piano (hand)
  (ecase hand
    (right (place-fingers-over-keys
            'right
            :thumb  60
            :index  62
            :middle 64
            :ring   65
            :pinkie 67))
    (left (place-fingers-over-keys
           'left
           :thumb  60
           :index  59
           :middle 57
           :ring   55
           :pinkie 53))))

  




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; piano-hand-style
;;; 
(defclass piano-hand-style (dual-hand-movement-style)
  ((hand :initarg :hand :initform 'right :accessor hand)
   (loc :initarg :loc :initform nil :accessor loc)
   (thumb  :initform nil :accessor thumb)
   (index  :initform nil :accessor index)
   (middle :initform nil :accessor middle)
   (ring   :initform nil :accessor ring)
   (pinkie :initform nil :accessor pinkie))
  (:default-initargs
   :two-exec-p nil
   :exec2-time 0
   :finger-based-style t
   :release-if-down nil
   :feature-slots nil
   :style-name 'piano-hand-style))


(defclass move-hand-to-keys (piano-hand-style)
  ()
  (:default-initargs
   :feature-slots '(hand thumb index middle ring pinkie)))




;;; test midi-keys
(defun test-model2 ()
  (setf *piano-type* 'visual-piano)
  (clear-all)
  (make-piano)
  (define-model foo
    (install-device '("motor" "piano")))
  (values 
   (piano)
   (defined-devices)
   (current-devices "motor")))




#|


;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; midi-number keys, piano component and device
;;; 


(defun middle-c ()
  (number->key +C4-number+))

;;; component functions





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; visual-key and visual-piano
;;; 









 ;(,hf ,hf)

;;;
;;; visual-piano
;;;
(defstruct (visual-piano 
            (:include piano-component)
            (:conc-name piano-)
            (:constructor make-visual-piano%))
  (visual->key      (make-hash-table :test #'equalp))
  (visclass->key    (make-hash-table :test #'equalp))
  (hand-finger->key (make-hash-table :test #'equalp)))

(defun visual-piano-chunks (piano)
  (with-slots (visclass->key) piano
    (maphash (lambda (key value)
               (declare (ignore value))
               (setf *octave-chunks* (adjoin (first key) *octave-chunks*)
                     *visclass-chunks* (adjoin (second key) *visclass-chunks*)))
            visclass->key))) 
                       

(defun make-visual-piano ()
  (let ((piano (make-visual-piano%)))
    (with-slots (number->key visual->key visclass->key) piano
      (setf number->key   (make-keys #'make-visual-key)
            visual->key   (index-hash-table number->key #'key-visual-xy)
            visclass->key (index-hash-table
                           number->key 
                           (lambda (key)
                             (list (key-octave key)
                                   (key-vis-class key))))))
    (visual-piano-chunks piano)
    piano))

(defun hand-finger->keys ()
  (with-slots (hand-finger->key) (piano)
    hand-finger->key))

(defun hand-finger->key (hand finger)
  (gethash (list hand finger) 
           (hand-finger->keys)))

(defun piano-keys-to-visicon ()
  (maphash (lambda (key visual-key) 
             (declare (ignore key))
             (visual-key-to-visicon visual-key))
           (piano-visual->key (piano)))
  (visicon (get-module :vision)))

(defun isa-visclass-ref (object)
  (and (listp object)
       (eq 2 (length object))
       (member (first object) *octave-chunks*)
       (member (second object) *visclass-chunks*)))

(deftype visclass-ref ()
  '(satisfies isa-visclass-ref))

(defun isa-color (object)
  (member object '(w b)))


(deftype color ()
  '(satisfies isa-color))

(defun isa-relative-ref (object)
  (and (listp object)
       (eq (length object) 3)
       (typep (nth 0 object) 'finger-name)
       (typep (nth 1 object) 'color)
       (typep (nth 2 object) 'integer)))

(deftype relative-ref ()
  '(satisfies isa-relative-ref))

;;;
;;; need to distinguish planning from execution?
;;; planning : compute feature values
;;; execution : make plan current. 
;;; need to integrate with motor module hands. 
;;; maybe no need to have 2 separate coordinate systems 
;;; for motor and vision if motor movement requests are all by interval specs


(defun next-key (anchor-key colour nb-keys)
  (if (not (zerop nb-keys))
      (let ((inc (if (>= nb-keys 0) 1 -1))
            (end (1- (abs nb-keys))))
        (do* ((i (+ (key-number anchor-key) inc)
                 (+ i inc))
              (pkey (number->key i)
                    (number->key i))
              (c (key-color pkey)
                 (key-color pkey))
              (nc (if (equal c colour) 1 0)
                  (if (equal c colour) (incf nc) nc)))
             ((eq nc end) pkey)))
    (error "The number of keys mnust be different from 0.")))

; (next-key (middle-c) 'w 1)

(defun from-finger-next-key (hand finger colour nb-keys)
  (let ((hand-finger-key (hand-finger->key hand finger)))
    (if hand-finger-key
        (next-key hand-finger-key colour nb-keys)
      (error "There is no key under (~S ~S)." hand finger))))
  

       
      
(defun visual->key (xy &optional hand)
  (with-slots (visual->key visclass->key) (piano)
    (etypecase xy
      (int-coor (gethash (->vector (typep->value xy 'int-coor)) visual->key))
      (visclass-ref (gethash xy visclass->key))
      (relative-ref (from-finger-next-key hand (nth 0 xy) (nth 1 xy) (nth 2 xy))))))

(defun collect-fingers (hand)
  (let (fingers-set)
    (with-slots (hand-finger->key) (piano)
      (maphash (lambda (key value)
                 (when (and (equal (first key) hand) (second key))
                   (push value fingers-set)))
               hand-finger->key))
    fingers-set))

(defun hand-xy (hand)
  (key-visual-xy (hand-finger->key hand 'middle)))


(defun finger-offsets (hand)
  (let ((hand-xy (hand-xy hand))
        offsets)
    (dolist (finger-key (collect-fingers hand) 
                    (append (list hand-xy) offsets))
      (push (append (list (key-finger-over finger-key))
                    (coor-offset hand-xy (key-visual-xy finger-key)))
            offsets))))


(defmethod finger-over-key ((visual-key visual-key) hand finger)
  (setf (key-finger-over visual-key) (list hand finger))
  (modify-visicon-finger visual-key (list hand finger))
  (with-slots (hand-finger->key) (piano)
    (setf (gethash (list hand finger) hand-finger->key)
          visual-key)))

(defmethod finger-over-key (xy hand finger)
  (finger-over-key (visual->key xy hand) hand finger))

(defmethod finger-off-key ((visual-key visual-key))
  (with-slots (hand-finger->key) (piano)
    (remhash (key-finger-over visual-key) hand-finger->key))
  (setf (key-finger-over visual-key) +actr-nil+)   
  (modify-visicon-finger visual-key +actr-nil+))

(defmethod finger-off-key (xy)
  (finger-off-key (visual->key xy)))

(defun all-fingers-off () 
  (with-slots (hand-finger->key) (piano)
    (maphash (lambda (key value) 
               (declare (ignore key))
               (finger-off-key value))
             hand-finger->key)))

(defun fingers-over-keys (hand &key thumb index middle ring pinkie)
  (let ((h (typep->value hand 'hand-name)))
    (when thumb  (finger-over-key thumb  h 'thumb))
    (when index  (finger-over-key index  h 'index))
    (when middle (finger-over-key middle h 'middle))
    (when ring   (finger-over-key ring   h 'ring))
    (when pinkie (finger-over-key pinkie h 'pinkie))))

(defun fingers-off-keys (hand &key thumb index middle ring pinkie)
  (let ((h (typep->value hand 'hand-name)))
    (with-slots (hand-finger->key) (piano)
      (maphash (lambda (key value) 
                 (when (equal (first key) h)
                   (when (and thumb (equal (second key) 'thumb)) 
                     (finger-off-key value))
                   (when (and index (equal (second key) 'index))
                     (finger-off-key value))
                   (when (and middle (equal (second key) 'middle))
                     (finger-off-key value))
                   (when (and ring (equal (second key) 'ring))  
                     (finger-off-key value))
                   (when (and pinkie (equal (second key) 'pinkie))
                     (finger-off-key value))))
               hand-finger->key))))



(defun hands-to-piano ()
  (hand-to-piano 'right)
  (hand-to-piano 'left))


(defun move-hand (hand xy)
  ;; compute distance middle to next middle
  ;; move all fingers by distance
  )

; (defun adjust-hand () )
;; move hand if finger over-extend reach
;; thumb and pinkie
  

  
(defmacro with-printed-visicon (&body body)
  `(progn 
     ,@body
     (run-n-events 4)
     (print-visicon)))

(defun piano-chunks ()
  (chunk-type (visual-key (:include visual-location)) 
                vis-class octave finger-over)
  (define-chunks none) ; +actr-nil+
  (define-chunks w b) ; colours
  (define-chunks-fct *octave-chunks*) ; octaves
  (define-chunks-fct *visclass-chunks*) ; vis-class
  )


(defun test-model2 ()
  (setf *default-piano* 'visual-piano)
  (clear-all)
  (make-piano)
  (define-model foo
    (install-device '("motor" "piano"))
    (piano-chunks)
    (piano-keys-to-visicon))

  (with-printed-visicon)
  (with-printed-visicon
    (finger-over-key (key-visual-xy (middle-c)) 'right 'thumb)
    (finger-over-key '(o4 b2-r-w) 'right 'middle))
  (with-printed-visicon
    (finger-off-key (key-visual-xy (middle-c))))
  (with-printed-visicon
    (all-fingers-off))
  (with-printed-visicon
    (fingers-over-keys 
     'right 
     :thumb  '(o4 b2-l-w) 
     :index  '(o4 b2-m-w) 
     :middle '(o4 b2-r-w) 
     :ring   '(o4 b3-l-w) 
     :pinkie '(o4 b3-ml-w)))
  (with-printed-visicon
    (fingers-off-keys 
     'right 
     :thumb  t 
     :index  t 
     :middle t))

  (values 
   (piano)
   (defined-devices)
   (current-devices "motor"))
  )
  

(defun test-model3 ()
  (setf *default-piano* 'visual-piano)
  (clear-all)
  (make-piano)
  (define-model foo
    (sgp :auto-attend t)
    (install-device '("motor" "piano"))
    (piano-chunks)
    (piano-keys-to-visicon)

    (p look-for-key
       ?visual>
       state free
       buffer empty

       ==>
       +visual-location>
       octave O4
       vis-class b2-l-w)

    (p finger-over-key
       ?visual>
       state free

       =visual-location>
       screen-x =x
       screen-y =y

       =visual>

       ==>
       !output! (=visual =visual-location =x =y)
       !eval! (finger-over-key `(,=x ,=y) 'right 'thumb)
       !stop!
       )


    )
  (run .5)
  (describe (hand-finger->keys)))

(defun test-model4 ()
  (setf *default-piano* 'visual-piano)
  (clear-all)
  (make-piano)
  (define-model foo
    (sgp :auto-attend t)
    (install-device '("motor" "piano"))
    (piano-chunks)
    (piano-keys-to-visicon)

    (p look-for-key
       ?visual>
       state free
       buffer empty

       ==>
       +visual-location>
       octave O4
       vis-class b2-l-w)

    (p finger-over-keys
       ?visual>
       state free

       =visual-location>
       screen-x =x
       screen-y =y

       =visual>

       ==>
       !output! (=visual =visual-location =x =y)
       !eval! (fingers-over-keys
               'right
               :thumb  `(,=x ,=y)
               :index  `(thumb w 2)
               :middle `(thumb w 3)
               :ring   `(thumb w 4)
               :pinkie `(thumb w 5))
       !stop!
       )


    )
  (run .5)
  (describe (hand-finger->keys)))



|#


#|




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; piano-keys
;;;
;;; midi-keys    : audio properties
;;; visual-keys  : visual properties (include midi-keys)
;;; tactile-keys : tactile properties (include visual-keys)
;;; piano-keys   : key names (include tactile-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; midi-key
;;; midi-keys are for the audio properties for piano keys. 
;;;

;;; midi-number

;;; midi audio dB and velocity
(defconstant +C4-number+ 60)
(defconstant +A4-number+ 69)
(defconstant +A4-frequency+ 440.0)
(defconstant +piano-maximum-dB+ 100) 
(defconstant +room-dB+ 40) 

(defconstant +min-velocity+ 0)
(defconstant +max-velocity+ 127)
(deftype velocity () 
  "A velocity value between +min-velocity+ and +max-velocity+."
  `(integer ,+min-velocity+ ,+max-velocity+))



(deftype midi-hertz ()
  `(real ,(midi-number->frequency +min-midi-number+) 
         ,(midi-number->frequency +max-midi-number+)))

(defun velocity->midi-dB (velocity)
  "From: GENERAL MIDI SYSTEM LEVEL 1 DEVELOPER GUIDELINES, 1998, p.9"
  (if (> (typep->value velocity 'velocity) 0)
      (* 40 (log (/ velocity +max-velocity+) 10))
    :no-value))

(defun velocity->db (velocity)
  "From velocity to dB, taking into account room noise level."
  (let* ((midi-dB (velocity->midi-dB velocity))
         (db (if (equal midi-dB :no-value)
                0 (+ +piano-maximum-dB+ midi-dB))))
    (if (> db +room-dB+) db 0)))

;;;
;;; midi-key 
;;;
(defstruct (midi-key
            (:constructor make-midi-key
             (number%)))
  (number number%)
  (frequency (midi-number->frequency number%))
  (amplitude 0)
  audicon-id)

;;; act-r audio
(defconstant +time-in-ms+ t)  
(defconstant +delay-time+ 50) ; min 50ms production execution time
(defconstant +recode-time+ 285) ;current act-r parameter value
(defconstant +min-cyles-for-pitch-detection+ 3) ;between 2 and 4, need a ref. 
(defconstant +min-attentive-time+ 100) ; 2 productions where (50) + what (50)

(defun pre-attentive-delay (hertz &optional (min-cycles +min-cyles-for-pitch-detection+))
  "Lower hertz requires more cycle to detect a tone, result in ms."
  (max 50 (round (* 1000 (/ min-cycles (typep->value hertz 'midi-hertz))))))

(defun attentive-delay (hertz &optional (min-cycles +min-cyles-for-pitch-detection+))
  (max (+ (pre-attentive-delay hertz min-cycles) 50)
       +min-attentive-time+))

(defun midi-key-on (midi-key &optional (velocity 80))
  (with-slots (frequency amplitude audicon-id) midi-key
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
    midi-key))

(defun midi-key-off (midi-key)
  (with-slots (amplitude audicon-id) midi-key
    (end-ongoing-sound audicon-id)
    (setf amplitude 0
          audicon-id nil)
    midi-key))

;;;
;;; midi-keys
;;;
(defstruct (midi-keys 
            (:constructor make-midi-keys%))
  (midi->key (make-hash-table)))

(defun fill-midi-keys (hash-table function)
  (do ((number +min-midi-number+ (incf number)))
      ((> number +max-midi-number+) 
       hash-table)
    (setf (gethash number hash-table)
          (funcall function number))))

(defun index-hash-table (ht-source ht-index function)
  (maphash (lambda (key value)
             (declare (ignore key))
             (setf (gethash (funcall function value) ht-index)
                   value))
           ht-source))
  
(defun make-midi-keys ()
  (let ((piano-keys (make-midi-keys%)))
    (with-slots (midi->key) piano-keys
      (fill-midi-keys midi->key #'make-midi-key)
    piano-keys)))  

(defun midi->key (midi-number midi-keys)
  (with-slots (midi->key) midi-keys
    (gethash midi-number midi->key)))

(defun note-on (midi-keys midi-number &optional (velocity 80))
  (midi-key-on (midi->key (typep->value midi-number 'midi-number) midi-keys)
               (typep->value velocity 'velocity)))

(defun note-off (midi-keys midi-number)
  (midi-key-off (midi->key (typep->value midi-number 'midi-number) midi-keys)))

(defun notes-on (midi-keys)
  (let (notes-on)
    (with-slots (midi->key) midi-keys
      (maphash (lambda (midi midi-key)
                 (declare (ignore midi))
                 (when (> (midi-key-amplitude midi-key) 0)
                   (push midi-key notes-on)))
               midi->key))
    notes-on))

(defun notes-off (midi-keys)
  (with-slots (midi->key) midi-keys
    (maphash (lambda (midi midi-key)
               (declare (ignore midi))
               (when (> (midi-key-amplitude midi-key) 0)
                 (midi-key-off midi-key)))
             midi->key)))


;;;
;;; visual-key
;;;

;;;
;;; tactile-key
;;;
;;; A tactile key is assumed to have 2 y values per key for one x value.
;;; This is to reflect the fact that fingers can press piano keys near (anterior) 
;;; or far (posterior) to key edges closed to the pianist. 
;;;
;;; tactile-xy
;;; x values increase from left to right (lateral).
;;; y values increase from far to near (sagital). 
;;; Each tactile-key has 2 contact zones per key, one near at .8 of key depth, 
;;; and one far at .5 of depth from the back of a key in relation to the pianist.
;;;
;;;
(defconstant +tactile-xy-origin+ #(0 0))

(defconstant +tactile-white-width+ 10)
(defconstant +tactile-white-depth+ 60)
(defconstant +tactile-black-depth+ 40)

(defconstant +tactile-near+ .8
  "About .8 of the key length from the back of the key.")
(defconstant +tactile-far+  .5
  "About .5 of the key length from the back of the key.")

;;; sagital coordinates for white and black keys
(defconstant +tactile-y-off-keyboard+ +tactile-white-depth+)
(defconstant +tactile-y-white-near+ (round (* +tactile-white-depth+ +tactile-near+)))
(defconstant +tactile-y-white-far+ (round (* +tactile-white-depth+ +tactile-far+)))
(defconstant +tactile-y-black-near+ (round (* +tactile-black-depth+ +tactile-near+)))
(defconstant +tactile-y-black-far+ (round (* +tactile-black-depth+ +tactile-far+)))

(defun tactile-xy (midi-number where)
  (->vector 
   (list 
    (x-coor midi-number (x +tactile-xy-origin+) +tactile-white-width+)
    (y-coor (y +tactile-xy-origin+)
            (ecase where
              (:near (if (isa-white-key midi-number)
                         +tactile-y-white-near+ 
                       +tactile-y-black-near+))
              (:far (if (isa-white-key midi-number)
                        +tactile-y-white-far+ 
                      +tactile-y-black-far+)))))))

(defstruct (tactile-key
            (:include visual-key)
            (:constructor make-tactile-key
             (number%)))
  (tactile-near (tactile-xy number% :near))
  (tactile-far (tactile-xy number% :far)))


;;;
;;; tactile-keys
;;;
(defstruct (tactile-keys 
            (:include visual-keys)
            (:constructor make-tactile-keys%))
  (finger->key (make-hash-table :test #'equalp))
  (tactile->key (make-hash-table :test #'equalp)))

(defun make-tactile-keys ()
  (let ((piano-keys (make-tactile-keys%)))
    (with-slots (midi->key visual->key tactile->key) piano-keys
      (fill-midi-keys midi->key #'make-tactile-key)
      (index-hash-table midi->key visual->key 
                        #'visual-key-visual-xy)
      (index-hash-table midi->key tactile->key 
                        #'tactile-key-tactile-near)
      (index-hash-table midi->key tactile->key 
                        #'tactile-key-tactile-far))
    piano-keys))

(defun tactile->key (xy-coor tactile-keys)
  (with-slots (tactile->key) tactile-keys
    (gethash xy-coor tactile->key)))

(defun finger->key (hand-finger tactile-keys)
  (with-slots (finger->key) tactile-keys
    (gethash hand-finger finger->key)))

(defun set-finger->key (hand-finger tactile-keys key)
  (with-slots (hf->key) tactile-keys
    (setf (gethash (list hand finger) hf->key)
          key)))

  

(defun move-finger-over-key (tactile-key hand finger)
  (finger-over-key tactile-key hand finger))

(defun move-finger-off-key (tactile-key hand finger)
  (finger-off-key tactile-key))

; need to track finger up and down see: set-finger-up
; styles should set r and theta when preparing movement. 
; (get-module :device)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;;; Notation properties
;;;
(defconstant +sharp+ (code-char #x266F))
(defconstant +flat+ (code-char #x266D))
(defconstant +white+ (code-char #x266E))
(defun flat (str) (format nil "~A~A" str +flat+))
(defun white (str) (format nil "~A~A" str +white+))
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


;;;
;;; piano-key
;;;
(defstruct (piano-key (:include midi-key)
                      (:constructor make-piano-key
                       (number%
                        &aux
                        (tone% (midi-number->tone number%))
                        (octave% (midi-number->octave number%))
                        (pitch-strings% (tone+octave->pitch-name-strings tone% octave%))
                        )))
  ;; visual properties
  (xy-visual (visual-xy number%))
  (w (w number%))
  (h (d number%))
  ;; tactile properties
  (xy-tactile-near (tactile-xy number% :near))
  (xy-tactile-far (tactile-xy number% :far))
  ;; notation properties
  (pitch-symbols (mapcar #'intern (getf pitch-strings% :combined)))
  ;(xy-visual-near (visual-xy number% :near))
  ;(xy-visual-far (visual-xy number% :far))
  ;(xy-tactile-off (tactile-xy number% :off))
  ;(xy-tactile-near (tactile-xy number% :near))
  ;(xy-tactile-far (tactile-xy number% :far))

  ;(key-octave (key-octave number%))
  ;(key-group (key-group number%))
  ;(key-pos (key-pos number%))
  ;(colour (munber->colour number%))
  ;(pitch-strings pitch-strings%) 
  ;(finger-over +nil+)
  visicon-id
  ;(tone tone%)
  ;(octave octave%)
  )

(defun x-tactile (piano-key)
  (with-slots (xy-tactile-near) piano-key
    (x xy-tactile-near)))

(defun y-tactile (piano-key where)
  (with-slots (xy-tactile-near xy-tactile-far) piano-key
    (ecase where
      (:near xy-tactile-near)
      (:far xy-tactile-far))))
            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; piano
;;;
(defstruct (piano (:constructor make-piano%
                   (&aux
                    (midi->key% 
                     (let ((ht (make-hash-table)))
                       (do ((number +min-midi-number+ (incf number)))
                           ((> number +max-midi-number+) ht)
                         (setf (gethash number ht)
                               (make-piano-key number))))))))
  (name 'piano) 
  (device "piano")
  (version +piano-version+)
  (documentation +piano-documentation+)
  (midi->key midi->key%)
  (vxy->key
   (let ((htable (make-hash-table :test #'equalp)))
     (maphash (lambda (number piano-key)
                (declare (ignore number))
                (setf (gethash (piano-key-xy-visual piano-key)
                               htable) 
                      piano-key))
              midi->key%)
     htable))
  (mxy->key
   (let ((htable (make-hash-table :test #'equalp)))
     (maphash (lambda (number piano-key)
                (declare (ignore number))
                (setf (gethash (piano-key-xy-tactile-near piano-key) htable) 
                      piano-key
                      (gethash (piano-key-xy-tactile-far piano-key) htable)
                      piano-key))
              midi->key%)
     htable))
  (mx->key
   (let ((htable (make-hash-table :test #'equalp)))
     (maphash (lambda (number piano-key)
                (declare (ignore number))
                (setf (gethash (x (piano-key-xy-tactile-near piano-key))
                               htable) 
                      piano-key))
              midi->key%)
     htable))
  (symbol->key 
   (let ((htable (make-hash-table)))
     (maphash (lambda (number piano-key)
                (declare (ignore number))
                (dolist (symbol (piano-key-pitch-symbols piano-key))
                  (setf (gethash symbol htable) piano-key)))
              midi->key%)
     htable))
  )

(defun symbol->key (symbol piano)
  (with-slots (symbol->key) piano
    (gethash symbol symbol->key)))


(defun vxy->key (xy piano)
  (with-slots (vxy->key) piano
    (gethash (->vector xy) vxy->key)))

(defun mxy->key (xy piano)
  (with-slots (mx->key) piano
    (let ((key (gethash (x xy) mx->key)))
      (values 
       key
       (ecase (y xy)
         (:near (piano-key-xy-tactile-near key))
         (:far (piano-key-xy-tactile-far key)))))))




(defun get-piano ()
  (get-component piano))

(defun component-p (name)
  (let ((mp (current-mp)))
    (assoc (->symbol name)
           (bt:with-lock-held ((meta-p-component-lock mp))
             (meta-p-component-list mp)))))

(defun device-p (name)
  (member (->string name) 
          (defined-devices) :test #'equalp))

(defun devlist->interface (devlist) (first devlist))
(defun devlist->device (devlist) (second devlist))


(defun key->value (key assoc-list)
  (second (assoc key assoc-list)))

(defun init-piano (devlist)
   (if (and (equalp (devlist->device devlist) "piano")
           (equalp (devlist->interface devlist) "motor"))
       (setf (device (the-hand 'right)) devlist
             (device (the-hand 'left))  devlist)
    (error "The device list ~S is not (~A ~A)." devlist "piano" "motor")))

(add-act-r-command "init-piano" 'init-piano  "init-piano. Not called directly.")

(defun notify-piano (devlist features)
  (if (and (equalp (devlist->device devlist) "piano")
           (equalp (devlist->interface devlist) "motor"))         
      (let ((piano (get-piano)))
        (if piano
            (let ((function (key->value 'style features)))
              (print (list features function))
              (if (and function (fboundp function))
                  (apply function (list piano ))
                (error "No style associated with feature ~S, or ~S is not a function." 
                       features function)))
          (error "There is no object component for device ~S." (devlist->device devlist))))
    (error "The device list ~S is not (~A ~A)." devlist "piano" "motor")))

(add-act-r-command "notify-piano" 'notify-piano 
                   "Method to notify the piano device/component to apply a function to arguments (features). Not called directly.")

(defun make-piano ()
  (clear-all)
  (when (component-p 'piano) (undefine-component piano))
  (define-component-fct
   'piano :version +piano-version+ :documentation +piano-documentation+
   :creation (lambda () (make-piano%)))
  (unless (device-p "piano")
    (define-device "piano" "init-piano" nil "notify-piano" "init-piano"))
  (get-piano))

; (notify-device '("motor" "piano") '((style describe)))
#|
(progn 
  (make-piano)
  (define-model test)
  (install-device '("motor" "piano"))
  (notify-device '("motor" "piano") '((style describe))))
|#

;(defparameter *default-finger-offset* +tactile-white-width+)

; (chunk-type finger-mouvement key velocity duration offset)
; (defstyle)

(defclass hand-style (dual-hand-movement-style)
  ((hand :initarg :hand :initform 'right :accessor hand)
   (loc :initarg :loc :initform nil :accessor loc)
   (thumb  :initform nil :accessor thumb)
   (index  :initform nil :accessor index)
   (middle :initform nil :accessor middle)
   (ring   :initform nil :accessor ring)
   (pinkie :initform nil :accessor pinkie))
  (:default-initargs
   :two-exec-p nil
   :exec2-time 0
   :finger-based-style t
   :release-if-down nil
   ;:feature-slots '(action hand hand-pos thumb index middle ring pinkie)
   :style-name 'hand-style))

(defun request-slots (chunk-spec)
  (act-r-chunk-spec-slots chunk-spec))

(defun request-slot (request-slots slot-name)
  (find slot-name request-slots :key #'act-r-slot-spec-name))

(defun request-slot-value (request-slots slot-name)
  (let ((request-slot (request-slot request-slots slot-name)))
    (if request-slot 
        (act-r-slot-spec-value request-slot)
      +actr-nil+)))

(defmethod initialize-instance :after ((style hand-style) &rest initargs &key &allow-other-keys)
  (let* ((request-spec (getf initargs :request-spec))
         (slots (when request-spec (request-slots request-spec))))
    (setf (thumb style)  (request-slot-value slots 'thumb)
          (index style)  (request-slot-value slots 'index)
          (middle style) (request-slot-value slots 'middle)
          (ring style)   (request-slot-value slots 'ring)
          (pinkie style) (request-slot-value slots 'pinkie)
          )))

(defmethod feat-differences ((hs1 hand-style) (hs2 hand-style)) 0)
(defmethod compute-exec-time ((m motor-module) (style hand-style)) 0.0)

(defclass hand-move (hand-style)
  ()
  (:default-initargs
   :feature-slots '(hand loc)
   :style-name 'hand-move))

(defmethod hand-move ((module pm-module) &key hand loc request-spec)
  (unless (or (check-jam module) (check-specs 'hand-move hand loc))
    (prepare-movement 
     module
     (make-instance 'hand-move :request-spec request-spec 
                    :hand hand :loc loc))))

(extend-manual-requests (hand-move hand loc thumb index) handle-style-request)
; (remove-manual-request hand-move)

(defun exec-hand-move (params)
  (pprint params))

(defmethod queue-output-events ((m motor-module) (style hand-move))
  (schedule-event-relative 
   (exec-time style) 'exec-hand-move :module :motor
   :params `((hand-move ,(hand style) ,(loc style) ,(thumb style) ,(index style)))))

(clear-all)
(define-model foo
  (sgp :trace-detail high)
  ;(chunk-type hand-move hand loc thumb)
  (p p1
     ?manual>
     state free
     ==> 
     +manual> 
     isa hand-move
     hand right
     loc (10 10)
     thumb 'test
     )
  )






(defstruct (finger-mouvement
            (:conc-name figmvt-))
  (key      +actr-nil+)
  (velocity +actr-nil+)
  (duration +actr-nil+)
  ;(offset   +actr-nil+)
  )

(defun make-fig-mvt (&optional finger-spec (default-velocity +actr-nil+) 
                                    (default-duration +actr-nil+))
  (etypecase finger-spec
    (null   (make-finger-mouvement))
    (symbol (make-finger-mouvement :key      finger-spec
                              :velocity default-velocity
                              :duration default-duration))
    (list   (let ((plist (cons :key finger-spec)))
              (make-finger-mouvement :key      (getf plist :key)
                                :velocity (getf plist :velocity default-velocity)
                                :duration (getf plist :duration default-duration))))))

(assert (equalp (make-fig-mvt) (make-finger-mouvement)))
(assert (equalp (make-fig-mvt 'c4) (make-finger-mouvement :key 'c4)))
(assert (equalp (make-fig-mvt '(c4 :velocity 100 :duration 4)) 
                (make-finger-mouvement :key 'c4 :velocity 100 :duration 4)))
(assert (equalp (make-fig-mvt 'c4 100 4) 
                (make-finger-mouvement :key 'c4 :velocity 100 :duration 4)))
(assert (equalp (make-fig-mvt '(c4) 100 4) 
                (make-finger-mouvement :key 'c4 :velocity 100 :duration 4)))
(assert (equalp (make-fig-mvt '(c4 :velocity 100 :duration 4) 50 1) 
                (make-finger-mouvement :key 'c4 :velocity 100 :duration 4)))

 
(defclass hand-style (dual-hand-movement-style)
  ((hand :initarg :hand :initform 'right)
   (velocity :initarg :velocity :initform +actr-nil+)
   (duration :initarg :duration :initform +actr-nil+)
   (hand-pos :initarg :hand-pos :initform +actr-nil+)
   (thumb  :initform (make-finger-event))
   (index  :initform (make-finger-event))
   (middle :initform (make-finger-event))
   (ring   :initform (make-finger-event))
   (pinkie ::initform (make-finger-event)))
  (:default-initargs
   :two-exec-p nil
   :exec2-time 0
   :finger-based-style t
   :release-if-down nil
   ;:feature-slots '(action hand hand-pos thumb index middle ring pinkie)
   :style-name 'hand-event))

(defun make-hand-event (hand velocity duration &key thumb index middle ring pinkie)
  (let ((hand-event (make-instance 'hand-event :hand hand
                                   :velocity velocity :duration duration)))
    (if (or thumb index middle ring pinkie)
        (progn 
          (when thumb  (setf (slot-value hand-event 'thumb)
                             (get-finger-property thumb velocity duration)))
          (when index  (setf (slot-value hand-event 'index)
                             (get-finger-property index velocity duration)))
          (when middle (setf (slot-value hand-event 'middle)
                             (get-finger-property middle velocity duration)))
          (when ring   (setf (slot-value hand-event 'ring)
                             (get-finger-property ring velocity duration)))
          (when pinkie (setf (slot-value hand-event 'pinkie)
                             (get-finger-property pinkie velocity duration))))
      (error "At least one finger needs to be specified in a hand-event."))
    hand-event))

(defmethod hand-event ((module pm-module) &key (action 'dummy-action) (hand 'right) (hand-pos +actr-nil+) 
                       (velocity +actr-nil+) (duration +actr-nil+) (thumb +actr-nil+) (index +actr-nil+) (middle +actr-nil+) 
                       (ring +actr-nil+) (pinkie +actr-nil+) request-spec)
  (unless (or (check-jam module) (check-specs 'hand-event action hand hand-pos thumb index middle ring pinkie))
    (prepare-movement
     module
     (make-hand-event hand 
     (make-instance 'piano-hand :request-spec request-spec
                    :action action :hand hand :hand-pos hand-pos 
                    :thumb thumb :index index :middle middle :ring ring 
                    :pinkie pinkie)))))
 


(deftype key-name-symbol () 'symbol)
(deftype key-name-string () 'string)
(deftype velocity () +velocity-type+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct hand-xy which-hand hand thumb index middle ring pinkie)

(defun out-of-keyboard (xy)
  (etypecase xy
    (vector-2 (vector (x xy) +tactile-y-off-keyboard+))
    (number (vector xy +tactile-y-off-keyboard+))))

(defun calculate-offset (source destination)
  (vector (- (x destination) (x source))
          (- (y destination) (y source))))

(defun hand-xy-offset (hand-xy)
  (with-slots (hand thumb index middle ring pinkie) hand-xy
    (setf thumb (calculate-offset hand thumb)
          index (calculate-offset hand index)
          middle (calculate-offset hand middle)
          ring (calculate-offset hand ring)
          pinkie (calculate-offset hand pinkie))
    hand-xy))
                                

(defun hands-at-piano (piano)
  (values
   (hand-xy-offset
    (make-hand-xy :which-hand 'right 
                  :hand   (out-of-keyboard (piano-key-xy-tactile-near (symbol->key 'e4 piano)))
                  :thumb  (out-of-keyboard (piano-key-xy-tactile-near (symbol->key 'c4 piano)))
                  :index  (out-of-keyboard (piano-key-xy-tactile-near (symbol->key 'd4 piano)))
                  :middle (out-of-keyboard (piano-key-xy-tactile-near (symbol->key 'e4 piano)))
                  :ring   (out-of-keyboard (piano-key-xy-tactile-near (symbol->key 'f4 piano)))
                  :pinkie (out-of-keyboard (piano-key-xy-tactile-near (symbol->key 'g4 piano)))))
   (hand-xy-offset
    (make-hand-xy :which-hand 'left 
                  :hand   (out-of-keyboard (piano-key-xy-tactile-near (symbol->key 'a3 piano)))
                  :thumb  (out-of-keyboard (piano-key-xy-tactile-near (symbol->key 'c4 piano)))
                  :index  (out-of-keyboard (piano-key-xy-tactile-near (symbol->key 'b3 piano)))
                  :middle (out-of-keyboard (piano-key-xy-tactile-near (symbol->key 'a3 piano)))
                  :ring   (out-of-keyboard (piano-key-xy-tactile-near (symbol->key 'g3 piano)))
                  :pinkie (out-of-keyboard (piano-key-xy-tactile-near (symbol->key 'f3 piano)))))))





|#
#|

(defclass piano-hand (dual-hand-movement-style)
  ((action :initarg :action :accessor action :initform 'dummy-action)
   (hand :initarg :hand :accessor hand :initform 'right)
   (hand-pos :initarg :hand-pos :accessor hand-pos :initform +actr-nil+)
   (thumb :initarg :thumb :accessor thumb :initform +actr-nil+)
   (index :initarg :index :accessor index :initform +actr-nil+)
   (middle :initarg :middle :accessor middle :initform +actr-nil+)
   (ring :initarg :ring :accessor ring :initform +actr-nil+)
   (pinkie :initarg :pinkie :accessor pinkie :initform +actr-nil+))
  (:default-initargs
   :two-exec-p nil
   :exec2-time 0
   :finger-based-style t
   :release-if-down nil
   ;:feature-slots '(action hand hand-pos thumb index middle ring pinkie)
   :style-name 'piano-hand))

(extend-manual-requests (piano-hand action hand) handle-style-request)

(defstyle hands-to-piano piano-hand)

(extend-manual-requests (hands-to-piano) handle-style-request)

(defmethod piano-hand ((module pm-module) &key (action 'dummy-action) (hand 'right) (hand-pos +actr-nil+) 
                       (thumb +actr-nil+) (index +actr-nil+) (middle +actr-nil+) 
                       (ring +actr-nil+) (pinkie +actr-nil+) request-spec)
  (unless (or (check-jam module) (check-specs 'piano-hand action hand hand-pos thumb index middle ring pinkie))
    (prepare-movement
     module
     (make-instance 'piano-hand :request-spec request-spec
                    :action action :hand hand :hand-pos hand-pos 
                    :thumb thumb :index index :middle middle :ring ring 
                    :pinkie pinkie))))


;;; piano-hand preparation parameters and methods.
;;;
;;; The default prep time for features is set to 0.05 per feature in the motor module.
;;; There is no need to change this default time value. 
;;; The total preparation time = feature count * module feature-prep-time (0.050)
;;; 

(defconstant +piano-hand-features+ '(hand hand-pos thumb index middle ring pinkie))

(defun count-features (style1 style2)
  (let ((count 0))
    ;; adding one to the feature change cound count for every different values
    (dolist (feature +piano-hand-features+ count)
      (unless (equalp (slot-value style1 feature) (slot-value style2 feature))
        (incf count)))))

(defmethod feat-differences ((style1 piano-hand) (style2 piano-hand))
  "This is a generic method assuming only *piano-hand-features* in a style. The features could be added in a style specific method."
  (count-features style1 style2))

;;; piano-hand initiation parameters and methods.
;;;
;;; The compute-exec-time method is called to determine
;;; how long after preparation of the style's action it will take
;;; until the action should be 'executed'.  It is called every
;;; time there is a request for the action and thus can change
;;; dynamically based on the features. However, for piano-hand styles
;;; it is assumed to be the same for every inherited styles. 

(defparameter *default-compute-exec-time* 0.05
  "Minimum time before execution can proceed after preparation is completed.")

(defmethod compute-exec-time ((m motor-module) (style piano-hand))
  ;; set as the value of the minimum compute execution time parameter. 
  (declare (ignore m style))
  *default-compute-exec-time*)

;;; The compute-finish-time method is called to determine how long
;;; after the preparation of the style's action it will take until
;;; the execution stage should complete and become free again.
;;; It must return a number which is the total execution stage
;;; time in seconds.  This is called after the exec-time is set.

;;; After calling compute-finish-time the style will set the 
;;; finish-time slot of the style to the result.

(defparameter *default-compute-finish-time* 0.05
  "Minimum finish time.")

(defmethod compute-finish-time ((m motor-module) (style piano-hand))
  (declare (ignore m))
  (+ *default-compute-finish-time* (exec-time style)))

;;;;;;;;;;;;;;;;;;
;;;
;;; Execution
(defun dummy-action (params)
  (print (list 'dummy-action params)))

(defmethod queue-output-events ((m motor-module) (style piano-hand))
  (schedule-event-relative 
   (exec-time style) (action style) :params nil))

(defmethod queue-output-events ((m motor-module) (style hands-to-piano))
  (schedule-event-relative 
   (exec-time style) 'dummy-action :params '(hands-to-piano)))

|#
#|
(make-piano)

(clear-all)

(define-model foo 
  (install-device '("motor" "piano"))
  (sgp :trace-detail high)
  ;(chunk-type piano-hand)
  (define-chunks dummy-action)
  
  (p p1
     ?manual>
     state free
     ==> 
     +manual> 
     isa hands-to-piano
     ))
|#

#|


(defun version (piano &key features)
  (declare (ignore features))
  (piano-version piano))

; (notify-piano '("piano" "motor") '((style version)))


(defun keys-group-width (nb-blacks-in-group)
  (ecase nb-blacks-in-group
    (2 (* 3 (mm->pixels +white-width+)))
    (3 (* 4 (mm->pixels +white-width+)))))

(defun nb-whites-width (nb-whites)
  (* nb-whites (mm->pixels +white-width+)))

; midi-number->nb-blacks-in-group

(defun black-from-before (midi-number)
  (let* ((nb-blacks (midi-number->black-keys-in-group midi-number))
         (grp-length (* (mm->pixels +white-width+) (1+ nb-blacks)))
         (black-unit (/ grp-length (case nb-blacks (2 5) (3 7))))
         (pos-in-grp (midi-number->pos-in-group midi-number)))
    (case pos-in-grp
      (1 (round black-unit))
      (2 (round (- (* 3 black-unit) (mm->pixels +white-width+))))
      (3 (round (- (* 5 black-unit) (* 2 (mm->pixels +white-width+))))))))

(defun visual-x (midi-number)
  (+ *x-origin*
     (* (count-white-keys-before midi-number)
        (mm->pixels +white-width+))
     (if (isa-white-key midi-number)
         +visual-x-white-offset+
       (black-from-before midi-number))))

(defun visual-y (midi-number where)
  (+ *y-origin* ; layout y origin
     (ecase where
       (:near (if (isa-white-key midi-number) 
                  +visual-y-white-near+ 
                +visual-y-black-near+))
       (:far (if (isa-white-key midi-number)
                 +visual-y-white-far+ 
               +visual-y-black-far+)))))
       
(defun visual-xy (midi-number where)
  (vector (visual-x midi-number) 
          (visual-y midi-number where)))


                                                   

|#

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; actr motor module hands
;;;
(defun motor-module ()
  (get-module :motor))

(defun the-hand (hand-name)
  "Returns the motor module hand instance."
  (ecase hand-name
    (left (left-hand (motor-module)))
    (right (right-hand (motor-module)))))
|#



#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; constants, parameters and types
;;;


;;; empty value for actr chunk slot values
(defconstant +nil+ 'none)

;;; midi numbers
(defconstant +min-midi-number+ 21)  ; F0
(defconstant +max-midi-number+ 108) ; C8
(defconstant +midi-number-type+ 
  `(integer ,+min-midi-number+ ,+max-midi-number+))
(defconstant +C4-number+ 60)
(defconstant +A4-number+ 69)

;;; midi number utilities
(defun number->tone (number)
  "Octave tone, from 1 to 12."
  (1+ (mod number 12)))

(defun number->octave (number)
  "An octave number, given a midi number value."
  (- (floor number 12) 1))

(defun number->pos-in-group (number)
  "The position of a key in a group by type (white or black), given a midi number."
  (let ((tone (number->tone number)))
    (cond ((member tone '(1 2 6 7)) 1)
          ((member tone '(3 4 8 9)) 2)
          ((member tone '(5 10 11)) 3)
          ((eq tone 12)  4))))

(defun isa-white-key (number)
  "Is the midi number value a white tone."
  (member (number->tone number) 
          '(1 3 5 6 8 10 12)))

(defun count-white-keys-before (number)
  "Counts the number of white keys before a given midi number."
  (do* ((i +min-midi-number+ (1+ i))
        (isa-white-key (isa-white-key i)
                     (isa-white-key i))
        (nb-whites (if isa-white-key 1 0)
                     (if isa-white-key (1+ nb-whites) nb-whites)))
       ((eq i number) (1- nb-whites))))

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
(defconstant +x-white-offset+ 10)
(defconstant +x-black-offset+ 5)
(defconstant +y-off+ 0)
(defconstant +y-white-near+ 10)
(defconstant +y-white-far+ 20)
(defconstant +y-black-near+ 20)
(defconstant +y-black-far+ 30)

(defun xy-virtual (number where)
  (let ((isa-white-key (isa-white-key number)))
    (vector (+ (* (1+ (count-white-keys-before number))
                +x-white-offset+)
             (if isa-white-key 0 +x-black-offset+))  
          (ecase where
            (:off +y-off+)
            (:near (if isa-white-key +y-white-near+ +y-black-near+))
            (:far (if isa-white-key +y-white-far+ +y-black-far+))))))




(defconstant +white-colour+ 'white)
(defconstant +black-colour+ 'black)

(defun munber->colour (number)
  (if (isa-white-key number)
      +white-colour+
    +black-colour+))

;;; piano key sizes in mm
(defconstant +white-width+      23) 
(defconstant +white-depth+    150)
(defconstant +black-width+   11)
(defconstant +black-depth+  95)

(defun w (number)
  (if (isa-white-key number) +white-width+ +black-width+))

(defun h (number)
  (if (isa-white-key number) +white-depth+ +black-depth+))


(defun midi-number->nb-blacks-in-group (midi-number)
  "The number of black keys in a group -> 2 or 3."
  (if (< (midi-number->tone midi-number) 6)
      2 3))

(defun group (midi-number)
  (intern 
   (format nil "~S~S" 
           +black-colour-prefix+
           (midi-number->nb-blacks-in-group midi-number))))

(defun midi-number->pos-in-group (midi-number)
  "The position of a key in a group by type (white or black), given a midi number."
  (let ((tone (midi-number->tone midi-number)))
    (cond ((member tone '(1 2 6 7)) 1)
          ((member tone '(3 4 8 9)) 2)
          ((member tone '(5 10 11)) 3)
          ((eq tone 12)  4))))

(defun group-pos (midi-number)
  "key colour and position in group"
  (intern 
   (format nil "~S~S" 
           (if (isa-white-key midi-number) 
               +white-colour-prefix+ 
             +black-colour-prefix+)
           (midi-number->pos-in-group midi-number))))


;;; Key coordiantes as pressing touch points.
;;; Y touch oordinates are distance to the pianist. 
;;; Piano key y coordinate is the back of the key. 
;;; Near to pianist = higher y value. 
;;; Far to pianist = lower y value. 
(defconstant +near+ .8) ; 80% of the key length from the back.
(defconstant +far+  .5) ; 50% of the key length from the back.
(defconstant +y-white-touch-far+     (* +white-depth+ +far+))
(defconstant +y-white-touch-near+    (* +white-depth+ +near+))
(defconstant +y-black-touch-far+  (* +black-depth+ +far+))
(defconstant +y-black-touch-near+ (* +black-depth+ +near+))
;;; x touch coordinates (middle of key width).
(defconstant +x-white-touch+    (/ +white-width+ 2))
(defconstant +x-black-touch+ (/ +black-width+ 2))
;;; x coordinates of black keys depends on their group type
(defconstant +black-grp2-spacing+ (/ (* +white-width+ 3) 5))
(defconstant +black-grp3-spacing+ (/ (* +white-width+ 4) 7))
;;; Origin of piano keys layout
;;; x left most position of keys.
;;; y top or back position of all keys. 
(defparameter *x-origin* 0)
(defparameter *y-origin* 0)

(defun x-touch (number)
  (if (isa-white-key number) +x-white-touch+ +x-black-touch+))

(defun x-to-black (black-pos-in-group spacing)
  (- (* (ecase black-pos-in-group (1 1) (2 3) (3 5)) 
        spacing)
     (* (1- black-pos-in-group) +white-width+)))

(defun number->key-group (number)
  "The black keys group, given a midi number."
  (if (< (number->tone number) 6)
      :2-black-keys 
    :3-black-keys))

(defun x-relative-to-white-before (number)
  (if (isa-white-key number) 0 
    (ecase (number->key-group number)
      (:2-black-keys 
       (x-to-black (number->pos-in-group number) 
                        +black-grp2-spacing+))
      (:3-black-keys
       (x-to-black (number->pos-in-group number) 
                        +black-grp3-spacing+)))))

(defun key-x (number)
  (+ *x-origin* ; layout x origin
     (* (count-white-keys-before number) +white-width+) ; all white widths before current key
     (x-touch number) ; about half the key width
     (if (isa-white-key number) 0 ; nothing else to add for a white key
        (x-relative-to-white-before number)) ; add rest to black key
     ))

(defun y-touch (number where)
  (ecase where
    (:near (if (isa-white-key number) +y-white-touch-near+ +y-black-touch-near+))
    (:far (if (isa-white-key number) +y-white-touch-far+ +y-black-touch-far+))))

(defun key-y (number where)
  (+ *y-origin* ; layout y origin
     (y-touch number where) ; near or far the pianist hand
     ))

;;; Notation properties
;;;
(defconstant +sharp+ (code-char #x266F))
(defconstant +flat+ (code-char #x266D))
(defconstant +white+ (code-char #x266E))
(defun flat (str) (format nil "~A~A" str +flat+))
(defun white (str) (format nil "~A~A" str +white+))
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



;;; scalling mm to unit
;;; Assuming a coordinate system of about 2,3 mm per unit
;;; using the white width as a reference
(defconstant +mm/unit+ 
  (/ +white-width+ 10))

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
    
|#

;;; eof