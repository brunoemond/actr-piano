;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;;
;;; midi-utilities.lisp
;;;
;;; 2026-04-08
;;;

;;; types
(defun check-type% (value type)
  "A type checking utility which returns the tested value if it is of the correct type."
  (assert (typep value type) (value) "Expected ~S, got ~S" type value)
  value)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Piano and piano-key properties
;;;
;;; midi
(defconstant +min-midi+ 21) ; F0
(defconstant +max-midi+ 108) ; C8
(defconstant +midi-value-type+
  `(integer ,+min-midi+ ,+max-midi+))
(defconstant +A4-midi+ 69)
(defconstant +C4-midi+ 60)

(defun midi->tone (midi)
  "Octave tones, from 1 to 12."
  (1+ (mod midi 12)))

(defun midi->octave (midi)
  "An octave number, given a midi value."
  (- (floor midi 12) 1))

(defun isa-natural (midi)
  "Is the midi value a natural tone."
  (member (midi->tone midi) 
          '(1 3 5 6 8 10 12)))

;;; sounds / hertz
(defconstant +A4-hertz+ 440.0)

(defun midi->hertz (midi)
  (* +A4-hertz+ (expt 2 (/ (- midi +A4-midi+) 12))))

;;; key colours
(defconstant +natural-colour+ 'white)
(defconstant +accidental-colour+ 'black)

;;; piano key sizes
;;; Assuming a coordinate system of about 2,3 mm per unit
(defconstant +mm/unit+ 2.3)
(defconstant +natural-width+     (floor (/  23 +mm/unit+)))  ;;  23 mm
(defconstant +natural-height+    (floor (/ 150 +mm/unit+)))  ;; 150 mm
(defconstant +accidental-width+  (floor (/  11 +mm/unit+)))  ;;  11 mm
(defconstant +accidental-height+ (floor (/  95 +mm/unit+)))  ;;  95 mm
(defconstant +natural-size+ (vector +natural-width+ +natural-height+))
(defconstant +accidental-size+ (vector +accidental-width+ +accidental-height+))

;;; Key coordiantes as pressing touch points.
;;; Y coordinate, near the pianist are higher y values.
(defconstant +accidental-touch-far+  
  (floor (* +accidental-height+ .5)))
(defconstant +accidental-touch-near+ 
  (floor (* +accidental-height+ .8)))
(defconstant +natural-touch-far+
  (floor (* +accidental-height+ .5)))
(defconstant +natural-touch-near+ 
  (floor (* +accidental-height+ .8)))

(defparameter *piano-x* 0)
(defparameter *piano-y* 0)

(defun y-coordinate (touch-y)
  (+ *piano-y* touch-y))

;;; X coordinate, left-right
(defun count-naturals-before (min-midi midi)
  (do* ((current-midi min-midi (1+ current-midi))
        (isa-natural (isa-natural current-midi)
                     (isa-natural current-midi))
        (nb-naturals (if isa-natural 1 0)
                     (if isa-natural (1+ nb-naturals) nb-naturals)))
       ((eq current-midi midi) 
        (1- nb-naturals))))

(defun x-coordinate (min-midi midi)
  (+ *piano-x*
     (/ +natural-width+ 2)
     (* (count-naturals-before min-midi midi) +natural-width+)
     (if (isa-natural midi) 0 (/ +natural-width+ 2))))

(defun xy-coordinate (min-midi midi)
  (let ((x (x-coordinate min-midi midi)))
    (if (isa-natural midi)
        (list (vector x (y-coordinate +natural-near-touch+))
              (vector x (y-coordinate +natural-far-touch+)))
      (list (vector x (y-coordinate +accidental-touch-far+))
            (vector x (y-coordinate +accidental-touch-near+))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; piano key
;;;
(defstruct (key 
            (:constructor make-key
             (midi
              &aux 
              (midi% (check-type% midi +midi-value-type+))
              (xy-set (xy-coordinate +min-midi+ midi%))
              (hertz (midi->hertz midi%))
              (tone (midi->tone midi%))
              (octave (midi->octave midi%))
              (pitch-name-strings (tone+octave->pitch-name-strings tone octave))
              (colour (if (isa-natural midi%) +natural-colour+ +accidental-colour+))
              (size (if (isa-natural midi%) +natural-size+ +accidental-size+)))))
  (midi +A4-midi+ :type midi-key) 
  xy-set
  pitch-name-strings
  hertz
  tone
  octave
  colour
  size)

(defun all-pitch-name-strings (key)
  (check-type key key)
  (let (all-names (pitch-name-strings (key-pitch-name-strings key)))
    (dolist (notation '(:scientific :lilypond :combined) all-names)
      (dolist (name (getf pitch-name-strings notation))
        (setf all-names (adjoin name all-names :test #'equalp))))))
    

(defun pitch-class-strings (key &optional (notation :combined))
  (check-type key key)
  (mapcar (lambda (name) 
            (subseq name 0 (1- (length name))))
          (getf (key-pitch-name-strings key) notation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; piano
;;;

(defstruct (piano 
            (:constructor make-piano
             (&aux 
              (midi->key 
               (let ((htable (make-hash-table)))
                 (do ((midi +min-midi+ (1+ midi)))
                     ((> midi +max-midi+) htable)
                   (setf (gethash midi htable)
                         (make-key midi)))))
              (pitch-string->key 
               (let ((htable (make-hash-table :test #'equalp)))
                 (maphash (lambda (midi key)
                            (declare (ignore midi))
                            (dolist (pitch-string (all-pitch-name-strings key))
                              (setf (gethash pitch-string htable) key)))
                          midi->key)
                 htable))
              (pitch-symbol->key 
               (let ((htable (make-hash-table)))
                 (maphash (lambda (midi key)
                            (declare (ignore midi))
                            (dolist (pitch-string (getf (key-pitch-name-strings key) :combined))
                              (setf (gethash (intern pitch-string) htable) key)))
                          midi->key)
                 htable))
              (xy->key 
               (let ((htable (make-hash-table :test #'equalp)))
                 (maphash (lambda (midi key)
                            (declare (ignore midi))
                            (dolist (xy (key-xy-set key))
                              (setf (gethash xy htable) key)))
                          midi->key)
                 htable))
              (x->key 
               (let ((htable (make-hash-table)))
                 (maphash (lambda (midi key)
                            (declare (ignore midi))
                            (setf (gethash (svref (car (key-xy-set key)) 0) htable)
                                  key))
                          midi->key)
                 htable)))))                
  midi->key
  pitch-string->key
  pitch-symbol->key
  xy->key
  x->key)

;(defun set-keys-size (piano)
  

(defun midi->key (midi piano)
  (check-type midi midi-key)
  (check-type piano piano)
  (gethash midi (piano-midi->key piano)))

(defun pitch-string->key (pitch-string piano)
  (check-type pitch-string string)
  (check-type piano piano)
  (gethash pitch-string (piano-pitch-string->key piano)))

(defun pitch->key (pitch-symbol piano)
  (check-type pitch-symbol symbol)
  (check-type piano piano)
  (gethash pitch-symbol (piano-pitch-symbol->key piano)))

(defun xy->key (xy piano)
  (check-type xy (simple-vector 2))
  (check-type piano piano)
  (gethash xy (piano-xy->key piano)))

(defun x->key (x piano)
  (check-type x integer)
  (check-type piano piano)
  (gethash x (piano-x->key piano)))


#|
For visual encoding of the keyboard. 
To use eventually

(defun midi->group-type (midi)
  (let ((tone (midi->tone midi)))
    (cond ((member tone '(1 2 3 4 5)) 
           *group-2-accidentals*)
          ((member tone '(6 7 8 9 10 11 12)) 
           *group-3-accidentals*))))

(defun midi->group-pos (midi)
  (let ((tone (midi->tone midi)))
    (cond ((member tone '(1 2 6 7)) 1)
          ((member tone '(3 4 8 9)) 2)
          ((member tone '(5 10 11)) 3)
          ((eq tone 12)  4))))


(defun midi->key-group (midi)
  (intern 
   (format nil "~S~S"
           (case (midi->color midi)
             (white 'w) (black 'b))
           (midi->group-pos midi))))

(defun midi->vis-group (midi)
  (intern 
   (format nil "~S~S-~S"
           (case (midi->color midi)
             (white 'w) (black 'b))
           (midi->group-pos midi)
           (midi->group-type midi))))

(defun midi->octave-group (midi)
  (intern (format nil "O~S" (midi->octave midi))))


(defun midi->color-pos (midi)
  (let ((tone (midi->tone midi)))
    (case tone
      (1 1) (2 1) (3 2) (4 2) (5 3)
      (6 4) (7 3) (8 5) (9 4) (10 6)
      (11 5) (12 7))))



(defun midi->pitch-symbols (midi &optional (notation :combined))
  (strings->symbols (midi->pitch-strings midi notation)))

(defparameter *pitch-midi-table* 
  (let ((midi-start 21) ; A0
        (midi-end 108) ; C8
        (htable (make-hash-table)))
    (do ((i midi-start (incf i)))
        ((> i midi-end) htable)
      (dolist (pitch (midi->pitch-symbols i))
        (setf (gethash pitch htable) i)))))

(defun pitch-names (&optional (start 21) (end 108))
  (let (pitch-names)
    (maphash (lambda (name midi)
               (when (and (>= midi start)
                          (<= midi end))
                 (setf pitch-names (append pitch-names (list name)))))
             *pitch-midi-table*)
    pitch-names))

(defun pitch->midi (pitch)
  (gethash pitch *pitch-midi-table*))

(defun pitch-from-interval (pitch interval &optional (accidental 'is))
  (let ((pitch-symbols (midi->pitch-symbols (+ (pitch->midi pitch) interval))))
    (case accidental
      (is (first pitch-symbols))
      (es (second pitch-symbols)))))

(defun prime->triad (prime &optional (mode 'major))
  (case mode
    (major 
     (values prime
             (pitch-from-interval prime 4)
             (pitch-from-interval prime 7)))
    (minor 
     (values prime
             (pitch-from-interval prime 3 'es)
             (pitch-from-interval prime 7)))))
          
|#

;;; eof