;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;;
;;; midi-utilities.lisp
;;;
;;; 2025-10-23
;;;
(defconstant +A4-hertz+ 440)
(defconstant +A4-midi+ 69)
(defconstant +C4-midi+ 60)

(defconstant +flat+ (code-char #x266D))
(defconstant +natural+ (code-char #x266E))
(defconstant +sharp+ (code-char #x266F))
(defun flat (str) (format nil "~A~A" str +flat+))
(defun natural (str) (format nil "~A~A" str +natural+))
(defun sharp (str) (format nil "~A~A" str +sharp+))

(defun midi->tone (midi)
  (1+ (mod midi 12)))

(defun midi->octave (midi)
  (- (floor midi 12) 1))

(defun isa-natural (midi)
  (member (midi->tone midi) 
          '(1 3 5 6 8 10 12)))

(defun nb-naturals (midi-start midi-end)
  (let ((nb-naturals 0))
    (do* ((midi midi-start (incf midi)))
         ((eq midi (1+ midi-end)) nb-naturals)
      (when (isa-natural midi)
        (incf nb-naturals)))))
          
(defun midi->pitch-class-strings (midi &optional (notation :combined))
  "Returns the name of a midi number. Optional notations include :scientific (string), :lilypond (string), or :combined (scientific: keep capital name; and lilypond: ekkp es is instead of charp and flat characters)."
  (getf (case (midi->tone midi)
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
                :combined   ("B"))))
        notation))

(defun midi->pitch-strings (midi &optional (notation :combined))
  (let ((octave (midi->octave midi)))
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
     (midi->pitch-class-strings midi notation))))



#|
(defparameter *key-color-natural* 'white)
(defparameter *key-color-accidendal* 'black)
(defparameter *group-2-accidentals* 'b2)
(defparameter *group-3-accidentals* 'b3)

;;;
;;; utilities
;;;
(defun midi->hertz (midi)
  (* +A4-hertz+ (expt 2 (/ (- midi +A4-midi+) 12))))







(defun strings->symbols (strings)
  (mapcar (lambda (string) (intern (string-upcase string)))
          strings))

(defun midi->pitch-class-symbols (midi &optional (notation :combined))
  (strings->symbols (midi->pitch-class-strings midi notation)))

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

(defun midi->color (midi)
  (let ((tone (midi->tone midi)))
    (cond ((member tone '(1 3 5 6 8 10 12)) 
           *key-color-natural*)
          ((member tone '(2 4 7 9 11)) 
           *key-color-accidendal*))))

(defun midi->octave-pos (midi)
  (midi->tone midi))

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
          

(defun natural-p (midi)
  (member (midi->tone midi) 
          '(1 3 5 6 8 10 12)))

(defun accidental-p (midi)
  (member (midi->tone midi) 
          '(2 4 7 9 11)))

(defun nb-naturals (midi-start midi-end)
  (do* ((midi midi-start (incf midi))
        (nb-naturals (if (natural-p midi) 1 0)
                     (if (natural-p midi) (incf nb-naturals) nb-naturals)))
       ((eq midi midi-end) nb-naturals)))

(defun naturals-left (pitch n)
  (when (> n 0)
    (do* ((midi (1- (pitch->midi pitch)) (1- midi))
          (isa-natural (natural-p midi) (natural-p midi))
          (naturals (when isa-natural (midi->pitch-symbols midi))
                    (append (when isa-natural (midi->pitch-symbols midi)) naturals))
          (i (if isa-natural 1 0) 
             (if isa-natural (incf i) i)))
         ((eq i n) naturals))))

(defun naturals-right (pitch n)
  (when (> n 0)
    (do* ((midi (1+ (pitch->midi pitch)) (1+ midi))
          (isa-natural (natural-p midi) (natural-p midi))
          (naturals (when isa-natural (midi->pitch-symbols midi))
                    (append naturals (when isa-natural (midi->pitch-symbols midi))))
          (i (if isa-natural 1 0) 
             (if isa-natural (incf i) i)))
         ((eq i n) naturals))))

|#

;;; eof