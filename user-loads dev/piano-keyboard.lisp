;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; piano-keyboard.lisp
;;;
;;; 2025-10-15
;;;
;;;

(defconstant +piano-key-colors+ '(white black))
(defconstant +w.5+ .5
  "Constant for the x coordinate of keys. Middle of width.")
(defconstant +h.75+ .75
  "Constant for the y coordinate of keys. 75% of the length / height.")

(defclass piano-keyboard (visicon-object) 
  (;; midi range
   (midi-start :type integer :initform 21 :initarg :midi-start :reader midi-start 
               :documentation "21->A0")
   (midi-end :type integer :initform 108 :initarg :midi-end :reader midi-end 
             :documentation "108->C8") 
   ;; colours
   (natural-key-color :type (member-of +piano-key-colors+) :initform 'white :initarg :natural-key-color :reader natural-key-color)
   (accidental-key-color :type (member-of +piano-key-colors+) :initform 'black :initarg :accidental-key-color :reader accidental-key-color)
   ;key width and height
   (natural-key-w :type number :initform 23.5 :initarg :natural-key-w :reader natural-key-w
                  :documentation "A physical key width of 23.5mm.")
   (natural-key-h :type number :initform 150 :initarg :natural-key-h :reader natural-key-h
                  :documentation "A physical key height of 150mm.")
   (accidental-key-w :type number :initform 13.7 :initarg :accidental-key-w :reader accidental-key-w
                     :documentation "A physical key width of 13.7mm.")
   (accidental-key-h :type number :initform 95 :initarg :accidental-key-h :reader accidental-key-h
                     :documentation "A physical key height of 95mm.")
   ;; key touch points, xy coordinate templates used to determine specific key coordiantes. 
   (natural-key-xy :type xy-coordinate :initform '(12 125) :initarg :natural-key-xy :reader natural-key-xy
                   :documentation "About half its width (23.5), and 125mm from its y orign, 25mm from the bottom.")
   (accidental-key-xy :type xy-coordinate :initform '(7 85) :initarg :accidental-key-xy :reader accidental-key-xy
                   :documentation "About half its width (13.7), and and near its bottom (95).")
   ;; key indexes
   (midi->key :type hash-table :initform (make-hash-table) :reader midi->key)
   (xy->key :type hash-table :initform (make-hash-table :test #'equalp) :reader xy->key)
   (pitch->key :type hash-table :initform (make-hash-table) :reader pitch->key)
   (lilypond->key :type hash-table :initform (make-hash-table :test #'equalp) :reader lilypond->key)
   ))

(defclass piano-key (visicon-object)
  ((midi :type integer :initform 0 :initarg :midi :reader midi)
   (pitch-symbols :type list :initform nil :reader pitch-symbols)
   (pitch-lilyponds :type list :initform nil :reader pitch-lilyponds)))

(defmethod print-object ((object piano-key) stream)
  (with-slots (pitch-symbols xy midi) object
    (print-unreadable-object (object stream :type t :identity nil)
      (format stream "~S ~S ~S" pitch-symbols xy midi))))

(defun set-color (piano-key keyboard)
  (with-slots (midi color) piano-key
    (setf color
          (if (isa-natural midi)
              (natural-key-color keyboard)
            (accidental-key-color keyboard)))))

(defun set-wh (piano-key keyboard)
  (with-slots (midi wh) piano-key
    (setf wh
          (if (isa-natural midi)
              (list (natural-key-w keyboard) (natural-key-h keyboard))
            (list (accidental-key-w keyboard) (accidental-key-h keyboard))))))

(defun compute-natural-x (nb-naturals keyboard)
  "The x value is the x value of the keyboard (min left) + the number of with of natural key widths before the key + half the with of a key."
  (let ((w (natural-key-w keyboard)))
    (round (+ (x keyboard)
              (* w (1- nb-naturals)) 
              (* w +w.5+)))))

(defun compute-accidental-x (nb-naturals keyboard)
  "The x value is the x value of the keyboard (min left) + the number of with of natural key widths before the key (centered)."
  (round (+ (x keyboard)
            (* (natural-key-w keyboard) nb-naturals))))


(defun compute-natural-y (keyboard)
  "The y value is the x value of the keyboard (top left) + 75% of the key height."
  (round (+ (y keyboard)
            (* (natural-key-h keyboard) +h.75+))))

(defun compute-accidental-y (keyboard)
  "The y value is the x value of the keyboard (top left) + 75% of the key height."
  (round (+ (y keyboard)
            (* (accidental-key-h keyboard) +h.75+))))

(defun set-xy (piano-key keyboard)
  (with-slots (midi xy) piano-key
    (let ((nb-naturals (nb-naturals (midi-start keyboard) midi)))
      (setf xy
            (if (isa-natural midi)
                (list (compute-natural-x nb-naturals keyboard)
                      (compute-natural-y keyboard))
               (list (compute-accidental-x nb-naturals keyboard)
                      (compute-accidental-y keyboard)))))))

(defun index-key (piano-key keyboard)
  (setf (gethash (midi piano-key) (midi->key keyboard)) piano-key
        (gethash (xy piano-key) (xy->key keyboard)) piano-key)
  (dolist (symbol (pitch-symbols piano-key))
    (setf (gethash symbol (pitch->key keyboard))
          piano-key))
  (dolist (lilypond (pitch-lilyponds piano-key) keyboard)
    (setf (gethash lilypond (lilypond->key keyboard))
          piano-key)))

(defmethod initialize-instance :after ((object piano-key) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (with-slots (midi pitch-symbols pitch-lilyponds) object
    (setf pitch-symbols (->symbol (midi->pitch-strings midi))
          pitch-lilyponds (midi->pitch-strings midi :lilypond))))

(defun make-piano-key (midi keyboard)
  (let ((piano-key (car (setf (surface-collection keyboard)
                              (make-instance 'piano-key :midi midi 
                                             :on-surface keyboard)))))
    (set-color piano-key keyboard)
    (set-wh piano-key keyboard)
    (set-xy piano-key keyboard)
    (index-key piano-key keyboard)
    piano-key))
   



;;; eof