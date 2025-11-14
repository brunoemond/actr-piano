;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; midi-utilities-tests.lisp
;;;
;;; 2025-10-29
;;;

(print ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; midi-utilities" *standard-output*)

;;;
(print "midi->octave" *standard-output*)
;;;
(assert (eq 0 (midi->octave 21)))
(assert (eq 4 (midi->octave 60)))
(assert (eq 8 (midi->octave 108)))

;;;
(print "nb-naturals" *standard-output*)
;;;
(assert (eq 1 (nb-naturals 60 60))) 
(assert (eq 1 (nb-naturals 60 61)))
(assert (eq 2 (nb-naturals 60 62)))
(assert (eq 7 (nb-naturals 60 71)))
(assert (eq 52 (nb-naturals 21 108)))

;;;
(print "midi->pitch-class-strings" *standard-output*)
;;;
(let ((midi 60))
  (assert (every #'string= '("C") (midi->pitch-class-strings midi :scientific)))
  (assert (every #'string= '("c") (midi->pitch-class-strings midi :lilypond)))
  (assert (every #'string= '("C") (midi->pitch-class-strings midi))))

(let ((midi 61))
  ;(assert (every #'string= '("C♯" "D♭") (midi->pitch-class-strings midi :scientific)))
  (assert (every #'string= '("cis" "des") (midi->pitch-class-strings midi :lilypond)))
  (assert (every #'string= '("CIS" "DES") (midi->pitch-class-strings midi))))

;;;
(print "midi->pitch-strings" *standard-output*)
;;;
(let ((midi 60))
  (assert (every #'string= '("C4") (midi->pitch-strings midi :scientific)))
  (assert (every #'string= '("c") (midi->pitch-strings midi :lilypond)))
  (assert (every #'string= '("C4") (midi->pitch-strings midi))))

(let ((midi 61))
  ;(assert (every #'string= '("C♯4" "D♭4") (midi->pitch-strings midi :scientific)))
  (assert (every #'string= '("cis" "des") (midi->pitch-strings midi :lilypond)))
  (assert (every #'string= '("CIS4" "DES4") (midi->pitch-strings midi))))

(let ((midi 49))
  ;(assert (every #'string= '("C♯3" "D♭3") (midi->pitch-strings midi :scientific)))
  (assert (every #'string= '("cis," "des,") (midi->pitch-strings midi :lilypond)))
  (assert (every #'string= '("CIS3" "DES3") (midi->pitch-strings midi))))

(let ((midi 73))
  ;(assert (every #'string= '("C♯5" "D♭5") (midi->pitch-strings midi :scientific)))
  (assert (every #'string= '("cis'" "des'") (midi->pitch-strings midi :lilypond)))
  (assert (every #'string= '("CIS5" "DES5") (midi->pitch-strings midi))))

;;; eof