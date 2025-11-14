;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; pitches-coordinates.lisp
;;;
;;; 2025-11-07
;;;
;;; Using a key note value and an interval lable to calculate the distance to the interval and the coodinate of the interval key. 

;; top left is (0 0)
(defparameter *natural-key-width* 10
  "Width in mm of a natural key.")
(defparameter *half-kw* (/ *natural-key-width* 2)
  "Half the width of a natural key.")

(defparameter *natural-y* 10
  "Distance in mm from the top to the centre of an natural key.")
(defparameter *accidental-y* 5
  "Distance in mm from the top to the centre of an accidental key.")

(defconstant +intervals+ '(P1 N2 M2 N3 M3 P4 TT P5 N6 M6 N7 M7 P8)
  "P = perfect, N = minor, M = major, TT = Tritone.")

(defconstant +natural-tones+ '(0 2 4 5 7 9 11))
(defconstant +accidental-tones+ '(1 3 6 8 10))

(defconstant +tone-nb-half-kw+
  '(
     0 (0 1 2 3 4    6 7 8 9 10 11 12    14)   ;C
     1 (  0 1 2 3    5 6 7 8  9 10 11    13 14)   ;Cis Des
     2 (    0 1 2    4 5 6 7  8  9 10    12 13 14)   ;D
     3 (      0 1    3 4 5 6  7  8  9    11 12 13 14)   ;Dis Fes
     4 (        0    2 3 4 5  6  7  8    10 11 12 13 14)      ;E
     5 (             0 1 2 3  4  5  6     8  9 10 11 12    14)   ;F      
     6 (               0 1 2  3  4  5     7  8  9 10 11    13 14)  ;Fis Ges      
     7 (                 0 1  2  3  4     6  7  8  9 10    12 13 14)   ;G      
     8 (                   0  1  2  3     5  6  7  8  9    11 12 13 14)   ;Gis Aes      
     9 (                      0  1  2     4  5  6  7  8    10 11 12 13 14)  ;A      
    10 (                         0  1     3  4  5  6  7     9 10 11 12 13 14)  ;Ais Bes      
    11 (                            0     2  3  4  5  6     8  9 10 11 12 13 14) ;B      
    )
  "Look-up table for the number of half natural key width. See function 'tone-interval->nb-half-kw'.")

(defun interval-pos (interval)
  "Position of the interval lable in the list of intervals."
  (position interval +intervals+))

(defun tone-interval->nb-half-kw (tone interval)
  (nth (interval-pos interval)  
       (getf +tone-nb-half-kw+ tone)))

(defun distance-x (tone interval)
  (* (tone-interval->nb-half-kw tone interval)
     *half-kw*))

(defun y-coordinate (tone)
  (if (member tone +natural-tones+)
      *natural-y*
    *accidental-y*))
  
(defun xy-from-interval (tone xy interval)
  (vector (+ (elt xy 0) (distance-x tone interval))
          (y-coordinate tone)))
          
(defun keys-distance (tone xy interval)
  (let ((xy-from-interval (xy-from-interval tone xy interval)))
    (sqrt (+ (expt (- (elt xy-from-interval 0) (elt xy 0)) 2)
             (expt (- (elt xy-from-interval 1) (elt xy 1)) 2)))))

;;;
;;; tests
;;;

;; all tones of P1 interval have zero steps
(dotimes (tone 12)
  (assert (eq 0 (tone-interval-steps tone 'p1))))

;; intervals of second minor is one step but for E and B
(dotimes (tone 12)
  (if (member tone '(4 11))
      (assert (eq 2 (tone-interval-steps tone 'n2)))
    (assert (eq 1 (tone-interval-steps tone 'n2)))))

;; P1 intervals have 0 x-distance
(assert (equal 0 (distance-x 4 'p1)))


;; distance between keys 
;; C to D (M2, 2nd major) and E to F (N2, 2nd minor)
(assert (eq 10.0 (keys-distance 0 (vector *half-kw* *natural-y*) 'm2)))
(assert (eq 10.0 (keys-distance 4 (vector *half-kw* *natural-y*) 'n2)))



;;; eof