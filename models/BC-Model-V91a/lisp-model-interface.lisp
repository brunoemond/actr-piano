; Interface-Funktionen für das ACT-R-Modell

(defun read-next-tone (tonliste)
 (if (< *counter* (length tonliste))
  (tone-num (nth *counter* tonliste))
  -1)
)
;gibt die Tonnummer wieder (ist am einfachsten)


(defun get-current-figure (tonliste)
 (let ((figure (tone-fig (nth *counter* tonliste))))
   (setf *counter* (+ 1 *counter*))
   figure
 )
)

; use global variable *dietoene*
(defun nocho (beat &optional (tones *dietoene*))
  "turns off the chord toggle of the tone at the given beat"
    (setf (tone-cho (find-tone-by-beat tones beat)) nil))

(defun nopla (beat &optional (tones *dietoene*))
  "turns off the play toggle of the tone at the given beat"
    (setf (tone-pla (find-tone-by-beat tones beat)) nil))



(defun erzeuge-actr-toene (mindur note-string fig-string)
; ich gehe davon aus, dass der Referenzton immer das kleine c ist
  (progn
	; hier werden die Töne erzeugt
    (dolist (stri (tokens note-string #'constituent 0) 'done)
      (setf *dietoene* (append1 *dietoene* (const-tone stri *last-tone*))))
	; Pausen werden als solche gekennzeichnet  
    (dolist (ton *dietoene* 'done) (if (tone-res ton) (setf (tone-name ton) "r")))
	; und jetzt noch die Ziffern hinzufügen
	(if (not (equal fig-string ""))
	  (add-figures-to-tonelist *dietoene* fig-string mindur)
	)
))


; hier noch einige Variblen und Funktionen, die im ACT-R-Modell verwendet werden

(defun model-next-round ()
  (progn 
    (run-full-time 5.0)
    (setf *counter* 0)
	(clear-buffer 'retrieval)
	(clear-buffer 'goal)
	(clear-buffer 'imaginal)
	(mod-chunk start-goal bnc nil bnp nil figc nil figp nil dnp nil prevchord nil bassmove nil diskmove nil r1 nil r2 nil r3 nil res nil dnp fii)
	(goal-focus start-goal))
)


(defstruct midi-pars-type
  (time 0.0 :type real)
  (tb 36 :type integer)
  (t1 55 :type integer)
  (t2 60 :type integer)
  (t3 64 :type integer)
)

(defvar *midi-pars* (make-midi-pars-type))
 
(defun make-midi-entry (ichunk istream)
  (let ((newtime (mp-time))
        (time (midi-pars-type-time *midi-pars*))  
        (tb (midi-pars-type-tb *midi-pars*))
		(t1 (midi-pars-type-t1 *midi-pars*))
		(t2 (midi-pars-type-t2 *midi-pars*))
		(t3 (midi-pars-type-t3 *midi-pars*))
		)
	(progn	
      (format istream "~&+0~,3,,,F~%" (- newtime time))
	  (setf (midi-pars-type-time *midi-pars*) newtime)
	  (format istream "off ~A 1 0~%" tb)
	  (format istream "off ~A 1 0~%" t1)
	  (format istream "off ~A 1 0~%" t2)
	  (format istream "off ~A 1 0~%" t3)
	  (setf tb (if (null (chunk-slot-value-fct ichunk 'bn)) 0 (+ 36 (chunk-slot-value-fct (chunk-slot-value-fct ichunk 'bn) 'cnum))))
          (setf t1 (if (null (chunk-slot-value-fct ichunk 'r1)) 0 (+ 36 (chunk-slot-value-fct (chunk-slot-value-fct ichunk 'r1) 'cnum))))
          (setf t2 (if (null (chunk-slot-value-fct ichunk 'r2)) 0 (+ 36 (chunk-slot-value-fct (chunk-slot-value-fct ichunk 'r2) 'cnum))))
	  (setf t3 (if (null (chunk-slot-value-fct ichunk 'r3)) 0 (+ 36 (chunk-slot-value-fct (chunk-slot-value-fct ichunk 'r3) 'cnum))))
  	  (when (< 0 tb) (format istream "on ~A 1 70~%" tb))
	  (when (< 0 t1) (format istream "on ~A 1 90~%" t1))
	  (when (< 0 t2) (format istream "on ~A 1 90~%" t2))
	  (when (< 0 t3) (format istream "on ~A 1 110~%" t3))
	  (setf (midi-pars-type-tb *midi-pars*) tb)
	  (setf (midi-pars-type-t1 *midi-pars*) t1)
	  (setf (midi-pars-type-t2 *midi-pars*) t2)
	  (setf (midi-pars-type-t3 *midi-pars*) t3)
	)
  )
)

(defun make-midi-entry-bn (ichunk istream)
  (let ((newtime (mp-time))
        (time (midi-pars-type-time *midi-pars*))  
        (tb (midi-pars-type-tb *midi-pars*))
		)
	(progn	
      (format istream "~&+0~,3,,,F~%" (- newtime time))
	  (setf (midi-pars-type-time *midi-pars*) newtime)
	  (format istream "off ~A 0~%" tb)
	  (setf tb (if (null ichunk) 0 (+ 36 (chunk-slot-value-fct ichunk 'cnum))))
  	  (format istream "on ~A 90~%" tb)
	  (setf (midi-pars-type-tb *midi-pars*) tb)
	)
  )
)

(defun make-lily-entry (ichunk istream)
  (let ((t1 (chunk-slot-value-fct ichunk 'r1))
		(t2 (chunk-slot-value-fct ichunk 'r2))
		(t3 (chunk-slot-value-fct ichunk 'r3))
		)
	(progn
      (setf t1 (if (null t1) "r" (chunk-slot-value-fct t1 'name)))
      (setf t2 (if (null t2) "r" (chunk-slot-value-fct t2 'name)))
      (setf t3 (if (null t3) "r" (chunk-slot-value-fct t3 'name)))
  	  (format istream "<~A ~A ~A>~%" t1 t2 t3))
  )
)

(defun make-timestamp (ostream)
  (multiple-value-bind
	 (second minute hour date month year day-of-week dst-p tz)
	 (get-decoded-time)
     (format ostream "It is now ~2,'0d:~2,'0d:~2,'0d, ~d/~2,'0d/~d (GMT~@d)~%"
	      hour
	      minute
	      second
	      month
	      date
	      year
	      (- tz))))

(defstruct exercise-type
  (title "" :type string)
  (tostri "" :type string)
  (figstri "" :type string)
  (lasttone "c" :type string)
  (lasttcount 12 :type integer)
  (rnum 4 :type integer)
  (rden 4 :type integer)
  (rmin 8 :type integer)
  (harm "cmaj" :type string)
  (droplist nil :type sequence))

; Zu lösende Probleme
; Problem 1: Wie gehe ich damit um, wenn auf einen Basston hintereinander 2 Ziffern kommen, wie z.B. bei 4 3 
;  Ansatz: Spalte den Ton in zwei Hälften und markiere die zweite Hälfte mit "nicht neu anspielen"
; Problem 2: Ich muss das Diatonie-Chromatik-Problem lösen, weil bei H. sehr bald Vorzeichen als Ziffern kommen

