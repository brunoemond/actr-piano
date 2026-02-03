; ich versuche mal, die ganze Sache zu ordnen:
; die Datei enthält allgemeine Strukturen und Funktionen über Musik, orientiert an der Lilypond-Notation
; sowie einige globale Konstanten
; Änderung vom 13.09.23: Die Struktur tone bekommt noch ein boolean Feld cho, das anzeigt, 
; ob ein Akkord gespielt werden soll, oder nicht

;------------- Hilfsfunktionen ---------------------------

(defun succ (ele ili)
  (car (cdr (member ele ili))))
  
(defun pred (ele ili)
  (if (not (equal (first ili) ele)) (nth (- (position ele ili) 1) ili)))
  
(defun append1 (lst obj)
  (append lst (list obj)))

(defmacro while (test &rest body)
  `(do () 
       ((not ,test))
	 ,@body))  


(defun tokens (str test start)
  (let ((p1 (position-if test str :start start)))
    (if p1
	  (let ((p2 (position-if #'(lambda (c)
	                             (not (funcall test c)))
                             str :start p1)))
        (cons (subseq str p1 p2)
              (if p2
                (tokens str test p2)
                nil)))
      nil)))  

(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\  ))
	   (not (char= c #\| ))))


; -------------------- globale Strukturen und Konstanten -----------------------

(defstruct rhythm-context
  (beatnum 4 :type integer)
  (beatden 4 :type integer)
  (mindur 8 :type integer)
  (minbar 8 :type integer)
)

(defstruct harmony-context
  (hkey "cmaj" :type string)
  (hmode 4 :type integer) ; 4 major, 3 minor
  (diatones (list "c" "d" "e" "f" "g" "a" "b") :type sequence) 
)

(defstruct global-context
  (rhythm nil :type rhythm-context)
  (harmony nil :type harmony-context)
)

(defstruct tone
  (name "c" :type string)
  (oct 'i :type atom)
  (num 24 :type integer)   ; chromatic number
  (dur 4 :type integer)    ; duration (in lilypond notation)
  (dot 0 :type integer)    ; duration (dot)
  (fig "" :type string)
  (res nil :type boolean)  ; a rest or not
  (cho T :type boolean)    ; play a new chord?
  (pla T :type boolean)    ; play the note or let it sound on
  (bea 0 :type integer)    ; the nth beat from beginning in the unit *mindur* 
)

; Ich weiche von Lilypond insofern ab, als ich längere Noten mit zwei konsekutiven Ziffern
; in zwei Noten aufteile und jeder eine eigene Ziffer zuordne. Gespielt wird dann nur die erste und ausgehalten (19.09.23)

(defparameter *semitones* (make-hash-table :test 'equal) "maps semitone names to numbers")
(setf (gethash "c" *semitones*) 0)
(setf (gethash "cis" *semitones*) 1)
(setf (gethash "des" *semitones*) 1)
(setf (gethash "d" *semitones*) 2)
(setf (gethash "dis" *semitones*) 3)
(setf (gethash "es" *semitones*) 3)
(setf (gethash "e" *semitones*) 4)
(setf (gethash "f" *semitones*) 5)
(setf (gethash "fis" *semitones*) 6)
(setf (gethash "ges" *semitones*) 6)
(setf (gethash "g" *semitones*) 7)
(setf (gethash "gis" *semitones*) 8)
(setf (gethash "as" *semitones*) 8)
(setf (gethash "a" *semitones*) 9)
(setf (gethash "ais" *semitones*) 10)
(setf (gethash "bes" *semitones*) 10)
(setf (gethash "b" *semitones*) 11)

(defparameter *diatones* (make-hash-table :test 'equal) "maps semitone names to diatonic numbers")
(setf (gethash "c" *diatones*) 0)
(setf (gethash "cis" *diatones*) 0)
(setf (gethash "des" *diatones*) 1)
(setf (gethash "d" *diatones*) 1)
(setf (gethash "dis" *diatones*) 1)
(setf (gethash "es" *diatones*) 2)
(setf (gethash "e" *diatones*) 2)
(setf (gethash "f" *diatones*) 3)
(setf (gethash "fis" *diatones*) 3)
(setf (gethash "g" *diatones*) 4)
(setf (gethash "gis" *diatones*) 4)
(setf (gethash "as" *diatones*) 5)
(setf (gethash "a" *diatones*) 5)
(setf (gethash "ais" *diatones*) 5)
(setf (gethash "bes" *diatones*) 6)
(setf (gethash "b" *diatones*) 6)

(defparameter *key-diatones* (make-hash-table :test 'equal) "maps key names to lists of diatonic tone names")
(setf (gethash "cmaj" *key-diatones*) (list "c" "d" "e" "f" "g" "a" "b"))
(setf (gethash "gmaj" *key-diatones*) (list "c" "d" "e" "fis" "g" "a" "b"))
(setf (gethash "dmaj" *key-diatones*) (list "cis" "d" "e" "fis" "g" "a" "b"))
(setf (gethash "amaj" *key-diatones*) (list "cis" "d" "e" "fis" "gis" "a" "b"))
(setf (gethash "fmaj" *key-diatones*) (list "c" "d" "e" "f" "g" "a" "bes"))
(setf (gethash "besmaj" *key-diatones*) (list "c" "d" "es" "f" "g" "a" "bes"))
(setf (gethash "esmaj" *key-diatones*) (list "c" "d" "es" "f" "g" "as" "bes"))
(setf (gethash "amin" *key-diatones*) (list "c" "d" "e" "f" "gis" "a" "b"))
(setf (gethash "emin" *key-diatones*) (list "c" "dis" "e" "fis" "g" "a" "b"))
(setf (gethash "bmin" *key-diatones*) (list "cis" "d" "e" "fis" "g" "ais" "b"))
(setf (gethash "dmin" *key-diatones*) (list "c" "d" "e" "f" "g" "a" "bes"))
(setf (gethash "gmin" *key-diatones*) (list "c" "d" "es" "fis" "g" "a" "bes"))
; to be continued

(defparameter *semitonesr* '("c" "cis" "d" "es" "e" "f" "fis" "g" "gis" "a" "bes" "b"))
(defparameter *octaves* '(g k i ii iii) "lists the names of the 5 octaves used in music01")

(defvar *mindur* 4) ; kleinste rhythmische Einheit
(defvar *dietoene* '())
(defvar *last-tone* (make-tone :name "c" :oct 'k :dur 4 :dot 0 :bea 0))
(defvar *counter* 0)  ; wichtig im ACT-R-Modell, zeigt den aktuellen Ton angesprochen

; *diatones* muss definiert werden, weil es in music01 angesprochen wird (vorläufig (14.9.23)
(defvar *rhythm* (make-rhythm-context))
(defvar *harmony* (make-harmony-context))
(defvar *context* (make-global-context :rhythm *rhythm* :harmony *harmony*))

; ---------- Funktionen unter Verwendung von globalen Variablen --------------

(defun diainterval (start intnum &optional (diat (harmony-context-diatones *harmony*)))
"returns a string with the pitch class of distance intnum from start; intervals are prime = 0 etc; 
 accepts start values from -8 through 99; alterations can be indicated with e.g. 3.5 instead of 3" 
  (let ((fac (round (/ intnum (abs intnum))))
        (interval (truncate intnum))
		(dianu (gethash start *diatones*))
		(res nil))
    (progn
 	  (setf interval (- interval fac))
      (if (equal interval 0) (setf res start)
        (setf res (nth (mod (+ dianu interval 14) 7) diat)))
	  (when (not (typep intnum 'integer))
         (setf res (nth (mod (round (+ (position res *semitonesr* :test #'equal) fac)) 12) *semitonesr*)))
	) res)
)

(defun rep-single (diat tona)
"destructively replaces the pitch class name tona at the appropriate place in diat"
  (setf (nth (gethash tona *diatones*) diat) tona)
)

(defun replace-alterations (diat replist)
"non-destructively replaces the list of pitch class names in replist at the appropriate places in diat"
(let ((newseq (copy-list diat)))
  (dolist (ele replist 'done) (rep-single newseq ele))
  newseq)
)

(defun getdiainterval (start end dir)
; start und end sind Tonnamen, dir gibt die Richtung an; zurückgegeben wird das diatonische Intervall
  (let ((diff (- (position end  (harmony-context-diatones *harmony*) :test #'equal) (position start  (harmony-context-diatones *harmony*) :test #'equal))))
	(if (equal dir 'up)
	  (if (>= diff 0) (+ diff 1) (+ 8 diff))
      (if (equal dir 'down) 
        (if (<= diff 0) (- diff 1) (+ -8 diff))
        nil)))
)

(defun inc-bea (idur idots)
; gibt ein Inkrement an Schlägen wieder; muss an die neue Struktur angepasst werden (14.9.23)
  (let ((beats (/ *mindur* idur)))
    (setf beats (+ beats (* beats (/ (- (expt 2 idots) 1) (expt 2 idots)))))))


(defun get-tone-number (itone)
; gibt die Nummer des gegebenen Tones (Typ: tone) zurück
; Zahlen im Jan 2026 an MIDI-Standard angepasst
  (+ (gethash (tone-name itone) *semitones*)
     (case (tone-oct itone)
      (g 36)
	  (k 48)
	  (i 60)
	  (ii 72)
	  (iii 84)
	  (otherwise 1000)))
)

(defun get-tone-number-2 (tname oct)
; gibt die Nummer des gegebenen Tones aus Tonname und Oktav zurück
  (+ (gethash tname *semitones*)
     (case oct
      (g 0)
	  (k 12)
	  (i 24)
	  (ii 36)
	  (iii 48)
	  (otherwise 1000)))
)

(defun get-tone (num) "returns the tone name with the given number as string"
  (nth (mod num 12) *semitonesr*))

(defun interval (tname1 tname2)
  (- (gethash tname2 *semitones*) (gethash tname1 *semitones*)))
  
  
(defun dia-chro-interval (tonename oct interval)
   (let* ((zielton (diainterval tonename interval))
          (sourcenum (get-tone-number-2 tonename oct))
		  (targetnum (get-tone-number-2 zielton oct))
		  (diff (- targetnum sourcenum))
		  (ind (* interval diff)))
	 (if (< ind 0) 
	   (if (> interval 0) 
	     (setf targetnum (+ targetnum 12))
		 (setf targetnum (- targetnum 12))))
	 targetnum))	 
   
	   
; folgende Funktion gibt das Vorzeichen nach Tonart zurück: Kreuz 1, kein Vz. 0, b -1
;  das stimmt leider noch nicht beim as und beim des - seltsam!
(defun accidental (cnum keynum)
   (let* ((modnum (mod cnum 12))
          (dnum (round (+ (* 0.557 modnum) 0.975)))
		  (tnum (round (mod (* 2.24 dnum) 8)))
		  (res 1))
		(if (< keynum (- tnum 8)) (setf res -1)
          (if (< keynum tnum) (setf res 0)))
    res)) 
		  


; -------- Übersetzungfunktionen für lilypond ohne Bezug zu globalen Variablen

(defun extract-tone-info (stri)
  "reads lilypond notation (in stri) and returns a list containing the relevant components"
  (let ((tonstri "")(octstri "")(durstri "")(dotstri ""))
    (do ((rest stri (subseq rest 1)))
      ((equal rest "") (list tonstri octstri durstri dotstri))
	  (let ((chr (char rest 0)))
	    (cond 
          ((alpha-char-p chr) (setf tonstri (concatenate 'string tonstri (string chr))))
          ((equal chr #\,) (setf octstri (concatenate 'string octstri ",")))
		  ((equal chr #\') (setf octstri (concatenate 'string octstri "'")))
		  ((equal chr #\.) (setf dotstri (concatenate 'string dotstri ".")))
		  ((digit-char-p chr) (setf durstri (concatenate 'string durstri (string chr))))
		  (T (print "kack"))))))) 

(defun extract-figure-info (stri)
  "reads lilypond notation (in stri) and returns a list containing the relevant components (figures/rests, duration);
   works only with up to one dot"
  (let ((figstri "")(durstri "")(dotstri "")(procdur nil))
    (do ((rest stri (subseq rest 1)))
      ((equal rest "") (list figstri durstri dotstri))
	  (let ((chr (char rest 0)))
	    (if (not procdur)
		  (cond 
		    ((equal chr #\<) T)
			((or (equal chr #\r) (equal chr #\s))
		      (setf figstri (concatenate 'string figstri (string chr)) procdur T))
			((or (digit-char-p chr) (equal chr #\+) (equal chr #\-))
			  (setf figstri (concatenate 'string figstri (string chr))))
			((equal chr #\Space)
			  (setf figstri (concatenate 'string figstri (string #\_))))
			((equal chr #\>) (setf procdur T)))
		  ;else:
	      (cond 
		    ((digit-char-p chr) (setf durstri (concatenate 'string durstri (string chr))))
		    ((equal chr #\.) (setf dotstri "1"))
		    (T (print "kack"))
		  ))))))

(defun find-next-lilypond-tone (t0 t1)
  (let ((rdiff (- (interval (tone-name t1) (tone-name t0))))
	(res 'untenminus))
    (print rdiff)
    (if (< rdiff -6) (setq res 'obenplus)
      (if (< rdiff 0) (setq res 'unten)
        (if (< rdiff 7) (setq res 'oben))))
    res))

(defun find-tone-by-beat (toene beat)
  "returns the tone with the given beat in the list of tones; beats are numbered continuously"
  (if (not (null toene))
    (if (equal (tone-bea (car toene)) beat)
      (car toene)
	  (find-tone-by-beat (cdr toene) beat))))


; ---- enthält wieder Bezug zur globalen Variablen, z.B. *last-tone* (auslagern?)

(defun set-rhythm (bnum bden minduration)
  (setf (rhythm-context-beatnum *rhythm*) bnum
        (rhythm-context-beatden *rhythm*) bden
		(rhythm-context-mindur *rhythm*) minduration
		(rhythm-context-minbar *rhythm*) (/ (* minduration bnum) bden))
)

(defun set-harmony (keyname)
  (let ((mode (cond
                ((numberp (search "maj" keyname)) 4)
				((numberp (search "min" keyname)) 3)
				(T 0)))
        (tlist (gethash keyname *key-diatones*)))
    (setf (harmony-context-hkey *harmony*) keyname
          (harmony-context-hmode *harmony*) mode
		  (harmony-context-diatones *harmony*) tlist))
)

(defun const-tone (tstri lasttone)
  "constructs a tone (type tone) from a single lilypond note (including rhythm and beat)"
  (let ((parli (extract-tone-info tstri))
        (tone (make-tone :name ""
		                 :oct (tone-oct lasttone)
						 :dur (tone-dur lasttone)
						 :dot (tone-dot lasttone)
						 :res nil)))
    (progn 
	  (setf (tone-name tone) (first parli))
	  (if (> (length (nth 2 parli)) 0)
	    (progn 
	      (setf (tone-dur tone) (read-from-string (nth 2 parli)))
		  (setf (tone-dot tone) 0)))
	  (if (> (length (nth 3 parli)) 0) (setf (tone-dot tone) (length (nth 3 parli))))
	  (setf (tone-bea tone) 
	    (+ (inc-bea (tone-dur lasttone)(tone-dot lasttone))(tone-bea lasttone)))
	  (print parli)
	  
	  (if (not (equal (tone-name tone) "r"))
	    (progn 
		  (setf (tone-res tone) nil)
 	      (let ((octsym (find-next-lilypond-tone lasttone tone)))
			 (cond 
			   ((equal octsym 'obenplus) (setf (tone-oct tone) (succ (tone-oct tone) *octaves*)))
			   ((equal octsym 'untenminus) (setf (tone-oct tone) (pred (tone-oct tone) *octaves*)))))
		  (do ((i 1 (incf i)))
			  ((> i (length (nth 1 parli))) (progn 
											  (setf (tone-num tone) (get-tone-number tone))
											  (setf *last-tone* tone)
											 ))
			(if (equal (char (nth 1 parli) 0) #\')
			  (setf (tone-oct tone) (succ (tone-oct tone) *octaves*))
			  (if (equal (char (nth 1 parli) 0) #\,)
				(setf (tone-oct tone) (pred (tone-oct tone) *octaves*))
				  (print "gleiche Oktave")))))
	    (progn ;else
		  (print "Es ist eine Pause")
		  (setf (tone-res tone) t)
		  (setf (tone-name tone) (tone-name lasttone))
		  (setf *last-tone* tone)
		)  
		))
   tone))
; die Funktion scheint die Punktierungen angemessen zu berücksichtigen (zumindest für einen Punkt)


(defun add-figures-to-tonelist (toene figstring mindur)
  (let* ((bcount 1)
	     (parli "")
  	     (increment 0)
	     (curdur 2)  ; muss in der Einheit *mindur* geführt werden
         (figlist '()))
	(progn	
	  (dolist (stri (tokens figstring #'constituent 0) 'done)
        (progn
          (setf parli (extract-figure-info stri)) ; hier evtl. die Punkte noch anders berücksichtigen
          (print parli)
		  (cond 
            ; hier noch Taktarten<>1 berücks.! 
		    ((or (equal (first parli) "r") (equal (first parli) "s"))
             (if (string> (second parli) "")
	           (if (string> (third parli) "")
	             (setf increment (inc-bea (read-from-string (second parli)) (read-from-string (third parli)))
	                   curdur increment)
                 (setf increment (inc-bea (read-from-string (second parli)) 0)
			           curdur increment))
               (setf increment curdur)))
		    (T
             (progn 
               (if (string> (second parli) "")
	             (if (string> (third parli) "")
	               (setf increment (inc-bea (read-from-string (second parli)) (read-from-string (third parli)))
	                     curdur increment)
                   (setf increment (inc-bea (read-from-string (second parli)) 0)
			             curdur increment))
                 (setf increment curdur))
			   (setf figlist (append1 figlist (cons bcount (first parli))))
			   ;(print figlist)
			   ))
		  )
		  (setf bcount (+ bcount increment))))
       (dolist (ele figlist 'done)
         (setf (tone-fig (find-tone-by-beat toene (car ele))) (cdr ele)))	   
     ))
)
		   
