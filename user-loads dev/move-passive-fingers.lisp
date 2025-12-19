(defvar *current* (list (cons 'thumb #(0 0))(cons 'index #(10 0))(cons 'middle #(20 0))
                        (cons 'ring #(30 0))(cons 'pinkie #(40 0))))
								     
(defvar *requested*  (list (cons 'thumb #(0 0))(cons 'middle #(30 0))
                           (cons 'pinkie #(50 0))))

(defun move-passive-fingers (hand-name requested-o current-o &optional (unit 10))
  (let ((firstu -1)
	(index nil)
	(found nil)
	(resli nil))
    (dolist (item current-o)
      (progn
	(unless found (setf firstu (1+ firstu)))
	(setf index (position (car item) requested-o :key #'car :test #'equal))
	(if index
 	    (progn
	      (setf (cdr item) (cdr (nth index requested-o)))
	      (setf resli (append resli (list (cons 'u item))))
	      (setf found t))
          (setf resli (append resli (list (cons 'd item)))))))
					;go back from firstu and adjust
    (let ((lcoor (aref (cddr (nth firstu resli)) 0)))
      (loop for i from (1- firstu) downto 0
            do (setf (aref (cddr (nth i resli)) 0) (- lcoor (* (- firstu i) unit))))
					;then adapt from firstu forward
      (loop for i from (1+ firstu) to (1- (length resli))
	    do (if (equal (car (nth i resli)) 'u)
		   (setf firstu i lcoor (aref (cddr (nth i resli)) 0))
                 (setf (aref (cddr (nth i resli)) 0) (+ lcoor (* (- i firstu) unit))))))
    (mapcar #'cdr resli)
    ))

;;;
;;; Just to test the function 
;;;
(defun finger-x-offsets (&key thumb index middle ring pinkie)
  (append (when thumb (list (cons 'thumb (vector thumb 0))))
          (when index (list (cons 'index (vector index 0))))
          (when middle (list (cons 'middle (vector middle 0))))
          (when ring (list (cons 'ring (vector ring 0))))
          (when pinkie (list (cons 'pinkie (vector pinkie 0))))))
  

(defun test-passive-fingers (requested expected)
  (let ((result (move-passive-fingers 'ignore-hand requested *current*)))
    (unless (equalp expected result)
      (warn "~%Result:   ~S~%Expected: ~S." result expected))))

;; no change: requesting a nil set of finger offsets
;; Currently generate a warning because all fingers are increased by 10.
(test-passive-fingers (finger-x-offsets) 
                      (finger-x-offsets :thumb 0 :index 10 :middle 20 :ring 30 :pinkie 40))

;; moving the index 
(test-passive-fingers (finger-x-offsets :index 20) 
                      (finger-x-offsets :thumb 10 :index 20 :middle 30 :ring 40 :pinkie 50))

;; moving the middle 
(test-passive-fingers (finger-x-offsets :middle 40) 
                      (finger-x-offsets :thumb 20 :index 30 :middle 40 :ring 50 :pinkie 60))

;; moving the pinkie 
(test-passive-fingers (finger-x-offsets :pinkie 50) 
                      (finger-x-offsets :thumb 10 :index 20 :middle 30 :ring 40 :pinkie 50))

; moving the index and pinkie, keeping the thumb in place
(test-passive-fingers (finger-x-offsets :thumb 0 :middle 30 :pinkie 50) 
                      (finger-x-offsets :thumb 0 :index 10 :middle 30 :ring 40 :pinkie 50))

; moving the thumb left
(test-passive-fingers (finger-x-offsets :thumb -10) 
                      (finger-x-offsets :thumb -10 :index 0 :middle 10 :ring 20 :pinkie 30))



