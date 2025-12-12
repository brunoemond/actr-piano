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


(move-passive-fingers 'right *requested* *current*)