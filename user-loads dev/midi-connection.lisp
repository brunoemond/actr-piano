;(require 'usocket)

(defclass tcp-connection ()
  ((c-ip :accessor c-ip :initarg :ip :initform "127.0.0.1") ;  127.0.0.1
   (c-port :accessor c-port :initarg :port :initform 9800)
   (socket :accessor c-sock)
   (stream :accessor c-strm)))

(defmethod initialize-instance :after ((instance tcp-connection) &key)
  (with-slots (c-ip c-port socket stream) instance
    (setf socket (usocket:socket-connect c-ip c-port)
          stream (usocket:socket-stream socket))))

(defmethod close-connection ((instance tcp-connection))
  (close (c-strm instance))
  (usocket:socket-close (c-sock instance)))

(defmethod connect-stream ((instance tcp-connection))
  (with-slots (c-ip c-port socket stream) instance
    (setf socket (usocket:socket-connect c-ip c-port)
          stream (usocket:socket-stream socket))))
  
(defmethod s-midi ((conn tcp-connection) nnum onoff &optional (channel 1) (loudness 120))
  (let ((stri "noteon "))
    (when (equal onoff 0) (setf stri "noteoff "))
    (setf stri (concatenate 'string stri (write-to-string channel) " " (write-to-string nnum) " " (write-to-string loudness)))
    (print stri)
    (format (c-strm conn) "~A~%~%" stri)
    (force-output (c-strm conn))))

(defparameter *con* (make-instance 'tcp-connection))

;(s-midi *con* 36 1)
;(s-midi *con* 60 1)

;(s-midi *con* 36 0)
;(s-midi *con* 60 0)

;(close-connection *con*)

(defun make-midi-command (ichunk iconn)
  (let ((newtime (mp-time))
        (time (midi-pars-type-time *midi-pars*))  
        (tb (midi-pars-type-tb *midi-pars*))
	(t1 (midi-pars-type-t1 *midi-pars*))
	(t2 (midi-pars-type-t2 *midi-pars*))
	(t3 (midi-pars-type-t3 *midi-pars*))
	)
    (setf (midi-pars-type-time *midi-pars*) newtime)
    (s-midi iconn tb 0)
    (s-midi iconn t1 0)
    (s-midi iconn t2 0)
    (s-midi iconn t3 0)
    (setf tb (if (null (chunk-slot-value-fct ichunk 'bn)) 0
		 (+ 36 (chunk-slot-value-fct (chunk-slot-value-fct ichunk 'bn) 'cnum))))
    (setf t1 (if (null (chunk-slot-value-fct ichunk 'r1)) 0
		 (+ 36 (chunk-slot-value-fct (chunk-slot-value-fct ichunk 'r1) 'cnum))))
    (setf t2 (if (null (chunk-slot-value-fct ichunk 'r2)) 0
		 (+ 36 (chunk-slot-value-fct (chunk-slot-value-fct ichunk 'r2) 'cnum))))
    (setf t3 (if (null (chunk-slot-value-fct ichunk 'r3)) 0
		 (+ 36 (chunk-slot-value-fct (chunk-slot-value-fct ichunk 'r3) 'cnum))))
    (when (< 0 tb) (s-midi iconn tb 1))
    (when (< 0 t1) (s-midi iconn t1 1))
    (when (< 0 t2) (s-midi iconn t2 1))
    (when (< 0 t3) (s-midi iconn t3 1))
    (setf (midi-pars-type-tb *midi-pars*) tb
          (midi-pars-type-t1 *midi-pars*) t1
          (midi-pars-type-t2 *midi-pars*) t2
          (midi-pars-type-t3 *midi-pars*) t3)))
	

(defun make-midi-command-bn (ichunk iconn)
  (let ((newtime (mp-time))
        (time (midi-pars-type-time *midi-pars*))  
        (tb (midi-pars-type-tb *midi-pars*)))
    (setf (midi-pars-type-time *midi-pars*) newtime)
    (s-midi iconn tb 0)
    (setf tb (if (null ichunk) 0 (+ 36 (chunk-slot-value-fct ichunk 'cnum))))
    (s-midi iconn tb 1)
    (setf (midi-pars-type-tb *midi-pars*) tb)))
