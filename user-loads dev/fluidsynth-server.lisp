;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;;
;;; fluidsynth-server.lisp
;;;
;;; 2026-02-03
;;;

(ql:quickload :usocket)

(defstruct fs-parameters 
  (bin "/opt/homebrew/Cellar/fluid-synth/2.5.2/bin/fluidsynth")
  (sound-font "/library/audio/sounds/banks/FluidR3_GM/FluidR3_GM.sf2")
  (host "127.0.0.1") (port 9800) pid)

(defparameter *fs* (make-fs-parameters))

(defun fs-start-command ()
  (format nil "~a -a coreaudio -g 1.0 -s -i -o shell.port=~a ~a"
          (fs-parameters-bin *fs*)
          (fs-parameters-port *fs*)
          (fs-parameters-sound-font *fs*)))

(defun fs-start ()
  (let ((pid (system:run-shell-command (fs-start-command) :wait nil)))
    (if pid 
        (setf (fs-parameters-pid *fs*) pid)
      (error "Unable to start FluidSynth with parameters ~S." *fs*))))
#|
(defun fs-alive-command ()
  (format nil "kill -0 ~a" (fs-parameters-pid *fs*)))

(defun fs-alive ()
  (let ((res (system:run-shell-command (fs-alive-command))))
    (if res res
      (error "Unable to kill FluidSynth with pid ~S." (fs-parameters-pid *fs*)))))
|#

(defun fs-kill-command ()
  (format nil "kill ~a" (fs-parameters-pid *fs*)))

(defun fs-quit ()
  (let ((res (system:run-shell-command (fs-kill-command))))
    (if (eq res 0) res
      (error "Unable to kill FluidSynth with pid ~S." (fs-parameters-pid *fs*)))))
                  
(defun fs-send (command)
  (usocket:with-client-socket 
      (sock stream 
            (fs-parameters-host *fs*) 
            (fs-parameters-port *fs*))
    (declare (ignore sock))
    (write-string command stream)
    (finish-output stream)
    t))

(defun fs-noteoff (key)
  (fs-send (format nil "noteoff 0 ~S" key)))

(defun fs-noteon (key &key (vel 100) dur)
  (fs-send (format nil "noteon 0 ~S ~S" key vel))
  (if dur 
      (progn (sleep dur) 
        (fs-noteoff key))
    t))





(defun fs-chord (host port keys &key (chan 0) (vel 100) (dur 0.5))
  (fs-tcp-send host port
               (with-output-to-string (s)
                 (dolist (k keys)
                   (format s "noteon ~d ~d ~d~%" chan k vel))))
  (sleep dur)
  (fs-tcp-send host port
               (with-output-to-string (s)
                 (dolist (k keys)
                   (format s "noteoff ~d ~d~%" chan k)))))



(defparameter *fs* nil)
(defconstant *fs-bin* "/opt/homebrew/Cellar/fluid-synth/2.5.2/bin/fluidsynth")
(defconstant *soundfont* "/library/audio/sounds/banks/FluidR3_GM/FluidR3_GM.sf2")

(defun read-all-chars (stream)
  (do* ((char (read-char-no-hang stream) (read-char-no-hang stream))
        (chars (list char) (if char (append chars (list char)) chars)))
       ((null char) chars)))

(defun fs-stream-p (stream)
  (typep stream 'system::pty-stream))

(defun kill-fs-clean ()
  (let ((process (mp:get-process "clean-stream")))
    (when process (mp:process-kill process))))

(defun fs-send (command)
  (when (fs-stream-p *fs*)
    (write-line command *fs*) 
    (finish-output *fs*)
    t))

(defun fs-quit ()
  (kill-fs-clean)
  (fs-send "quit"))

(defun fs-start ()
  (when (fs-stream-p *fs*)
    (fs-quit))
  (let ((fs (sys:open-pipe
             (format nil "~a -a coreaudio -g 1.0 -o audio.period-size=256 -o audio.periods=8 ~a"
                     *fs-bin* *soundfont*)
             :direction :io
             :save-exit-status t
             :use-pty t)))
    (mp:process-run-function 
     "clean-stream" ()  
     (lambda () (loop (read-all-chars fs))))
    (setf *fs* fs)))

(defun fs-noteoff (key &key (chan 0))
  (fs-send (format nil "noteoff ~S ~S" chan key)))

(defun fs-noteon (key &key (chan 0) (vel 100) dur)
  (fs-send (format nil "noteon ~S ~S ~S" chan key vel))
  (when dur 
    (sleep dur)
    (fs-noteoff key :chan chan))
  t)
  

#|
(fs-start)
(progn (fs-noteon 60) (sleep .25) (fs-noteoff 60))
(progn (fs-noteon 60 :dur .25))


(progn (fs-noteon 60) (fs-noteon 64) (fs-noteon 67))
(progn (fs-noteon 60 :dur .25) (fs-noteon 64 :dur .25) (fs-noteon 67 :dur .25))
|#