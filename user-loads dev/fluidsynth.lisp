;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;;
;;; fluidsynth.lisp
;;;
;;; 2026-02-03
;;;

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