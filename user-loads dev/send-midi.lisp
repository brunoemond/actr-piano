;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;;
;;; send-midi.lisp
;;;
;;; 2025-10-29
;;;
(defparameter *sendmidi* "/usr/local/bin/sendmidi"
  "Binary application location.")

(defparameter *midi-destination* "IAC Driver Bus 1"
  "On MacOS: Inter-Application Communication Bus 1.")

(defun run-shell-command (shell-command)
  #+lispworks
  (sys:run-shell-command shell-command)
  #+sbcl
  (uiop:run-program shell-command))

(defun note-on (midi &optional (velocity 100))
  (run-shell-command
   (format nil "~A dev ~S on ~S ~S" 
           *sendmidi* *midi-destination* 
           midi velocity)))

(defun note-off (midi) 
  (run-shell-command 
   (format nil "~A dev ~S on ~S 0" 
           *sendmidi* *midi-destination* 
           midi)))

(defun note-off2 (midi &optional (velocity 100)) 
  (run-shell-command
   (format nil "~A dev ~S off ~S ~S" 
           *sendmidi* *midi-destination* 
           midi velocity)))


(defun test ()
  (let ((notes '(60 64 67)))
    (dolist (note notes)
      (note-on note 100))
    (sleep 1.0)
    (dolist (note notes)
      (note-off note))))


(defun open-fluidsynth-socket ()
  (comm:open-tcp-stream "127.0.0.1" 9800))


(defun start-fluidsynth (&key (host "127.0.0.1") (service 9800))
  (run-shell-command "fluidsynth -s -v midi.portname=\"FluidSynth virtual port\"")
  (sleep .5)
  (comm:open-tcp-stream host service))

;; noteon 0 60 100
;; fluidsynth -s -v -p "FluidSynth virtual port"
;; fluidsynth -s -v -a coreaudio -m coremidi -p "IAC Driver Bus 1"
;; "IAC Driver Bus 1"
;;; eof