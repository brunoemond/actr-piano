;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; device-interface.lisp
;;;
;;; 2025-10-01
;;;
;;;

;;;
;;; device-list
;;;
(defun devlist-interface (device-list)
  (first device-list))

(defun devlist-device (device-list)
  (second device-list))

(defun devlist-details (device-list)
  (third device-list))


;;;
;;; pm-device
;;;
;(defclass vision-device ()
;  (visicon-id visicon-features))


; current-devices interface)
; add a function to test it a device is installed for an interface
; 


;;;
;;; device-interface tables
;;;
(defmacro with-di-lock (&body body)
  `(let ((di *default-device-interface*))
     (bt:with-lock-held ((device-tables-lock di))
       (progn ,@body))))

(defun describe-devices ()
  (describe (device-table *default-device-interface*)))

(defun describe-interfaces ()
  (describe (interface-table *default-device-interface*)))


;;;
;;; device and interface notification 
;;; (notify-device device-list features)
;;; (notify-interface name features)

(defun device-notification-command (device-name)
  (with-di-lock
    (multiple-value-bind (device defined)
        (gethash (->string device-name) (device-table di))
      (if defined (act-r-device-notification device)
        (warn "Unknown device ~S." device-name)))))

(defun act-r-commandp (name)
  (let ((command (gethash name (dispatcher-command-table *dispatcher*))))
    (and command (fboundp (dispatch-command-underlying-function command)))))

(defmethod (setf device-notification-command) ((command-name string) (device-name string))
  (if (act-r-commandp command-name)
      (with-di-lock
        (multiple-value-bind (device defined)
            (gethash (->string device-name) (device-table di))
          (if defined (setf (act-r-device-notification device) command-name)
            (warn "Unknown device ~S." device-name))))
    (warn "Unknown actr-command ~S." command-name)))

(defun interface-notification-command (interface-name)
  (with-di-lock
    (multiple-value-bind (interface defined)
        (gethash (->string interface-name) (interface-table di))
      (if defined (interface-notification interface)
        (warn "Unknown inerface ~S." interface-name)))))

(defmethod (setf interface-notification-command) ((command-name string) (interface-name string))
  (if (act-r-commandp command-name)
      (with-di-lock
        (multiple-value-bind (interface defined)
            (gethash (->string interface-name) (interface-table di))
          (if defined (setf (interface-notification interface) command-name)
            (warn "Unknown interface ~S." interface-name))))
    (warn "Unknown actr-command ~S." command-name)))

;;;
;;; device-with-instance
;;;
(defstruct (device-with-instance (:include act-r-device)) instance)

(defmethod device-instance ((device device-with-instance))
  (device-with-instance-instance device))

(defmethod device-instance ((device-name string))
  (with-di-lock
    (multiple-value-bind (device defined)
        (gethash (->string device-name) (device-table di))
      (if defined (device-instance device)
        (warn "Unknown device ~S." device-name)))))

(defmethod (setf device-instance) ((instance null) device-name)
  (undefine-device (->string device-name)))

(defmethod (setf device-instance) ((instance standard-object) device-name)
  (when (define-device device-name)
    (with-di-lock
      (let ((device (make-device-with-instance
                     :instance instance 
                     :notification "apply-device-command")))
        (setf (gethash device-name (device-table di)) device)))))

;; apply-device-command is called by notify-device which checks if the devide is installed for an interface
;; (notify-device device-list feature)

(defmethod apply-device-command ((instance standard-object) (features list))
  (let ((command-name (getf features :command)))
    (if command-name 
        (apply 'dispatch-apply (append (list command-name instance) 
                                       (remove-from-plist features :command)))
      (warn "Cannot apply a device command. Features list ~S does not contain a ':command' feature." features))))

(defmethod apply-device-command ((device-name string) (features list))
  (apply-device-command (device-instance device-name) features))

(defmethod apply-device-command ((devlist list) (features list))
  (apply-device-command (devlist-device devlist) features))

(add-act-r-command "apply-device-command" 'apply-device-command)

;;;
;;; interface-notifications
;;;

(defmethod interface-notification% ((interface-name string))
  (with-di-lock
    (interface-notification (gethash interface-name (interface-table di)))))

(defmethod (setf interface-notification%) ((command-name string) (interface-name string))
  (with-di-lock
    (setf (interface-notification (gethash interface-name (interface-table di)))
          command-name)))



(defun extend-interface-notification (params)
  (if (or (act-r-commandp (car params))
          (fboundp (car params)))
      (apply (car params) (cdr params))
    (print-warning "Unknown command ~S and parameters ~S." (car params) (cdr params))))

(defparameter *basic-motor-commands*
  '(set-hand-device set-hand-position set-finger-offset get-hand-device get-hand-position get-finger-position))
                    
(defun apply-motor-command (params)
  (if (member (car params) *basic-motor-commands*)
      (motor-interface params)
    (progn
      (print-warning "Extending motor-interface notification with action ~S and parameters ~S."
                     (car params) (cdr params))
      (extend-interface-notification params))))

(add-act-r-command "apply-motor-command" 'apply-motor-command)

(setf (interface-notification% "motor") 
      "apply-motor-command")

(defun apply-vision-command (params)
  (extend-interface-notification params))

(add-act-r-command "apply-vision-command" 'apply-vision-command)

(setf (interface-notification% "vision") 
      "apply-vision-command")



;;; eof