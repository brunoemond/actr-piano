;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; device.lisp
;;;
;;; 2025-09-29
;;;
;;;
#|
The interactive-object code aims to facilitate the creation and use of interactive objects (devices) 
as part of ACT-R simulations. Interactive objects can be perceived, acted upon, or both. From the ACT-R
software architecture, interactive objects are components which can be interfaced with perceptual 
and motor modules. The interactive object class and methods facilitate the creation of devices 
that do not have features associated with other devices such as experimental windows, computer 
keyboards, mice, and microphones.

Definitions (from ACT-R 7.30 Reference Manual)

++ Component
A component is a software construct used to build the system that implements the ACT-R theory, but
which has no basis in the theory. A component is a data structure which is instantiated once in the
software and can provide commands which will be called automatically when clear-all, reset, model
definition, and model deletion occur. Currently, the interface for creating and working with
components is only an internal mechanisms and is not documented for user use.

++ Devices
The act-r simulation already includes a set of devices that can be used to have a cognitive model
interfact with objects such as an experimental window, a computer a keyboard, a mouse, and a microphone. 

++ Interface
Interfaces link a device to one or many modules. which support act-r distributed system design.
|#

;;;
;;; utilities
;;;
(defmethod ->symbol ((object string)) (intern (string-upcase object)))
(defmethod ->symbol ((object symbol)) object)

(defmethod ->string ((object string)) object)
(defmethod ->string ((object symbol)) (write-to-string object :case :downcase))

;;; types
(defun isa-list-of-strings (list)
  (every #'stringp list))

(deftype list-of-strings ()
  `(satisfies isa-list-of-strings))

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
;;; device
;;;
(defclass device () 
  ((interfaces :initarg :interfaces :initform nil :reader interfaces)))

(defclass vision-device (device)
  ()
  (:default-initargs
   :interfaces '("vision")))

(defclass motor-device (device)
  ()
  (:default-initargs
   :interfaces '("motor")))

(defclass vision-motor-device (device)
  ()
  (:default-initargs
   :interfaces '("vision" "motor")))

;;;
;;; act-r-device-with-instance
;;;

(defstruct (device-with-instance (:include act-r-device)) instance)

(defun instance (device)
  (if (typep device 'device)
      (device-with-instance-instance device)
    (warn "Device ~S does not have an instance slot." device)))

(defun isa-device (device-name)
  (let ((di *default-device-interface*))
    (bt:with-lock-held ((device-tables-lock di))
      (multiple-value-bind (device defined)
          (gethash (->string device-name) (device-table di))
        (if defined (instance device)
          (warn "Unknown device ~S." device-name))))))

(defmethod (setf isa-device) ((instance null) device-name)
  (undefine-device (->string device-name)))

(defmethod apply-notification ((devlist list) (features list))
  (apply-notification (isa-device (devlist-device devlist)) 
                      features))

(defmethod apply-notification ((instance device) (features list))
  (let ((function (getf features :command)))
    (if function 
        (progn (remf features :command)
          (apply 'dispatch-apply (append (list function instance) features)))
      (warn "Cannot apply a notification. Features list ~S does not contain a ':command' feature." features))))

(add-act-r-command "apply-notification" 'apply-notification "Applies a features function to a device. Params: device-list 'features'" nil)

(defmethod (setf isa-device) (instance device-name)
  (let ((di *default-device-interface*)
        (device (make-device-with-instance :instance instance :notification "apply-notification")))
    (when (define-device device-name)
      (bt:with-lock-held ((device-tables-lock di))
        (dolist (interface (interfaces device)
                           (setf (gethash device-name (device-table di)) device))
          (install-device (list interface device-name)))))))





#|
(defclass test () ((x :initform 2 :accessor x)))

(add-act-r-command "x" 'x )

(defun x-m (name params success result)
  (print (list name params success result)))

(add-act-r-command "x-m" 'x-m )

(monitor-act-r-command "x" "x-m" :after)


(define-model test)
(setf (isa-device "test") (make-instance 'test))
(install-device '("motor" "test"))

(notity-device '("motor" "test") '(:command "x"))

(remove-act-r-command "x")
(remove-act-r-command "x-m")


(progn
  (clear-all)
  (setf (isa-component 'test) nil)
  (setf (isa-device "test") nil)

  ;; Define a component named 'test as an interactive object instance
  (setf (isa-component 'test) (make-instance 'interactive-object))

  ; There is a component named 'test
  (assert (null (isa-device "test")))

  (setf (isa-device "test") 'test)

  ;(setf (isa-component 'test) nil)
  ;(setf (isa-device "test") nil)
  )

|#





;;; eof