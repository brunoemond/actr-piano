;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; interactive-object.lisp
;;;
;;; 2025-09-10
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
;;; interactive-object
;;;
(defclass interactive-object (typep-slots)
  ((name :initarg :name :initform 'interactive-object :reader name :type symbol)
   (version :initarg :version :initform "1.0" :reader version :type string)
   (doc :initarg :documentation :initform "Not documented." :reader doc :type string)
   (interfaces :initarg :interfaces :initform nil :type list-of-strings :reader interfaces)))
   
;;;
;;; interactive-objects as components.
;;;
(defun isa-component (component-name)
  (get-component-fct (->symbol component-name)))

(defmethod (setf isa-component) ((instance null) (component-name symbol))
  (undefine-component-fct component-name))

(defmethod (setf isa-component) ((instance interactive-object) (component-name symbol)) 
  (define-component-fct 
   component-name
   :version (version instance) 
   :documentation (doc instance)
   :creation (lambda () instance)
   :delete nil
   :clear-all nil
   :create-model nil
   :delete-model nil
   :before-reset nil
   :after-reset nil))

(progn
  (setf (isa-component 'test) nil)

  ; There is no component named 'test
  (assert (null (isa-component 'test)))

  ;; Define a component named 'test as an interactive object instance
  (setf (isa-component 'test) (make-instance 'interactive-object))

  ; There is a component named 'test
  (assert (isa-component 'test))

  ;; Undefine the component named 'test
  (setf (isa-component 'test) nil)

  )

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
;;; interactive-object components as devices
;;;
(defun isa-device (device-name)
  (and (member (->string device-name) (defined-devices) :test #'equalp)
       (isa-component (->symbol device-name))))

(defmethod (setf isa-device) ((instance null) device-name)
  (undefine-device (->string device-name)))


(




;; 5 methods can be associated to a device
;; initialize, remove, notify, set-hand, unset-hands

;; initialize-object returns the component associated to the device-name
(defmethod initialize-device ((instance interactive-object))
  instance)

(defmethod initialize-device ((device-list list))
  ;; assumes that the component symbol name is the same as the device name. 
  (initialize-object (isa-component (devlist-device device-list))))











(add-act-r-command "initialize-device" 'initialize-device 
                   "Generic method to return a component from a device-name. Do not call directly.")

(defmethod (setf isa-device) ((component-name symbol) device-name)
  (if (eq component-name (->symbol device-name))
      (define-device device-name 
                     "initialize-object" nil nil)))


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


;;; eof