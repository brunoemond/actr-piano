;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; cognitive-object.lisp
;;;
;;; 2025-09-10
;;;
;;;
#|
The cognitive-object code aims to facilitate the creation and use of cognitive objects (devices) 
as part of ACT-R simulations. Cognitive objects can be perceived, acted upon, or both. From the ACT-R
software architecture, cognitive objects are components which can be interfaced with perceptual 
and motor modules. The cognitive object class and methods facilitate the creation of devices 
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

;;;
;;; cognitive-object
;;;
(defclass cognitive-object (typep-slots)
  ((name :initarg :device-name :initform 'cognitive-object :reader name :type symbol)
   (version :initarg :version :initform "1.0" :reader version :type string)
   (doc :initarg :documentation :initform "Not documented." :reader doc :type string)))

(defmethod device-name ((instance cognitive-object))
  (->string (name instance)))

(defmethod component-name ((instance cognitive-object))
  (name instance))

(defun component-p (name)
  (let ((mp (current-mp)))
    (aif (assoc name (bt:with-lock-held ((meta-p-component-lock mp))
                       (meta-p-component-list mp)))
       (values (act-r-component-instance (cdr it)) t)
         (values (print-warning "~s does not name a current component." name) nil))))

(defun isa-component (name)
  (get-component-fct (->symbol name)))

(defmethod (setf isa-component) ((instance cognitive-object) name)
  (when (isa-component name)
    (undefine-component-fct name))
  (define-component-fct 
   name 
   :version (version instance) 
   :documentation (doc instance)
   :creation (lambda () instance)
     ;:delete (lambda () nil)
     ;:clear-all (lambda () nil)
     ;:create-model (lambda () nil)
     ;:delete-model (lambda () nil)
     ;:before-reset (lambda () nil)
     ;:after-reset (lambda () nil)
   ))

(progn
  
  (assert (null (isa-component 'test)))

  (setf (isa-component 'test)
        (make-instance 'cognitive-object))

  (assert (isa-component 'test))

  (assert (null (isa-component 'test)))
  
  (define-model test)
  (delete-model test))




;;; eof