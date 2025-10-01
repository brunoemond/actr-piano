;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; device-interface.lisp
;;;
;;; 2025-10-01
;;;
;;;


;;;
;;; utilities
;;;
(defmethod ->symbol ((object string)) (intern (string-upcase object)))
(defmethod ->symbol ((object symbol)) object)

(defmethod ->string ((object string)) object)
(defmethod ->string ((object symbol)) (write-to-string object :case :downcase))

(defun remove-from-plist (plist key)
  (loop for (k v) on plist by #'cddr
        unless (eq k key)
        append (list k v)))

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
;;; device-interface
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

(defmethod apply-device-command ((instance standard-object) (features list))
  (let ((command-name (getf features :command)))
    (if command-name 
        (apply 'dispatch-apply (append (list command-name instance) 
                                       (remove-from-plist features :command)))
      (warn "Cannot apply a device command. Features list ~S does not contain a ':command' feature." features))))

(defmethod apply-device-command ((device-name string) (features list))
  (apply-device-command (device-instance device-name) features))

(add-act-r-command "apply-device-command" 'apply-device-command "Applies a features command to a device. Params: device-list 'features'" nil)



;;; eof