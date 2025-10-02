;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; typep-slots.lisp
;;;
;;; 2025-08-15
;;;
(defclass typep-slots () ())

(defmethod typep-slot-value ((object typep-slots) (slot-name symbol))
  (slot-value object slot-name))

#+lispworks
(defun slot-type (standard-object slot-name)
  (let ((slot-definition (find slot-name (class-slots (class-of standard-object)) :key 'slot-definition-name)))
    (if slot-definition
        (slot-definition-type slot-definition)
      (error "Unknown slot name ~S in object ~S." slot-name standard-object))))

#+lispworks
(defmethod (setf typep-slot-value) (value (object typep-slots) (slot-name symbol))
  (if (typep value (slot-type object slot-name))
      (setf (slot-value object slot-name) value)
    (error "~S is not of type ~S for slot ~S in object ~S." 
           value (slot-type object slot-name) slot-name object)))
#+sbcl
(defmethod (setf typep-slot-value) (value (object typep-slots) (slot-name symbol))
  (setf (slot-value object slot-name) value))

(defun slots-macrolet (typep-slots-instance slot-names)
  (let (slots-macrolet)
    (dolist (slot-name slot-names slots-macrolet)
      (setf slots-macrolet 
            (append slots-macrolet
                    `((,slot-name (typep-slot-value ,typep-slots-instance (quote ,slot-name)))))))))
                  
(defmacro with-typep-slots ((&rest slot-names) typep-slots-instance &body body)
  (let ((objsym (gensym)))
    `(let ((,objsym ,typep-slots-instance))
       (declare (ignorable ,objsym))
       (symbol-macrolet ,(slots-macrolet objsym slot-names) ,@body))))

(defmethod initialize-instance :after ((object typep-slots) &rest initargs &key &allow-other-keys)
  (declare (ignore object initargs)))

#+lispworks
(defadvice ((method initialize-instance :after (typep-slots)) initialize-type-slots :after)
    (instance &rest initargs) 
  (declare (ignore initargs))
  (dolist (slot-definition (class-slots (class-of instance)) instance)
    (let* ((slot-name (slot-definition-name slot-definition))
           (type (slot-type instance slot-name))
           (boundp (slot-boundp instance slot-name)))
      (cond ((eq t type) t)
            ((and boundp (not (typep (slot-value instance slot-name) type)))
             (error "When initializing object, ~S is not of type ~S for slot ~S in object ~S." 
                    (slot-value instance slot-name) type slot-name instance))
            ((not boundp)
             (error "When initializing object, ~S is not of type ~S for slot ~S in object ~S." 
                    (make-condition 'unbound-slot) type slot-name instance))))))

;;; eof