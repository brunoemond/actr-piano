;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; visicon-object.lisp
;;;
;;; 2025-10-02
;;;
#|
A visicon-object is a surface that can be perceived by actr as a set of visicon-entry.
|#

;;; surface 
(defclass surface (typep-slots)
  ((xy :type xy-coordinate :initform '(0 0) :initarg :xy :reader xy)
   (wh :type xy-coordinate :initform '(1 1) :initarg :wh :reader wh)
   (on-surface :type (null-or-type surface) :initform nil :initarg :on-surface :reader on-surface)
   (surface-collection :type (set-of surface) :initform nil :initarg :surface-collection :reader surface-collection)))

(defmethod x ((instance list)) (first instance))
(defmethod x ((instance simple-vector)) (aref instance 0))
(defmethod x ((instance surface)) (x (xy instance)))

(defmethod y ((instance list)) (second instance))
(defmethod y ((instance simple-vector)) (aref instance 1))
(defmethod y ((instance surface)) (y (xy instance)))


(defun w (surface) (first (wh surface)))
(defun h (surface) (second (wh surface)))

(defmethod coordinate= (coor1 coor2)
  (and (typep coor1 'xy-coordinate)
       (typep coor2 'xy-coordinate)
       (and (eq (x coor1) (x coor2))
            (eq (y coor1) (y coor2)))))

(defmethod coordinate= ((s1 surface) (s2 surface))
  (coordinate= (xy s1) (xy s2)))

(defmethod (setf surface-collection) ((collection-element surface) (instance surface))
  (with-slots (surface-collection) instance
    (setf (slot-value collection-element 'on-surface) instance
          surface-collection (adjoin collection-element surface-collection :test #'coordinate=))))

(defmethod (setf surface-collection) ((collection-elements list) (instance surface))
  (dolist (collection-element collection-elements (surface-collection instance))
    (setf (surface-collection instance) collection-element)))
 
(defmethod adjust-x ((instance null)) 0)
(defmethod adjust-y ((instance null)) 0)

(defmethod adjust-x ((instance surface)) 
  (+ (adjust-x (on-surface instance))
     (x instance)))

(defmethod adjust-y ((instance surface)) 
  (+ (adjust-y (on-surface instance))
     (y instance)))

(defmethod adjust-xy ((instance surface))
  (list (adjust-x instance)
        (adjust-y instance)))

(defmethod adjust-w ((instance null)) 0)
(defmethod adjust-h ((instance null)) 0)

;; to do adjust-w, adjust-h
(defmethod adjust-w ((instance surface)) 
  (w instance))

(defmethod adjust-h ((instance surface)) 
  (h instance))

(defmethod adjust-wh ((instance surface))
  (list (adjust-w instance)
        (adjust-h instance)))

;;; visicon-object
(defclass visicon-object (surface)
  ((distance :type (null-or-type number) :initform nil :initarg :distance :reader distance)
   (color :type empty-or-symbol :initform +empty+ :initarg :color :reader color)
   (value :type empty-or-symbol :initform +empty+ :initarg :value :reader value)
   (feature-id :type symbol :initform nil :reader feature-id)
   (visual-location :type symbol :initform nil :reader visual-location)
   (visual-features :type (set-of symbol) :initform nil :initarg :visual-features :reader visual-features)))

(defmethod screen-x ((instance visicon-object)) (adjust-x instance))
(defmethod screen-y ((instance visicon-object)) (adjust-y instance))
(defmethod width ((instance visicon-object)) (w instance))
(defmethod height ((instance visicon-object)) (h instance))
(defmethod kind ((instance visicon-object)) "Set using actr computed value."  (declare (ignore instance)) nil) ; "Set using actr computed value."  (declare (ignore instance)) nil)
(defmethod size ((instance visicon-object)) "Set using actr computed value." (declare (ignore instance)) nil)
(defmethod status ((instance visicon-object)) "Set using actr computed value." (declare (ignore instance)) nil)
(defmethod screen-pos ((instance visicon-object)) "Set using actr computed value." (declare (ignore instance)) nil)

(defun visloc-type (visicon-object)
  (intern (format nil "~S-FEATURE" (type-of visicon-object))))

(defun visobj-type (visicon-object)
  (type-of visicon-object))

(defconstant +actr-features+
  `(; visual-location
    screen-x screen-y distance kind size
    ; visual-location & visual-object
    value color height width
    ; visual-object
    screen-pos status))

(defun slot->feature (visicon-object slot-name)
  (if (fboundp slot-name)
      (let ((slot-value (apply slot-name (list visicon-object))))
        (cond (slot-value
               (list slot-name slot-value))
              ((not (member slot-name +actr-features+))
               (list slot-name +empty+))))   
    (error "There is no reader method ~S for object ~S." slot-name visicon-object)))

(defun object->features (visicon-object slot-names)
  (let (features-set)
    (dolist (slot-name slot-names features-set)
      (setf features-set (append features-set 
                                 (slot->feature visicon-object slot-name))))))

(defun visual-object-features (visicon-object &key isa)
  (with-slots (visual-features) visicon-object
    (append (when isa
              (list 'isa (list (visloc-type visicon-object) (visobj-type visicon-object))))
            (object->features visicon-object +actr-features+)
            (object->features visicon-object visual-features))))

(defun chunk-types-p (visicon-object)
  (and (chunk-type-p-fct (visloc-type visicon-object))
       (chunk-type-p-fct (visobj-type visicon-object))))

(defun define-chunk-types (visicon-object)
  (unless (chunk-types-p visicon-object)
    (let (chunks-defs)
      (dolist (chunk-type-spec `(((,(visloc-type visicon-object) (:include visual-location)))
                                 ((,(visobj-type visicon-object) (:include visual-object)))) 
                               chunks-defs)
        (let ((features (copy-list (slot-value visicon-object 'visual-features))))
          (push (chunk-type-fct (append chunk-type-spec features)) chunks-defs))))))

; to do: parse features and add chunks for slot values to avoid message:
; Warning: Creating chunk ABC with no slots

(defun set-visicon-ids (visicon-object feat-id)
  (with-slots (feature-id visual-location) visicon-object
    (setf feature-id feat-id
          visual-location (chunk-visual-loc feat-id))))

(defmethod add-to-visicon ((object visicon-object))
  (define-chunk-types object)
  (unless (chunk-p-fct (visobj-type object))
    (define-chunks-fct (list (visobj-type object))))
  (dolist (colection-element (surface-collection object))
    (add-to-visicon colection-element))
  (values 
   (set-visicon-ids object (car (add-visicon-features (visual-object-features object :isa t))))
   object))

(add-act-r-command "add-to-visicon" 'add-to-visicon)

(defmethod delete-from-visicon ((object visicon-object))
  (with-slots (feature-id visual-location) object 
    (when (member feature-id (visicon (get-module :vision)))
      (delete-visicon-features feature-id))
    (dolist (colection-element (surface-collection object))
      (delete-from-visicon colection-element))
    (setf feature-id nil
          visual-location nil)))

(add-act-r-command "delete-from-visicon" 'delete-from-visicon)

(defmethod modify-visicon ((object visicon-object))
  (let ((features (visual-object-features object))
        (feature-id (slot-value object 'feature-id)))
    (when feature-id
      (modify-visicon-features 
       (append (list feature-id) features)))))

(add-act-r-command "modify-visicon" 'modify-visicon)

;; to do 
;; find-visicon-object: traverse the hierarchy of object collections 
;; until a predicate is true of an object. 


;;; eof