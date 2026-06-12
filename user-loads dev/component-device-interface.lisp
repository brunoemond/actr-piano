;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; component-device-interface.lisp
;;;
;;; 2025-05-31
;;;
#|
 component-device-interface contains a set of objects and methods to interface with 
some actr-r own framework elements including: components, devices and interfaces. 

|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities
;;;
(defmethod copy-object ((object symbol)) object)
(defmethod copy-object ((object number)) object)
(defmethod copy-object ((object string)) object)

(defmethod copy-object ((object list)) 
  (mapcar #'copy-object object))

(defmethod copy-object ((object simple-vector)) 
  (apply #'vector (coerce object 'list)))

(defmethod copy-object ((object hash-table))
  (let ((copy (make-hash-table :test #'equalp)))
    (maphash (lambda (key value)
               (setf (gethash key copy) (copy-object value)))
             object)
    copy))

(defmethod copy-object ((object standard-object))
  (let* ((class (class-of object))
         (slots (mapcar 'slot-definition-name (class-slots class)))
         (copy (make-instance class)))
    (dolist (slot slots copy)
      (setf (slot-value copy slot)
            (copy-object (slot-value object slot))))))
    

;;; conversions
(defmethod ->string ((object string)) object)
(defmethod ->string ((object symbol)) (write-to-string object :case :downcase))

(defmethod ->symbol ((object string)) (intern (string-upcase object)))
(defmethod ->symbol ((object symbol)) object)
(defmethod ->symbol ((devlist list)) (->symbol (second devlist)))

(defmethod ->vector ((object simple-vector)) object)
(defmethod ->vector ((object list)) (apply #'vector object))

(defmethod ->list ((object simple-vector)) (coerce object 'list))
(defmethod ->list ((object list)) object)

;;; position (xy), size (w h), rectangle
(defmethod first-of ((object list)) (first object))
(defmethod second-of ((object list)) (second object))
(defmethod first-of ((object simple-vector)) (svref object 0))
(defmethod second-of ((object simple-vector)) (svref object 1))

(eval-when (:compile-toplevel)
  (let ((xy '(1 2))
        (v (vector 1 2)))
    (assert (eq 1 (first-of xy)))
    (assert (eq 2 (second-of xy)))
    (assert (eq 1 (first-of v)))
    (assert (eq 2 (second-of v)))))

;;; vector

(defun vector+ (v1 v2)
  (vector (+ (first-of v1) (first-of v2)) 
          (+ (second-of v1) (second-of v2))))

(defun vector- (v1 v2)
  (vector (- (first-of v1) (first-of v2)) 
          (- (second-of v1) (second-of v2))))

;;; actr hand-pos utilities

(defconstant *hand-symbols* '(left right))
(defconstant *finger-symbols* '(thumb index middle ring pinkie))

(defun validate-hand (hand-name)
  (member hand-name *hand-symbols*))

(defun validate-finger (finger-name)
  (member finger-name *finger-symbols*))

(defun validate-hand-finger (hand-finger)
  (if (and (validate-hand (first hand-finger))
           (validate-finger (second hand-finger)))
      hand-finger
    (error "Hand-finger ~s must combine one of ~S and one of ~S." 
           hand-finger *hand-symbols* *finger-symbols*)))

(defun offset-xs (hand finger)
  (ecase hand
    (left (ecase finger
            (thumb  '(0 -1 -2 -3 -4))
            (index  '(1  0 -1 -2 -3))
            (middle '(2  1  0 -1 -2))
            (ring   '(3  2  1  0 -1))
            (pinkie '(4  3  2  1  0))))
    (right (ecase finger
             (thumb  '( 0  1  2  3 4))
             (index  '(-1  0  1  2 3)) 
             (middle '(-2 -1  0  1 2))
             (ring   '(-3 -2 -1  0 1))
             (pinkie '(-4 -3 -2 -1 0))))))

(defun finger-offsets (hand-name finger-name)
  (do* ((f *finger-symbols* (cdr f))
        (xs (offset-xs hand-name finger-name) (cdr xs))
        (offsets `((,(car f) ,(vector (car xs) 0)))
                 (append offsets `((,(car f) ,(vector (car xs) 0))))))
       ((null (cdr f)) offsets)))

(eval-when (:compile-toplevel)
  (assert (equalp (offset-xs 'left 'thumb) '(0 -1 -2 -3 -4)))
  (assert (equalp (finger-offsets 'right 'pinkie) 
                  '((THUMB #(-4 0)) (INDEX #(-3 0)) (MIDDLE #(-2 0)) (RING #(-1 0)) (PINKIE #(0 0))))))

(defun null-hand-pos-p (hand-pos)
  (or (null hand-pos)
      (and (hand-pos-p hand-pos)
           (null (hand-pos-loc hand-pos)))))

(defmethod hand-xy ((hand-pos hand-pos))
  (hand-pos-loc hand-pos))

(defmethod (setf hand-xy) (xy (hand-pos hand-pos))
  (setf (hand-pos-loc hand-pos) 
        (->vector xy)))

(defmethod fingers ((hand-pos hand-pos))
  (hand-pos-fingers hand-pos))

(defmethod (setf fingers) ((finger-offsets list) (hand-pos hand-pos))
  (setf (hand-pos-fingers hand-pos) finger-offsets))

(defmethod finger-offset ((hand-pos hand-pos) (finger-name symbol))
  (second (find finger-name (fingers hand-pos) :key #'first)))

(defmethod (setf finger-offset) (offset-xy (hand-pos hand-pos) (finger-name symbol))
  (setf (second (find finger-name (hand-pos-fingers hand-pos) :key #'first))
        (->vector offset-xy)))

(defun hand+offset (hand-xy finger-xy)
  (list (+ (first-of hand-xy) (first-of finger-xy))
        (+ (second-of hand-xy) (second-of finger-xy))))

(defmethod finger-xy ((hand-pos hand-pos) (finger-name symbol))
  (let ((hand-xy (hand-xy hand-pos))
        (finger-offset (finger-offset hand-pos finger-name)))
    (->vector (hand+offset hand-xy finger-offset))))

(defmethod (setf finger-xy) (finger-xy (hand-pos hand-pos) (finger-name symbol))
  (let ((hand-xy (hand-xy hand-pos)))
    (setf (finger-offset hand-pos finger-name)
          (vector (- (first-of finger-xy) (first-of hand-xy))
                  (- (second-of finger-xy) (second-of hand-xy))))
    (finger-xy hand-pos finger-name)))

(defun fingers-xy (hand-pos)
  (values (finger-xy hand-pos 'thumb) (finger-xy hand-pos 'index) (finger-xy hand-pos 'middle)
          (finger-xy hand-pos 'ring) (finger-xy hand-pos 'pinkie)))
          
(defun make-hand-position (hand-name anchor-finger xy &key thumb index middle ring pinkie)
  (let ((hand-pos (make-hand-pos :loc (->vector xy) 
                                 :fingers (finger-offsets hand-name anchor-finger))))
    (when thumb (setf (finger-offset hand-pos 'thumb) thumb))
    (when index (setf (finger-offset hand-pos 'index) index))
    (when middle (setf (finger-offset hand-pos 'middle) middle))
    (when ring (setf (finger-offset hand-pos 'ring) ring))
    (when pinkie (setf (finger-offset hand-pos 'pinkie) pinkie))
    hand-pos))

(defun make-hand-position-0 ()
  (make-hand-pos :loc (->vector '(0 0))
                 :fingers (mapcar (lambda (f) (list f (vector 0 0))) 
                                  *finger-symbols*)))
(defun hand-position-0-p (object)
  (and (hand-pos-p object)
       (equalp (hand-xy object) #(0 0))
       (every (lambda (f) (equalp (second f) #(0 0)))
              (fingers object))))


(eval-when (:compile-toplevel)
  (let ((hand-pos (make-hand-position 'right 'thumb '(0 0))))
    (assert (equalp (hand-xy hand-pos) #(0 0)))
    (assert (equalp (fingers hand-pos) '((THUMB #(0 0)) (INDEX #(1 0)) (MIDDLE #(2 0)) (RING #(3 0)) (PINKIE #(4 0))))))
  (multiple-value-bind (thumb index middle ring pinkie)
      (fingers-xy (make-hand-position 'right 'index '(2 1) :thumb '(-3 1) :middle '(2 0) :ring '(3 1) :pinkie '(5 1)))
    (assert (equalp thumb #(-1 2)))
    (assert (equalp index #(2 1)))
    (assert (equalp middle #(4 1)))
    (assert (equalp ring #(5 2)))
    (assert (equalp pinkie #(7 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; rect
;;;
(defclass rect ()
  ((xy :initform '(0 0) :initarg :xy :accessor xy)
   (wh :initform '(1 1) :reader wh)))

(defmethod print-object ((object rect) (stream stream))
  (print-unreadable-object (object stream :identity nil :type t)
    (format stream "~S ~S" (xy object) (wh object))))

(defun x (rect) (first-of (xy rect)))
(defun y (rect) (second-of (xy rect)))
(defun w (rect) (first-of (wh rect)))
(defun h (rect) (second-of (wh rect)))

(defmethod (setf xy) ((xy list) (rect rect))
  (destructuring-bind (old-x old-y) (xy rect)
    (let ((new-x (first-of xy))
          (new-y (second-of xy)))
      (with-slots (xy) rect
        (setf xy (list (if new-x new-x old-x)
                       (if new-y new-y old-y)))))))

(defmethod (setf wh) ((wh list) (rect rect))
  (destructuring-bind (old-w old-h) (wh rect)
    (let ((new-w (first-of wh))
          (new-h (second-of wh)))
      (with-slots (wh) rect
        (setf wh (list (if new-w new-w old-w)
                       (if new-h new-h old-h)))))))

(eval-when (:compile-toplevel)
  (let ((rect (make-instance 'rect :xy '(1 2))))
    (assert (equal 1 (x rect)))
    (assert (equal 2 (y rect)))
    (setf (wh rect) '(3 4))
    (assert (equal 3 (w rect)))
    (assert (equal 4 (h rect)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; surface
;;;
(defclass surface (rect)
  ((surface-objects-htable :initform (make-hash-table :test #'equalp) :accessor surface-objects-htable)))

(defmethod print-object ((object surface) (stream stream))
  (print-unreadable-object (object stream :identity nil :type t)
    (format stream "~S ~S ~S" 
            (xy object) (wh object)
            (surface-objects-htable object))))

(defun reset-surface-of-surface-objects (surface)
  (maphash (lambda (key surface-object)
             (declare (ignore key))
             (setf (slot-value surface-object 'surface)
                   surface))
           (surface-objects-htable surface)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; surface-object
;;;
(defclass surface-object (rect)
  ((surface :initarg :surface :initform nil :accessor surface)))

(defmethod print-object ((object surface-object) (stream stream))
  (print-unreadable-object (object stream :identity nil :type t)
    (format stream "~S ~S" (xy object) (surface object))))

(defmethod copy-object ((object surface-object))
  (let* ((class (class-of object))
         (slots (mapcar 'slot-definition-name (class-slots class)))
         (copy (make-instance class)))
    (dolist (slot slots copy)
      (unless (equal slot 'surface)
        (setf (slot-value copy slot)
              (copy-object (slot-value object slot)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; surface & surface-object
;;;
(defmethod surface-object (xy (surface surface))
  (gethash (->list xy) (surface-objects-htable surface)))

(defmethod (setf surface-object) ((surface-object surface-object) xy (surface surface))
  (declare (ignore xy))
  (let ((on-surface-xy (vector+ (xy surface) (xy surface-object))))
    (if (surface-object on-surface-xy surface)
        (error "Surface object ~S is already on surface ~S at location ~S." 
               surface-object surface on-surface-xy)
      (setf (slot-value surface 'wh)
            (list (max (w surface) (x surface-object))
                  (max (h surface) (y surface-object)))
            (xy surface-object) 
            on-surface-xy
            (gethash (->list on-surface-xy) (surface-objects-htable surface)) 
            surface-object))))

(defmethod initialize-instance :after ((surface-object surface-object) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (with-slots (surface xy) surface-object
    (when (and surface xy)
      (setf (surface-object t surface) surface-object))))

(eval-when (:compile-toplevel)
  (let* ((surface (make-instance 'surface :xy '(10 10)))
         (tile1 (make-instance 'surface-object :xy '(0 1)))
         (tile2 (make-instance 'surface-object :xy '(0 1) :surface surface))
         (tile3 (make-instance 'surface-object :xy '(2 2) :surface surface)))
    (assert (eq 0 (x tile1)))
    (assert (eq 1 (y tile1)))
    (assert (eq 1 (w tile1)))
    (assert (eq 1 (h tile1)))
    (assert (eq 10 (x tile2)))
    (assert (eq 11 (y tile2)))
    (assert (equalp '(2 2) (wh surface)))
    (list tile2 tile3 surface)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; act-r component device and interface
;;;
#|
The code below links a component name (symbol) to a class name.
Multiple instances of a class as act-r components can be associated to 
a device name (string). 

Actr device/component creation needs to occur before a model is defined.
|#
(defclass device-component ()
  ((device-name :initarg :device-name :initform "device-component" :accessor device-name :type string)
   (version :initarg :version :initform "1.0" :reader version :type string)
   (doc :initarg :documentation :initform "Not documented." :reader doc :type string)))

;;; component
(defmethod component-name ((device-component device-component))
  (->symbol (device-name device-component)))

(defun device-component (name)
  (get-component-fct (->symbol name)))

(defun component-p (str-or-sym)
  (let ((mp (current-mp)))
    (assoc (->symbol str-or-sym)
           (bt:with-lock-held ((meta-p-component-lock mp))
             (meta-p-component-list mp)))))

(defun def-component (component-name instance)
  (when (component-p component-name)
    (undefine-component-fct component-name))
  (define-component-fct 
   component-name 
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

;;; device-component 
;; (setf (device-component "devname") |class-name or instance| )
(defun device-p (name)
  (let ((devname (->string name)))
    (when (member devname (defined-devices) :test #'equalp) ;(current-model)
      t)))

(defun def-device (device-name)
  (when (device-p device-name)
    (undefine-device device-name))
  (define-device device-name nil nil "device-notification")
  device-name)

(defmethod (setf device-component) ((instance device-component) (device-name string))
  (setf (device-name instance) device-name)
  (def-component (->symbol device-name) instance)
  (def-device device-name)
  instance)

(defmethod (setf device-component) ((class-name symbol) (device-name string))
  (setf (device-component device-name) (make-instance class-name)))

(defmethod (setf component-instance) (instance (device-name string))
  (let ((mp (current-mp)))
    (bt:with-lock-held ((meta-p-component-lock mp))
      (let ((actr-component-assoc (assoc (->symbol device-name) (meta-p-component-list mp))))
        (when actr-component-assoc 
          (setf (act-r-component-instance (cdr actr-component-assoc))
                instance))))))

;;; interface
;; (install '("motor" "devname"))

(defun devlist-interface (devlist) (first devlist))
(defun devlist-device (devlist) (second devlist))

(defun device-interfaces (devname)
  (when (current-model)
    (mapcan (lambda (dev)
              (when (member devname dev :test #'equalp) (list (first dev))))
            (when (current-device-interface)
              (device (current-device-interface))))))

(defun device-interface-p (devname interface)
  (member interface (device-interfaces devname) :test #'equalp))

(defmethod print-object ((object device-component) (stream stream))
  (print-unreadable-object (object stream :identity t :type t)
    (format stream ":name ~S :device ~S :interfaces ~S" 
            (device-name object) 
            (device-p (device-name object))
            (device-interfaces (device-name object)))))

(defun clear-device-component (devname)
  (clear-all)
  (undefine-component-fct (->symbol devname))
  (undefine-device devname))

(defmethod device-notification ((devlist list) (features list))
  (if (equalp (devlist-interface devlist) "motor")
      (let ((device-component (device-component (devlist-device devlist))))
        (if device-component
            (let ((function (car (key->value 'style features))))
              (if (and function (fboundp function))
                  (apply function (list device-component :features features))
                (error "No style associated with feature ~S, or ~S is not a function." 
                       features function)))
          (error "There is no object component for device ~S." (devlist-device devlist))))
    (error "Thee device list ~S is not for motor." devlist)))

(add-act-r-command "device-notification" 'device-notification 
                   "Method to notify features to a device component function. Not called directly.")

(eval-when (:compile-toplevel)
  (let ((instance (make-instance 'device-component)))
    (print instance)
    (clear-all)
    (assert (null (device-p (device-name instance))))
    (setf (device-component "test") instance)
    (print instance)
    (assert (device-p (device-name instance)))
    (define-model test)
    (assert (null (device-interfaces (device-name instance))))
    (install-device '("motor" "test"))
    (print instance)
    (assert (device-interfaces (device-name instance)))
    (assert (device-interface-p "test" "motor"))
    (assert (null (device-interface-p "test" "vision")))
    (clear-device-component "test")))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; act-r motor modules and hands
;;;
(defun motor-module ()
  (get-module :motor))

(defun hand-tracker ()
  (extension (motor-module)))

(defmethod print-object ((object hand) (stream stream))
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S" (name object) (hand-xy (loc object)))))

(defun the-hand (hand-name)
  (ecase hand-name
    (left (left-hand (motor-module)))
    (right (right-hand (motor-module)))))

(defun select-hand-position (the-hand current)
  (if (or current (null (next-loc the-hand)))
      (loc the-hand) 
    (next-loc the-hand)))

;;; hand device
(defmethod hand-device ((hand-name symbol) &key (current t))
  (device-for-hand hand-name :current current))

(defmethod (setf hand-device) ((devname string) (the-hand hand) &key (current t))
  (declare (ignore current))
  (setf (device the-hand) (list "motor" devname)))

(defmethod (setf hand-device) ((devname string) (hand-name symbol) &key (current t))
  (declare (ignore current))
  (setf (hand-device (the-hand hand-name)) devname))

(defmethod hands-device (&key (current t))
  (let (hands-device)
    (dolist (hand *hand-symbols* hands-device)
      (push (list hand (hand-device hand :current current)) hands-device))))

(defmethod (setf hands-device) (device &key (current t))
  (declare (ignore current))
  (dolist (the-hand (mapcar #'the-hand *hand-symbols*) (hands-device))
    (setf (hand-device the-hand) device)))

;;; hand-device-component
(defmethod hand-device-component ((the-hand hand))
  (let ((devlist (device the-hand)))
    (when devlist (device-component (devlist-device devlist)))))

(defmethod hand-device-component ((hand-name symbol))
  (hand-device-component (the-hand hand-name)))

(eval-when (:compile-toplevel)
  (clear-all)
  (setf (device-component "abcd") 'device-component)
  (define-model abcd)
  (install-device '("motor" "abcd"))
  (assert (null (equalp '("motor" "abcd") (hand-device 'right))))
  (assert (null (equalp '("motor" "abcd") (hand-device 'left))))
  (setf (hands-device) "abcd")
  (assert (equalp "abcd" (device-name (hand-device-component 'right))))
  (assert (equalp "abcd" (device-name (hand-device-component 'left))))
  (assert (equalp '("motor" "abcd") (hand-device 'right)))
  (assert (equalp '("motor" "abcd") (hand-device 'left)))
  (clear-device-component "abcd"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; audio-object
;;;
(defclass audible-object ()
  ((sound-id :type number-or-nil :reader sound-id)  
   (frequency :type number :initform 400 :initarg :frequency :reader frequency)))

(defun audio-module ()
  (get-module :audio))

(defun start-audio-tone (object)
  (let ((audio-module (audio-module)))
    (if audio-module
        (with-slots (sound-id frequency) object
          (setf sound-id
                (new-ongoing-sound 
                 frequency 
                 (tone-detect-delay audio-module)
                 (tone-recode-delay audio-module)
                 (if (ms-time-units audio-module)
                     (mp-time-ms) (mp-time))
                 'external 'tone 
                 (if (ms-time-units audio-module)t nil))))
      (error "Audio module not found."))))

(defun end-audio-tone (object)
  (with-slots (sound-id) object
    (when sound-id
      (end-ongoing-sound sound-id)
      (setf sound-id nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; visicon-object
;;;
(defparameter *visual-location-features*
  '(screen-x screen-y distance kind size))

(defparameter *shared-features*
  '(value color height width))

(defparameter *visual-object-features*
  '(screen-pos status))

(defparameter *actr-core-features*
  (append *visual-location-features*
          *shared-features*
          *visual-object-features*))

(defparameter *symbol-for-nil-value* 'empty)

(defclass visicon-object (surface-object)
  ((distance :type number-or-nil :initform nil :initarg :distance :reader distance)
   (color :type symbol :initform nil :initarg :color :reader color)
   (value :type atom :initform nil :initarg :value :reader value)
   (feature-id :type symbol :initform nil :reader feature-id)
   (visual-location :type symbol :initform nil :reader visual-location)
   (visual-features :type symbols-set :initform nil :initarg :visual-features)))

(defmethod screen-x ((object visicon-object)) (x object))
(defmethod screen-y ((object visicon-object)) (y object))
(defmethod width ((object visicon-object)) (w object))
(defmethod height ((object visicon-object)) (h object))
(defmethod kind ((object visicon-object)) "Set using actr computed value." nil)
(defmethod size ((object visicon-object)) "Set using actr computed value." nil)
(defmethod status ((object visicon-object)) "Set using actr computed value." nil)
(defmethod screen-pos ((object visicon-object)) "Set using actr computed value." nil)

(defun key->value (key alist)
  (cdr (assoc key alist)))

(defun remove-pairs (alist &optional keys)
  (let ((new-alist alist))
    (dolist (key keys new-alist)
      (setf new-alist (remove key new-alist :key #'car)))))

(defun slot->feature (object slot-name)
  (if (fboundp slot-name)
      (let ((slot-value (apply slot-name (list object))))
        (cond (slot-value
               (list slot-name slot-value))
              ((not (member slot-name *actr-core-features*))
               (list slot-name *symbol-for-nil-value*))))   
    (error "There is no reader method ~S for object ~S." slot-name object)))

(defun object->features (object slot-names)
  (let (features-set)
    (dolist (slot-name slot-names features-set)
      (setf features-set (append features-set 
                                 (slot->feature object slot-name))))))

(defun visloc-type (object)
  (intern (format nil "~S-FEATURE" (type-of object))))

(defun visobj-type (object)
  (type-of object))

(defun chunk-types-p (object)
  (and (chunk-type-p-fct (visloc-type object))
       (chunk-type-p-fct (visobj-type object))))

(defun def-chunk-types (object)
  (unless (chunk-types-p object)
    (let (chunks-defs)
      (dolist (chunk-type-spec `(((,(visloc-type object) (:include visual-location)))
                                 ((,(visobj-type object) (:include visual-object)))) 
                               chunks-defs)
        (let ((features (copy-list (slot-value object 'visual-features))))
          (push (chunk-type-fct (append chunk-type-spec features)) chunks-defs))))))

(eval-when (:compile-toplevel)
  (let ((instance (make-instance 'visicon-object)))
    (clear-all)
    (define-model test)
    (def-chunk-types instance)))

(defun visual-object-features (object &key isa)
  (with-slots (visual-features) object
    (append (when isa
              (list 'isa (list (visloc-type object) (visobj-type object))))
            (object->features object *visual-location-features*)
            (object->features object *shared-features*)
            (object->features object *visual-object-features*)
            (object->features object visual-features))))

(eval-when (:compile-toplevel)
  (defclass ext-vo (visicon-object)
    ((ext-slot :initform 'white :accessor ext-slot))
    (:default-initargs
     :visual-features '(ext-slot)))
  (let ((instance (make-instance 'ext-vo)))
    (clear-all)
    (define-model test)
    (assert (equalp (visual-object-features instance :isa t)
                    '(ISA (EXT-VO-FEATURE EXT-VO) SCREEN-X 0 SCREEN-Y 0 HEIGHT 1 WIDTH 1 EXT-SLOT WHITE)))
    (assert (equalp (visual-object-features instance)
                    '(SCREEN-X 0 SCREEN-Y 0 HEIGHT 1 WIDTH 1 EXT-SLOT WHITE)))
    (setf (ext-slot instance) nil)
    (assert (equalp (visual-object-features instance)
                    '(SCREEN-X 0 SCREEN-Y 0 HEIGHT 1 WIDTH 1 EXT-SLOT EMPTY))))
  (unintern 'ext-vo)
  )

(defun set-visicon-ids (object feat-id)
  (with-slots (feature-id visual-location) object
    (setf feature-id feat-id
          visual-location (chunk-visual-loc feat-id))))

; to do: parse fedatures and add chunks for slot values to avoid message:
; Warning: Creating chunk ABC with no slotsé
(defmethod add-to-visicon ((object visicon-object))
  (def-chunk-types object)
  (unless (chunk-p-fct (visobj-type object))
    (define-chunks-fct (list (visobj-type object))))
  (values 
   (set-visicon-ids object (car (add-visicon-features (visual-object-features object :isa t))))
   object))

(defmethod delete-from-visicon ((object visicon-object))
  (with-slots (feature-id visual-location) object 
    (when (member feature-id (visicon (get-module :vision)))
      (delete-visicon-features feature-id))
    (setf feature-id nil
          visual-location nil)))

(defmethod modify-visicon ((object visicon-object))
  (let ((features (visual-object-features object))
        (feature-id (slot-value object 'feature-id)))
    (when feature-id
      (modify-visicon-features 
       (append (list feature-id) features)))))

(eval-when (:compile-toplevel)
  (defclass ext-vo (visicon-object)
    ((ext-slot :initform 'white :accessor ext-slot))
    (:default-initargs
     :visual-features '(ext-slot)))
  (let ((instance (make-instance 'ext-vo)))
    (clear-all)
    (define-model test)
    (assert (and (null (feature-id instance)) (null (visual-location instance))))
    (add-to-visicon instance)
    (run-n-events 5)
    (print-visicon)
    (assert (and (feature-id instance) (visual-location instance)))
    (setf (xy instance) '(nil 10))
    (modify-visicon instance)
    (run-n-events 5)
    (print-visicon)
    (delete-from-visicon instance)
    (run-n-events 5)
    (print-visicon)
    (assert (and (null (feature-id instance)) (null (visual-location instance))))
    )
  (unintern 'ext-vo)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; visible-surface
;;;
(defclass visible-surface (device-component surface)
  ())

(defmethod print-object ((object visible-surface) (stream stream))
  (print-unreadable-object (object stream :identity nil :type t)
    (format stream "~S ~S ~S ~S" 
            (device-name object) (device-interfaces object)
            (xy object) (wh object))))

(defmethod add-to-visicon ((object visible-surface))
  (if (device-interface-p (device-name object) "vision")
      (maphash (lambda (key visicon-object)
                 (declare (ignore key))
                 (add-to-visicon visicon-object))
               (surface-objects-htable object))
    (error "Object ~S does not interface with vision." object)))
  
(defmethod delete-from-visicon ((object visible-surface))
  (maphash (lambda (key visicon-object)
             (declare (ignore key))
             (delete-from-visicon visicon-object))
           (surface-objects-htable object))
  object)

(defmethod modify-visicon ((object visible-surface))
  (maphash (lambda (key visicon-object)
             (declare (ignore key))
             (modify-visicon visicon-object))
           (surface-objects-htable object))
  object)


(eval-when (:compile-toplevel)
  (clear-all)
  (setf (device-component "test") 'visible-surface)
  (let* ((surface (device-component "test"))
         (tile1 (make-instance 'visicon-object :xy '(5 5) :surface surface))
         (tile2 (make-instance 'visicon-object :xy '(1 1) :surface surface)))
    (declare (ignore tile1 tile2))
    (define-model test)
    (install-device '("vision" "test")) 
    (add-to-visicon (device-component "test"))
    (run-n-events 5)
    (print-visicon)
    (print (device-component "test"))
    (clear-device-component "test")))

(eval-when (:compile-toplevel)
  (defclass ext-vo (visicon-object)
    ((ext-slot :initform 'white :accessor ext-slot))
    (:default-initargs
     :visual-features '(ext-slot)))
  (clear-all)
  (setf (device-component "test") 'visible-surface)
  (let* ((surface (device-component "test"))
         (ext-slot1 (make-instance 'ext-vo :surface surface)))
    (declare (ignore ext-slot1)))
  (define-model test)
  (install-device '("vision" "test")) 
  (add-to-visicon (device-component "test"))
  (run-n-events 5)
  (print-visicon)
  (clear-device-component "test")
  (unintern 'ext-vo)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; tangigle-object
;;;
(defclass tangible-object (visicon-object)
  ((hand :initform nil :accessor hand)
   (finger :initform nil :accessor finger)
   (state :initform nil :accessor state))
  (:default-initargs
   :visual-features '(hand finger state)))

(defmethod hand-finger ((tangible-object tangible-object))
  (with-slots (hand finger) tangible-object
    (when (and hand finger)
      (list hand finger))))

(defmethod (setf hand-finger) ((hand-finger list) (tangible-object tangible-object))
  (with-slots (hand finger surface) tangible-object
    (when (or (null hand-finger) 
              (validate-hand-finger hand-finger))
      (setf hand (first hand-finger)
            finger (second hand-finger))
      (when surface
        (setf (get-hand-finger-object t surface) tangible-object)))))

(defmethod remove-finger ((tangible-object tangible-object))
  (setf (hand-finger tangible-object) nil
        (state tangible-object) nil))

(eval-when (:compile-toplevel)
  (let ((instance (make-instance 'tangible-object)))
    (setf (hand-finger instance) '(right index))
    (assert (equalp (hand-finger instance) '(right index)))
    (assert (null (state instance)))
    (remove-finger instance)
    (assert (null (hand-finger instance)))
    (assert (null (state instance)))))

(defmethod tangible-object-over ((tangible-object tangible-object) &key features)
  (setf (state tangible-object) nil
        (hand-finger tangible-object)
        (list (car (key->value 'hand features))
              (car (key->value 'finger features))))
  (when (current-model) (modify-visicon tangible-object))
  (hand-finger tangible-object))

(defmethod tangible-object-away ((tangible-object tangible-object) &key features)
  (declare (ignore features))
  (remove-finger tangible-object)
  (when (current-model) (modify-visicon tangible-object))
  (hand-finger tangible-object))

(defmethod tangible-object-press ((tangible-object tangible-object) &key features)
  (declare (ignore features))
  (when (hand-finger tangible-object)
    (setf (state tangible-object) 'pressed))
  (when (current-model) (modify-visicon tangible-object))
  (state tangible-object))

(defmethod tangible-object-release ((tangible-object tangible-object) &key features)
  (declare (ignore features))
  (when (hand-finger tangible-object)
    (setf (state tangible-object) nil))
  (when (current-model) (modify-visicon tangible-object))
  (state tangible-object))

(eval-when (:compile-toplevel)
  (let ((instance (make-instance 'tangible-object)))
    (tangible-object-over instance :features '((hand right) (finger index)))
    (assert (equalp (hand-finger instance) '(right index)))
    (tangible-object-press instance)
    (assert (equal (state instance) 'pressed))
    (tangible-object-release instance)
    (assert (null (state instance)))
    (tangible-object-away instance)
    (assert (null (hand-finger instance)))
    (assert (null (state instance)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; visible-hand
;;;
(defclass visible-hand (visicon-object)
  ((hand :initarg :hand :reader hand)
   (thumb :initarg :thumb :initform nil :accessor thumb)
   (index :initarg :index :initform nil :accessor index)
   (middle :initarg :middle :initform nil :accessor middle)
   (ring :initarg :ring :initform nil :accessor ring)
   (pinkie :initarg :pinkie :initform nil :accessor pinkie))
  (:default-initargs
  :visual-features '(pinkie ring middle index thumb hand)))

(defparameter *left-hand* (make-instance 'visible-hand :hand 'left))
(defparameter *right-hand* (make-instance 'visible-hand :hand 'right))

(defmethod tangible-object-over ((visible-hand visible-hand) &key features)
  (let ((surface (car (key->value 'surface features)))
        (xy (car (key->value 'xy features))))
    (setf (slot-value visible-hand 'surface)
          surface
          (slot-value visible-hand 'xy) 
          xy
          (gethash (hand visible-hand) (surface-objects-htable surface))
          visible-hand)
    (if (feature-id visible-hand)
        (modify-visicon visible-hand)
      (add-to-visicon visible-hand))))

(defmethod hand-away ((visible-hand visible-hand))
  (setf (slot-value visible-hand 'xy) '(0 0))
  (dolist (finger *finger-symbols* visible-hand)
    (setf (slot-value visible-hand finger) nil)))

(defmethod tangible-surface-hand-away ((visible-hand visible-hand) &key features)
  (declare (ignore features))
  ;(let ((surface (car (key->value 'surface features)))) 
    (hand-away visible-hand)
    ;(remhash hand (hand-finger-objects-htable tangible-surface))
    (delete-from-visicon visible-hand)
    ) ;)
         

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; tangible-surface
;;;
(defclass tangible-surface (visible-surface)
  ((hand-finger-objects-htable :initform (make-hash-table :test #'equalp) :reader hand-finger-objects-htable)))

(defmethod get-hand-finger-object ((hand-finger list) (tangible-surface tangible-surface))
  (when (validate-hand-finger hand-finger) 
    (gethash hand-finger (hand-finger-objects-htable tangible-surface))))

(defmethod (setf get-hand-finger-object) ((tangible-object tangible-object) hand-finger (tangible-surface tangible-surface))
  (declare (ignore hand-finger))
  (when (hand-finger tangible-object)
    (setf (gethash (hand-finger tangible-object) (hand-finger-objects-htable tangible-surface))
          tangible-object)))

(defmethod tangible-surface-hand-away ((tangible-surface tangible-surface) &key features)
  (let ((hand (car (key->value 'hand features))))
    (when hand
      (ecase hand 
        (left (tangible-surface-hand-away *left-hand* :features `((surface ,tangible-surface))))
        (right (tangible-surface-hand-away *right-hand* :features `((surface ,tangible-surface)))))
      (maphash (lambda (key tangible-object)
                 (when (equal hand (hand tangible-object))
                   (tangible-object-away tangible-object)
                   (remhash key (hand-finger-objects-htable tangible-surface))))
               (hand-finger-objects-htable tangible-surface)))))

(defmethod tangible-surface-hands-away ((tangible-surface tangible-surface) &key features)
  (declare (ignore features))
  (dolist (hand '(left right) t)
    (tangible-surface-hand-away tangible-surface :features `((hand ,hand)))))

(defun validate-finger-position (finger xy tangible-surface)
  (let ((surface-object (surface-object xy tangible-surface)))
    (if surface-object surface-object
      (error "No object at location ~S for finger ~S on surface ~S." xy finger tangible-surface))))

(defmethod validate-finger-positions ((hand-pos hand-pos) (tangible-surface tangible-surface))
  (unless (hand-position-0-p hand-pos)
    (dolist (finger *finger-symbols* t)
      (validate-finger-position finger (finger-xy hand-pos finger) tangible-surface))))

(defmethod tangible-surface-hand-over :before ((tangible-surface tangible-surface) &key features)
  (let ((hand-pos (car (key->value 'hand-pos features))))
    (validate-finger-positions hand-pos tangible-surface)))
  
(defmethod tangible-surface-hand-over ((tangible-surface tangible-surface) &key features)
  (let ((hand (car (key->value 'hand features)))
        (hand-pos (car (key->value 'hand-pos features))))
    (dolist (finger *finger-symbols*)
      (let* ((xy (finger-xy hand-pos finger))
             (tangible-object (surface-object xy tangible-surface)))
        (tangible-object-over tangible-object :features `((hand ,hand) (finger ,finger)))
        (case hand
          (left (setf (slot-value *left-hand* finger) xy))
          (right (setf (slot-value *right-hand* finger) xy)))
        (setf (get-hand-finger-object (list hand finger) tangible-surface)
              tangible-object)))
    (ecase hand
      (left (tangible-object-over *left-hand* :features `((surface ,tangible-surface) 
                                                          (xy ,(->list (hand-xy hand-pos))))))
      (right (tangible-object-over *right-hand* :features `((surface ,tangible-surface) 
                                                            (xy ,(->list (hand-xy hand-pos)))))))))


(defmethod tangible-object-press ((tangible-surface tangible-surface) &key features)
  (let ((hand (car (key->value 'hand features)))
        (finger (car (key->value 'finger features))))
    (when (and hand finger)
      (tangible-object-press 
       (get-hand-finger-object (list hand finger) tangible-surface)))))

(defmethod tangible-object-release ((tangible-surface tangible-surface) &key features)
  (let ((hand (car (key->value 'hand features)))
        (finger (car (key->value 'finger features))))
    (when (and hand finger)
      (tangible-object-release 
       (get-hand-finger-object (list hand finger) tangible-surface)))))

(eval-when (:compile-toplevel :execute)
  (let* ((surface (make-instance 'tangible-surface))
         (a (make-instance 'tangible-object :xy '(1 1) :surface surface))
         (b (make-instance 'tangible-object :xy '(2 1) :surface surface))
         (c (make-instance 'tangible-object :xy '(3 1) :surface surface))
         (d (make-instance 'tangible-object :xy '(4 1) :surface surface))
         (e (make-instance 'tangible-object :xy '(5 1) :surface surface)))
    (declare (ignore a b c d e))
    (assert (validate-finger-positions (make-hand-position 'right 'thumb '(1 1)) surface))
    (clear-all)
    (setf (device-component "abcd") surface)
    (define-model test
      (define-chunks pressed))
    (install-device '("vision" "abcd"))
    (install-device '("motor" "abcd"))
    (add-to-visicon surface)
    (run-n-events 5)
    (print-visicon)
    (tangible-surface-hand-over surface :features `((hand right) (finger thumb)
                                                    (hand-pos ,(make-hand-position 'right 'thumb '(1 1)))))
    (run-n-events 5)
    (print-visicon)
    (tangible-object-press surface :features `((hand right) (finger thumb)
                                               (hand-pos ,(make-hand-position 'right 'thumb '(1 1)))))

    (run-n-events 5)
    (print-visicon)
    (tangible-object-press surface :features `((hand right) (finger middle)
                                               (hand-pos ,(make-hand-position 'right 'thumb '(1 1)))))
    (run-n-events 5)
    (print-visicon)
    (tangible-object-release surface :features `((hand right) (finger middle)
                                               (hand-pos ,(make-hand-position 'right 'thumb '(1 1)))))
    (run-n-events 5)
    (print-visicon)
    (tangible-surface-hand-away surface :features `((hand right) 
                                               (hand-pos ,(make-hand-position 'right 'thumb '(1 1)))))
    (run-n-events 5)
    (print-visicon)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; styles
;;;

(defun new-extended-manual-request (chunk-type)
  (let ((module (or (motor-module) (make-instance 'motor-module))))
    (bt:with-lock-held ((requests-table-lock module))
      (when (gethash (car chunk-type) (new-requests-table module))
        (remhash (car chunk-type) (new-requests-table module)))))
  (extend-manual-requests-fct chunk-type 'handle-style-request))

(defclass hand-action (hf-movement)
  ((action :initarg :action :initform 'empty-action :accessor action)
   (vis-loc :initarg :vis-loc :initform nil :accessor vis-loc)   
   (thumb :initarg :thumb :initform nil :accessor thumb)
   (index :initarg :index :initform nil :accessor index)
   (middle :initarg :middle :initform nil :accessor middle)
   (ring :initarg :ring :initform nil :accessor ring)
   (pinkie :initarg :pinkie :initform nil :accessor pinkie)
   (hand-pos :initarg :hand-pos :initform nil :accessor hand-pos))
  (:default-initargs
   :feature-slots '(action hand finger vis-loc thumb index middle ring pinkie)
   :style-name 'apply-hand-action
   :two-exec-p nil
   :finger-based-style t
   :release-if-down nil))

(defun make-hand-action (style-class action hand request-spec &optional finger)
  (make-instance style-class :action action :hand hand :request-spec request-spec
                 :finger (if finger finger 'thumb)))

(defmethod hand-action ((mtr-mod dual-execution-motor-module) &key (style-class 'hand-action) action hand finger 
                        vis-loc thumb index middle ring pinkie request-spec)
  (unless (or (check-jam mtr-mod) (check-specs action hand))
    (let ((instance (make-hand-action style-class action hand request-spec finger)))
      (when vis-loc (setf (vis-loc instance) vis-loc))
      (when thumb (setf (thumb instance) thumb))
      (when index (setf (index instance) index))
      (when middle (setf (middle instance) middle))
      (when ring (setf (ring instance) ring))
      (when pinkie (setf (pinkie instance) pinkie))
      (prepare-movement mtr-mod instance))))

(defmethod hand-position ((hand-action hand-action) &key (current t))
  (declare (ignore current))
  (with-slots (hand finger vis-loc thumb index middle ring pinkie) hand-action
    (make-hand-position 
     hand finger (if vis-loc vis-loc (hand-xy (loc (the-hand hand))))
     :thumb thumb :index index :middle middle :ring ring :pinkie pinkie)))
   
(defmethod prepare-features ((mtr-mod dual-execution-motor-module) (hand-action hand-action))
  (setf (updated-pos hand-action) 
        (if (equal (action hand-action) 'tangible-surface-hand-away)
            (make-hand-position-0)
          (hand-position hand-action))))
       
(defmethod feat-differences ((current hand-action) (previous hand-action))
  (let ((count 0))
    (if (equal (action current) 'tangible-surface-hand-away)
        count
      (dolist (feature-slot (feature-slots current) count)
        (unless (equalp (apply feature-slot (list current))
                        (apply feature-slot (list previous)))
          (incf count))))))

(defmethod compute-exec-time ((mtr-mod dual-execution-motor-module) (hand-action hand-action))
  .2)

(defmethod compute-finish-time ((mtr-mod dual-execution-motor-module) (hand-action hand-action))
  (+ .1 (exec-time hand-action)))

(defun make-finger-down (module hand finger)
  (let ((extension (extension module)))
    (bt:with-recursive-lock-held ((hand-tracker-lock extension))
      (unless (gethash (list hand finger) (hand-tracker-finger-down extension))
        (setf (gethash (list hand finger) (hand-tracker-finger-down extension)) t)))))

(defun make-finger-up (module hand finger)
  (let ((extension (extension module)))
    (bt:with-recursive-lock-held ((hand-tracker-lock extension))
      (when (gethash (list hand finger) (hand-tracker-finger-down extension))
        (remhash (list hand finger) (hand-tracker-finger-down extension))))))

(defmethod general-queue-output-events ((mtr-mod dual-execution-motor-module) (hand-action hand-action))
  (ecase (action hand-action)
    (tangible-surface-hand-over
     (schedule-event-relative (seconds->ms (exec-time hand-action)) 'set-hand-position 
                              :time-in-ms t :module :motor :output nil
                              :destination :motor :params (list (hand hand-action) (updated-pos hand-action))))
    (tangible-surface-hand-away
     (schedule-event-relative (seconds->ms (exec-time hand-action)) 'set-hand-position 
                              :time-in-ms t :module :motor :output nil
                              :destination :motor :params (list (hand hand-action) (updated-pos hand-action)))
     (dolist (finger *finger-symbols*)
       (schedule-event-relative (seconds->ms (exec-time hand-action)) 'make-finger-up 
                              :time-in-ms t :destination :motor :module :motor :output nil
                              :params (list (hand hand-action) finger))))
    (tangible-object-press
     (schedule-event-relative (seconds->ms (exec-time hand-action)) 'make-finger-down 
                              :time-in-ms t :destination :motor :module :motor :output nil
                              :params (list (hand hand-action) (finger hand-action))))
    (tangible-object-release
     (schedule-event-relative (seconds->ms (exec-time hand-action)) 'make-finger-up 
                              :time-in-ms t :destination :motor :module :motor :output nil
                              :params (list (hand hand-action) (finger hand-action)))))
  (schedule-event-relative (exec-time hand-action) 
                           (action hand-action)
                           :params (list (hand-device-component (hand hand-action)) 
                                         :features `((hand ,(hand hand-action)) 
                                                     (finger ,(finger hand-action))
                                                     (hand-pos ,(updated-pos hand-action))))))
        
(defmethod queue-output-events ((mtr-mod dual-execution-motor-module) (hand-action hand-action))
  (general-queue-output-events mtr-mod hand-action))
    
(new-extended-manual-request '(hand-action action hand finger vis-loc thumb index middle ring pinkie))      
  

(eval-when (:compile-toplevel :execute)
  (let* ((*break-on-signals* t)
         (surface (make-instance 'tangible-surface))
         (a (make-instance 'tangible-object :xy '(1 1) :surface surface))
         (b (make-instance 'tangible-object :xy '(2 1) :surface surface))
         (c (make-instance 'tangible-object :xy '(3 1) :surface surface))
         (d (make-instance 'tangible-object :xy '(4 1) :surface surface))
         (e (make-instance 'tangible-object :xy '(5 1) :surface surface)))
    (declare (ignore a b c d e))
    (clear-all)
    (setf (device-component "abcd") surface)
    (define-model test
      (define-chunks pressed))
    (install-device '("vision" "abcd"))
    (install-device '("motor" "abcd"))
    (setf (hands-device) "abcd")
    (add-to-visicon surface)
    (run-n-events (mp-queue-count))
    (print-visicon)
    (format t "~% Place hand ~%~%")
    (hand-action (motor-module)
                 :action 'tangible-surface-hand-over
                 :hand 'right :finger 'thumb :vis-loc '(1 1))
    (run 1)
    (print-visicon)
    (describe *right-hand*)
    (format t "~% Press thumb ~%~%")
    (hand-action (motor-module)
                 :action 'tangible-object-press
                 :hand 'right :finger 'thumb :vis-loc '(1 1))
    (run 1)
    (print-visicon)
    (format t "~% Press middle ~%~%")
    (hand-action (motor-module)
                 :action 'tangible-object-press
                 :hand 'right :finger 'middle :vis-loc '(1 1))
    (run 1)
    (print-visicon)
    (format t "~% Release middle ~%~%")
    (hand-action (motor-module)
                 :action 'tangible-object-release
                 :hand 'right :finger 'middle :vis-loc '(1 1))
    (run 1)
    (print-visicon)
    (format t "~% Remove hand ~%~%")
    (hand-action (motor-module)
                 :action 'tangible-surface-hand-away
                 :hand 'right)
    (run 1)
    (print-visicon)
    ))

:eof