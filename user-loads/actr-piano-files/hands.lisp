;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; hands.lisp
;;;
;;; 2025-10-20
;;;

;;;
;;; parameters
;;;
(defconstant +hand-names+ '(left right))
(defconstant +finger-names+ '(thumb index middle ring pinkie))

;;; motor module
(defun motor-module ()
  (get-module :motor))

(defun hand-tracker ()
  (extension (motor-module)))

(defun the-hand (hand-name)
  "Returns the motor module hand instance."
  (ecase hand-name
    (left (left-hand (motor-module)))
    (right (right-hand (motor-module)))))

;; hand-device
(defun hand-device (hand-name)
  "Returns the device associated to a hand."
  (if (typep hand-name `(member-of ,+hand-names+))
      (notify-interface "motor" `(get-hand-device ,hand-name))
    (print-warning "~S is not a hand name ~S." hand-name +hand-names+)))

(defmethod (setf hand-device) ((dev-list list) (hand-name symbol))
  "Sets the device associated to a hand, but (install-device device-list) does it for each hand."
  (if (and (typep dev-list 'device-list)
           (typep hand-name `(member-of ,+hand-names+)))
      (notify-interface "motor" `(set-hand-device ,hand-name ,dev-list))
    (print-warning "Either ~S is not a hand name ~S, or ~S is not a device list."
                   hand-name +hand-names+ dev-list)))
  
;; hand-xy
(defun hand-xy (hand-name)
  "Returns the xy coordinate of a hand."
  (if (typep hand-name `(member-of ,+hand-names+))
      (notify-interface "motor" `(get-hand-position ,hand-name))
    (print-warning "~S is not a hand name ~S." hand-name +hand-names+)))

(defmethod (setf hand-xy) ((xy list) (hand-name symbol))
  "Sets the xy coordinate of a hand."
  (if (and (typep xy 'xy-coordinate)
           (typep hand-name `(member-of ,+hand-names+)))
      (notify-interface "motor" `(set-hand-position ,hand-name ,(first xy) ,(second xy)))
    (print-warning "Either ~S is not a hand name ~S, or ~S is not a xy-coordinate."
                   hand-name +hand-names+ xy)))

(defmethod (setf hand-xy) ((xy vector) (hand-name symbol))
  (setf (hand-xy hand-name) (->list xy)))

(add-act-r-command "set-hand-xy" (lambda (hand xy) (setf (hand-xy hand) xy)))

;; finger-offset
(defmethod (setf finger-offsets) ((finger-xys list) (hand-name symbol))
  "Sets the finger offsets for a hand. Ea"
  (if (typep finger-xys '(set-of finger-offset))
      (notify-interface "motor" `(set-finger-offset ,hand-name ,@finger-xys))
    (print-warning "~S is not a set of finger offset (finger x y)." finger-xys)))

(defparameter *default-rh-finger-offsets*
  '((thumb 0 0) (index 1 1) (middle 2 0) (ring 3 0) (pinkie 4 0)))
(defparameter *default-lh-finger-offsets*
  '((thumb 0 0) (index -1 0) (middle -2 0) (ring -3 0) (pinkie -4 0)))

(defun set-default-finger-offsets (thumbs-xy)
  (setf (hand-xy 'right) thumbs-xy
        (hand-xy 'left) thumbs-xy
        (finger-offsets 'right) *default-rh-finger-offsets*
        (finger-offsets 'left) *default-lh-finger-offsets*)
  (values (hand-position 'right) 
          (hand-position 'left)))

;; finger-xy
(defun finger-xy (hand-name finger)
  (if (and (typep finger `(member-of ,+finger-names+))
           (typep hand-name `(member-of ,+hand-names+)))
      (notify-interface "motor" `(get-finger-position ,hand-name ,finger))
    (print-warning "Either ~S is not a hand name ~S, or ~S is not a finger name ~S."
                   hand-name +hand-names+ finger +finger-names+)))

(defmethod (setf finger-xy) ((xy list) hand-name finger)
  (let ((hand-xy (hand-xy hand-name)))
    (setf (finger-offsets hand-name)
          (list (list finger 
                      (- (first xy) (first hand-xy))
                      (- (second xy) (second hand-xy)))))
    (finger-xy hand-name finger)))

(add-act-r-command "set-finger-xy" (lambda (hand finger xy) (setf (finger-xy hand finger) xy)))

;;;;; piano-hand-movement
(defclass piano-hand-movement-style (dual-hand-movement-style)
  ())

;;;;; move-piano-finger
(defstyle move-piano-finger piano-hand-movement-style hand finger to-xy)

(defmethod prepare-features ((m motor-module) (action move-piano-finger))
  (multiple-value-bind (r theta)
      (r-theta (finger-xy (hand action) (finger action))
               (to-xy action))
    (setf (updated-pos action)
          (move-finger (the-hand (hand action)) (finger action) r theta :current nil))))

(defmethod feat-differences ((action1 move-piano-finger) (action2 move-piano-finger))
  (let ((count 0))
    (unless (equalp (to-xy action1) (to-xy action2))
      (incf count))
    count))

(defmethod compute-exec-time ((m motor-module) (action move-piano-finger))
  .2)

(defmethod compute-finish-time ((m motor-module) (action move-piano-finger))
  (+ .1 (exec-time action)))

(defmethod queue-output-events ((m motor-module) (action move-piano-finger))
  (schedule-event-relative 
   (exec-time action) "set-finger-xy"
   :params (list (hand action) 
                 (finger action)
                 (to-xy action))))

(extend-manual-requests (move-piano-finger hand to-xy) handle-style-request)



;;;;; move-piano-hand
(defstyle move-piano-hand piano-hand-movement-style hand to-xy thumb-xy)

(defmethod prepare-features ((m motor-module) (action move-piano-hand))
  (multiple-value-bind (r theta)
      (r-theta (hand-xy (hand action))
               (to-xy action))
    (setf (updated-pos action)
          (move-hand (the-hand (hand action)) r theta :current nil)))
  (when (thumb-xy action)
    (multiple-value-bind (r theta)
      (r-theta (finger-xy (hand action) 'thumb)
               (thumb-xy action))
      (setf (updated-pos action)
            (move-finger (the-hand (hand action)) 'thumb r theta :current nil)))))

(defmethod feat-differences ((action1 move-piano-hand) (action2 move-piano-hand))
  (let ((count 0))
    (unless (equalp (to-xy action1) (to-xy action2))
      (incf count))
    (unless (equalp (thumb-xy action1) (thumb-xy action2))
      (incf count))
    count))

(defmethod compute-exec-time ((m motor-module) (action move-piano-hand))
  .2)

(defmethod compute-finish-time ((m motor-module) (action move-piano-hand))
  (+ .1 (exec-time action)))

(defmethod queue-output-events ((m motor-module) (action move-piano-hand))
  (schedule-event-relative 
   (exec-time action) "set-hand-xy"
   :params (list (hand action) (to-xy action)))
  (when (thumb-xy action)
    (schedule-event-relative 
     (exec-time action) "set-finger-xy"
     :params (list (hand action) 'thumb (thumb-xy action)))))

(extend-manual-requests (move-piano-hand hand to-xy thumb-xy) handle-style-request)


#|




;;; hand-pos
(defmethod hand-pos ((the-hand hand) &key (current t))
  (hand-position the-hand :current current))

(defmethod hand-pos ((hand-name symbol) &key (current t))
  (hand-pos (the-hand hand-name) :current current))

(defmethod (setf hand-pos) ((hand-pos hand-pos) (the-hand hand) &key (current t))
  (set-hand-position (motor-module) the-hand hand-pos :current current))

(defmethod (setf hand-pos) ((hand-pos hand-pos) (hand-name symbol) &key (current t))
  (setf (hand-pos (the-hand hand-name) :current current) hand-pos))






;;; finger-offsets

(defmethod finger-offsets ((hand hand))
  (hand-pos-fingers (hand-position hand)))

(defmethod finger-offsets ((hand-name symbol))
  (finger-offsets (the-hand hand-name)))

(defmethod (setf finger-offsets) ((finger-offsets list) (hand hand))
  (let (new-offsets)
    (dolist (old-offset 
             (finger-offsets hand)
             (setf (hand-pos-fingers (hand-position hand)) new-offsets))
      (let ((new-offset (assoc (car old-offset) finger-offsets)))
        (setf new-offsets
              (if new-offset 
                  (append new-offsets (list new-offset))
                (append new-offsets (list old-offset))))))))

(defmethod (setf finger-offsets) ((finger-offsets list) (hand-name symbol))
  (setf (finger-offsets (the-hand hand-name)) finger-offsets))
           
(defun set-default-finger-offsets ()
  (setf (finger-offsets 'right) *default-rh-finger-offsets*
        (finger-offsets 'left) *default-lh-finger-offsets*)
  (values (hand-position 'right) (hand-position 'left)))

(defmethod move-finger-to-pos ((hand hand) finger new-pos &key (current t))
  (let ((old-pos (finger-loc hand finger)))
    (multiple-value-bind (r theta)
        (r-theta old-pos new-pos)
      (move-finger hand finger r theta :current current))))

(defmethod move-finger-to-pos ((hand-name symbol) finger new-pos &key (current t))
  (move-finger-to-pos (the-hand hand-name) finger new-pos :current current))

(defmethod move-hand-to-pos ((hand hand) new-pos &key (current t))
  (let ((old-pos (hand-loc hand)))
    (multiple-value-bind (r theta)
        (r-theta old-pos new-pos)
      (move-hand hand r theta :current current))))

;; xy-to-polar (from to)

(defmethod move-hand-to-pos ((hand-name symbol) new-pos &key (current t))
  (move-hand-to-pos (the-hand hand-name)  new-pos :current current))

|#
#|
(defmethod hand-position ((the-hand hand) &key (current t))
  (bt:with-lock-held ((hand-lock the-hand))
    (let ((location (if (or current (null (next-loc the-hand))) (loc the-hand) (next-loc the-hand))))
      location)))

(defmethod set-hand-position ((mtr-mod motor-module) hand pos &key (current t))
  (let ((h (ecase hand
             (right (right-hand mtr-mod))
             (left (left-hand mtr-mod)))))
    (bt:with-lock-held ((hand-lock h))
      (if current
          (setf (loc h) pos)
        (setf (next-loc h) pos))))
  nil)

|#
#|
(set-hand-position ; hand x y 
                   (if (and (numberp (second p)) (numberp (third p)))
                       (case (first p)
                         (left 
                          (let ((hand (left-hand motor)))
                            (bt:with-lock-held ((hand-lock hand))
                              (setf (loc hand) (make-hand-pos :loc (vector (second p) (third p))
                                                              :fingers (left-standard-offsets))))))
                         (right 
                          (let ((hand (right-hand motor)))
                            (bt:with-lock-held ((hand-lock hand))
                              (setf (loc hand) (make-hand-pos :loc (vector (second p) (third p))
                                                              :fingers (right-standard-offsets))))))
                         (t
                          (print-warning "Invalid hand ~s provided when notifying motor interface for set-hand-position." (first p))))
                     (print-warning "Invalid x and/or y position ~s and ~s provided when notifying motor interface for set-hand-position." 
                                    (second p) (third p))))
|#

;(defclass finger-spacing ()
;  ((hand :type `(member-of +hand-names+) :initarg :hand :initform 'right :reader hand)))
   




;;; eof