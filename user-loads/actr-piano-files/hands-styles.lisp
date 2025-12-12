;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; hands-styles.lisp
;;;
;;; 2025-11-19
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; From actr documentation: 
;;; The motor module has three internal states:
;;; preparation, processor, and execution. When a request is received by the motor module, it goes
;;; through three phases: preparation, initiation, and execution each of which depends upon a different
;;; combination of those internal states being available.
;;;
;;; The preparation phase determines which features are changing from the previous motor request. 
;;; The preparation cost for a style action will require the time to prepare n+1 features where n 
;;; is the number of features specified for the style, and the +1 is for the style itself.
;;;
;;; When feature preparation is complete, the motor module makes the specified movement. The movement
;;; is composed of 2 phases: initiation and execution.  
;;;
;;; Here is a summary of the transition states:
;;; Preparation state    Processor state    Execution state    When
;;;      FREE                FREE                FREE          Before event arrives
;;;      BUSY                BUSY                FREE          Preparation: When request is received
;;;      FREE                BUSY                BUSY          Initiation:  After preparation of movement
;;;      FREE                FREE                BUSY          Exacution:   After initiation of movement
;;;      FREE                FREE                FREE          When movement is complete

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TO DO
;;;
;;; There are 5 movement styles implemented in the act-r motor module. 
;;; Some of them might need to be adapted to the piano. 
;;;
;;; Punch. This is a simple downstroke followed by an upstroke of a finger (pressing a key or
;;;        button located directly beneath the finger).
;;; Peck.  This is a directed movement of a finger to a location followed by a keystroke, all as one
;;;        continuous movement.
;;; Peck-recoil. Same as "peck" above, but the finger moves back to the location at which it
;;;        started its movement.
;;; Ply.   Moving a device to a given location in space or to control the position of an associated
;;;        object e.g. the cursor on the screen by using the mouse.
;;; Hand-ply. Moves a hand to a new location in space e.g. from the keyboard to the mouse.


;;;;;;;;;;;;;;;;;;
;;;
;;; Piano hand styles. 
;;; The piano-hand style is a generic style to handle common methods of more specific
;;; styles. 

(defstyle piano-hand-style dual-hand-movement-style hand thumb index middle ring pinkie)

;;; piano-hand preparation parameters and methods.
;;;
;;; The default prep time for features is set to 0.05 per feature in the motor module.
;;; There is no need to change this default time value. 
;;; The total preparation time = feature count * module feature-prep-time (0.050)
;;; 

(defvar *piano-hand-features* '(hand thumb index middle ring pinkie))

(defun count-features (style1 style2)
  (let ((count 0))
    ;; adding one to the feature change cound count for every different values
    (dolist (feature *piano-hand-features* count)
      (unless (equalp (slot-value style1 feature) (slot-value style2 feature))
        (incf count)))))

(defmethod feat-differences ((style1 piano-hand-style) (style2 piano-hand-style))
  "This is a generic method assuming only *piano-hand-features* in a style. The features could be added in a style specific method."
  (count-features style1 style2))

;;; piano-hand initiation parameters and methods.
;;;
;;; The compute-exec-time method is called to determine
;;; how long after preparation of the style's action it will take
;;; until the action should be 'executed'.  It is called every
;;; time there is a request for the action and thus can change
;;; dynamically based on the features. However, for piano-hand styles
;;; it is assumed to be the same for every inherited styles. 

(defparameter *default-compute-exec-time* 0.05
  "Minimum time before execution can proceed after preparation is completed.")

(defmethod compute-exec-time ((m motor-module) (style piano-hand-style))
  ;; set as the value of the minimum compute execution time parameter. 
  (declare (ignore m style))
  *default-compute-exec-time*)

;;; The compute-finish-time method is called to determine how long
;;; after the preparation of the style's action it will take until
;;; the execution stage should complete and become free again.
;;; It must return a number which is the total execution stage
;;; time in seconds.  This is called after the exec-time is set.

;;; After calling compute-finish-time the style will set the 
;;; finish-time slot of the style to the result.

(defparameter *default-compute-finish-time* 0.05
  "Minimum finish time.")

(defmethod compute-finish-time ((m motor-module) (style piano-hand-style))
  (declare (ignore m))
  (+ *default-compute-finish-time* (exec-time style)))

;;;;;;;;;;;;;;;;;;
;;;
;;; Movement styles to be used in productions. 
;;;
;;; position-hand-fingers (hand &optional to-xy thumb index middle ring pinkie.
;;;   Only hand is required, however without a hand location or a finger offset the
;;;   request would have no effect. 
;;;

(when (find-class 'position-hand-fingers nil)
  (remove-manual-request position-hand-fingers))

(defstyle position-hand-fingers piano-hand-style hand to-xy thumb index middle ring pinkie)

(extend-manual-requests-fct 
 `(position-hand-fingers 
   hand 
   (to-xy ,+empty+) 
   (thumb ,+empty+) 
   (index ,+empty+) 
   (middle ,+empty+) 
   (ring ,+empty+) 
   (pinkie ,+empty+)) 
 'handle-style-request) 

;; preparation phase: prepare-features and count differences. 

(defun change-if-new-value (hand finger new-value)
  (when (and new-value (not (eq new-value +empty+)))
    (setf (finger-offset hand finger) (->vector new-value))))

(defmethod prepare-features ((m motor-module) (style position-hand-fingers))
  (let ((hand (hand style)))
    (initialize-next-hand-pos hand)
    (when (to-xy style) (setf (hand-xy hand) (->vector (to-xy style))))
    (dolist (finger +finger-names+
                    (setf (updated-pos style)
                          (hand-pos hand :current nil)))
      (change-if-new-value hand finger (funcall finger style)))))

(defmethod feat-differences ((style1 position-hand-fingers) (style2 position-hand-fingers))
  (let ((*piano-hand-features* (push 'to-xy *piano-hand-features*)))
    (count-features style1 style2)))


(defun set-current-hand-pos (hand)
  "Set the current hand state to its next state (:current nil)."
  (setf (hand-pos hand :current t) 
        (hand-pos hand :current nil)))

(defmethod queue-output-events ((m motor-module) (action position-hand-fingers))
  (schedule-event-relative 
   (exec-time action) 'set-current-hand-pos :params (list (hand action))))


;;;;;;;;;;;;;;;;;;
;;;
;;; activate-keys 
;;;

(defstyle activate-keys piano-hand-style thumb-force thumb-duration index-force index-duration 
          middle-forve middle-dutation ring-force ring-duration pinkie-force pinkie-duration)

;;; eof