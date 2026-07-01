;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; hands.lisp
;;;
;;; 2025-11-03
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

;; hand-pos current and next
(defun hand-pos (hand-name &key current)
  (let ((next-loc (next-loc (the-hand hand-name))))
    (if current
        (loc (the-hand hand-name))
      next-loc)))

(defmethod (setf hand-pos) (hand-pos hand-name &key current)
  (set-hand-position (motor-module) hand-name hand-pos :current current))

(defun initialize-next-hand-pos (hand-name)
  (setf (hand-pos hand-name :current nil)
        (copy-hand-pos (hand-pos hand-name :current t))))

(defun clear-next-hand-pos (hand-name)
  (setf (hand-pos hand-name :current nil) nil))

                            
;; hand-xy
(defun hand-xy (hand-name &key current)
  "Returns the current or next xy coordinate of a hand."
  (if (typep hand-name `(member-of ,+hand-names+))
      (hand-pos-loc (hand-pos hand-name :current current))     
    (print-warning "~S is not a hand name ~S." hand-name +hand-names+)))

(defmethod (setf hand-xy) (xy hand-name &key current)
  (setf (hand-pos-loc (hand-pos hand-name :current current)) xy))

;; finger-offsets
(defun finger-offsets (hand-name &key current)
  (hand-pos-fingers (hand-pos hand-name :current current)))

(defmethod (setf finger-offsets) (offsets-list hand-name &key current)
  (setf (hand-pos-fingers (hand-pos hand-name :current current))
        offsets-list))

;; finger-offset
(defun finger->vector (finger list)
  (second (assoc finger list)))

(defun finger-offset (hand-name finger &key current)
  (finger->vector finger (finger-offsets hand-name :current current)))

(defmethod (setf finger-offset) (xy hand-name finger &key current)
  (setf (cdr (assoc finger (finger-offsets hand-name :current current)))
        (list xy)))

;; finger-xy
(defun finger-xy (hand-name finger &key current)
  (if (and (typep finger `(member-of ,+finger-names+))
           (typep hand-name `(member-of ,+hand-names+)))
      (vector+ (hand-xy hand-name :current current)
               (finger-offset hand-name finger :current current))
    (print-warning "Either ~S is not a hand name ~S, or ~S is not a finger name ~S."
                   hand-name +hand-names+ finger +finger-names+)))

(defmethod (setf finger-xy) (xy hand-name finger &key current)
  (setf (finger-offset hand-name finger :current current) 
        (vector- xy (hand-xy hand-name :current current))))


;; change-anchor-finger
(defun copy-offsets (hand-name &key current)
  (copy-list (finger-offsets hand-name :current current)))

(defun offset-vector (start end)
  (vector- end start))

(defun offsets->xys (hand-xy offsets)
  (mapcar (lambda (offset)
            (list (first offset) 
                  (vector+ hand-xy (second offset))))
        offsets))

(defun change-anchor-finger (hand-xy offsets finger)
  (let* ((finger-xys (offsets->xys hand-xy offsets))
         (fxy (finger->vector finger finger-xys)))
    (mapcar (lambda (finger-xy)
              (list (first finger-xy)
                    (vector- (second finger-xy) fxy)))
            finger-xys)))

;; place hands on the keyboard
(defun default-finger-offsets (hand-name &optional (finger-spacing 1))
  "Assumes that the default is that all fingers are evenly spaced and aligned on the x-axis (constant y value) with the thumb offset being (0 0)."
  (let ((direction (ecase hand-name (right 1) (left -1))))
    `((thumb (0 0))
      (index (,(* direction finger-spacing) 0))
      (middle (,(* 2 direction finger-spacing) 0))
      (ring (,(* 3 direction finger-spacing) 0))
      (pinkie (,(* 4 direction finger-spacing) 0)))))

(defun place-hand-on-keyboard (hand-name anchor-finger hand-xy &optional (finger-spacing 1))
  (if (and (typep hand-name `(member-of ,+hand-names+))
           (typep anchor-finger `(member-of ,+finger-names+)))
      (progn
        (setf (hand-xy hand-name :current t) 
              (->vector hand-xy)
              (finger-offsets hand-name :current t) 
              (change-anchor-finger hand-xy
                                    (default-finger-offsets hand-name finger-spacing)
                                    anchor-finger))
        (hand-pos hand-name :current t))
    (print-warning "Either ~S is not a hand name ~S, or ~S is not a finger-name ~S."
                   hand-name +hand-names+ anchor-finger +finger-names+)))



;;; eof