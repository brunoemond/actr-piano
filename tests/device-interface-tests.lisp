;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; device-interface-tests.lisp
;;;
;;; 2025-08-15
;;;

(print ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; device-interface" *standard-output*)

;; define a device instance
(defclass some-device () ((some-slot :initform 'abc :accessor some-slot)))

;; add commands for object accessor
(add-act-r-command "some-slot" 'some-slot)

;; defines a monitor command for an after execution monitoring
(defun some-slot-m (function-name params success result)
    (print (list function-name params success result)))

(add-act-r-command "some-slot-m" 'some-slot-m)
(monitor-act-r-command "some-slot" "some-slot-m" :after)

(clear-all)
(define-model test)
(with-model test
  ;;;
  (print "no device instance" *standard-output*)
  ;;;
  (assert (null (device-instance "test")))

  ;;;
  (print "add device instance" *standard-output*)
  ;;;
  (setf (device-instance "test") (make-instance 'some-device))
  (assert (device-instance "test"))

  ;;;
  (print "execute command from different device references and print the monitor parameters" *standard-output*)
  ;;;
  (assert (eq 'abc (apply-device-command (make-instance 'some-device) '(:command "some-slot"))))
  (assert (eq 'abc (apply-device-command "test" '(:command "some-slot"))))

  ;; remove device, commands and unintern device class
  (setf (device-instance "test") nil)

  )

(remove-act-r-command "some-slot")
(remove-act-r-command "some-slot-m")
(unintern 'some-device)

;;; eof