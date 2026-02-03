;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; load-models.lisp
;;;
;;;

;;; Defines the models root directory
(defparameter *models-dir*
  (make-pathname :directory (pathname-directory *load-truename*)))


;;; eof