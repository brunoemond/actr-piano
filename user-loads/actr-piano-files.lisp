;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; actr-piano-files.lisp
;;;
;;; 2025-08-29
;;;
(setf (logical-pathname-translations "actr-piano")
      `(("**;*.*.*" 
         ,(merge-pathnames "**/" (make-pathname :directory (pathname-directory *load-truename*))))))

(let ((actr-piano-files
       '(
         ;; Common lisp utilities
         "typep-slots.lisp"

         )))

  (dolist (file-name actr-piano-files actr-piano-files)
    (load (concatenate 'string "actr-piano:actr-piano-files;" file-name))))
       

;;; eof