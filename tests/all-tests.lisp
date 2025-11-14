;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; all-tests.lisp
;;;
;;; 2025-08-29
;;;
(let ((dir (make-pathname :directory (pathname-directory *load-truename*)))
      (file-names
       '(
         "typep-slots-tests.lisp"
         "types-tests.lisp"
         "device-interface-tests.lisp"
         "visicon-object-tests.lisp"
         "hands-tests.lisp"
         ;"midi-utilities-tests.lisp"
         ;"piano-keyboard-tests.lisp"
         )))
  (dolist (file-name file-names file-names)
    (load (merge-pathnames file-name dir))))
  
;;; eof