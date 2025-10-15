;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;
;;; actr-piano-files.lisp
;;;
;;; 2025-08-29
;;;

(let ((dir (make-pathname :directory (pathname-directory *load-truename*)))
      (file-names
       '(
         "actr-piano-files/utilities.lisp"
         "actr-piano-files/typep-slots.lisp"
         "actr-piano-files/types.lisp"
         "actr-piano-files/device-interface.lisp"
         "actr-piano-files/visicon-object.lisp"
         "actr-piano-files/hands.lisp"
         )))
  (dolist (file-name file-names file-names)
    (load (merge-pathnames file-name dir))))

;;; eof