;;;-*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-


; (push :actr-recompile *features*)

(load (make-pathname 
       :directory (append (pathname-directory (user-homedir-pathname)) 
                          '("Documents-Mac" "Lisp Projects" "act-r"))
       :name "load-actr+quicklisp" :type "lisp"))


(echo-act-r-output)

:eof