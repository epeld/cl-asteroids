
; (ql:system-apropos "cl-glut-examples") 


;(load "~/Downloads/quicklisp.lisp")
;(quicklisp-quickstart:install)

(ql:quickload :trivial-main-thread)
(ql:quickload :cl-glut)

(quickproject:make-project #p"~/Documents/Code/cl-game/" :depends-on '(simple-tasks trivial-main-thread cl-opengl))

(ql:quickload "quickproject")
