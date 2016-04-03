
; (ql:system-apropos "cl-glut-examples") 



(ql:quickload :trivial-main-thread)
(ql:quickload :cl-glut-examples)

; (compile-file "package.lisp")

(sb-thread:list-all-threads)

(sb-thread:interrupt-thread)
