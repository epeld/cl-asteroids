;;;; cl-game.asd

(quicklisp:quickload :cl-glut)
(quicklisp:quickload :cl-glu)

(asdf:defsystem #:cl-game
  :description "Describe cl-game here"
  :author "Erik Peldan <erik.peldan+lisp@gmail.com>"
  :license "None"
  :depends-on (#:simple-tasks
               #:trivial-main-thread
               #:cl-glut
               #:cl-glu)
  :serial t
  :components ((:file "package")
               (:file "window")
               (:file "asteroid")))


(asdf:load-system :cl-game)
