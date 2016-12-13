;;;; cl-game.asd

(quicklisp:quickload :cl-glut)

(asdf:defsystem #:cl-game
  :description "Describe cl-game here"
  :author "Erik Peldan <erik.peldan+lisp@gmail.com>"
  :license "None"
  :depends-on (#:simple-tasks
               #:trivial-main-thread
               #:cl-glut)
  :serial t
  :components ((:file "package")
               (:file "scene")
               (:file "window")))


(asdf:load-system :cl-game)
