;;;; cl-game.asd

(quicklisp:quickload :trivial-main-thread)
(quicklisp:quickload :simple-tasks)
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
	       (:file "physics")
	       (:file "game")
	       (:file "asteroid")
               (:file "window")))


(asdf:load-system :cl-game)
