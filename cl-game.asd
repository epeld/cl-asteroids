;;;; cl-game.asd

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
	       (:file "asteroid")))

