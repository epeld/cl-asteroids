;;;; cl-game.asd

(quicklisp:quickload :cl-glut)
(quicklisp:quickload :cl-glu)
(quicklisp:quickload :zpng)

(asdf:defsystem #:cl-game
  :description "Describe cl-game here"
  :author "Erik Peldan <erik.peldan+lisp@gmail.com>"
  :license "None"
  :depends-on (#:zpng
               #:cl-glut
               #:cl-glu)
  :serial t
  :components ((:file "package")
               (:file "window")
               (:file "asteroid")))


(asdf:load-system :cl-game)
