
(defpackage :foobar.window
  (:use :cl)
  (:nicknames :window))

(defpackage :foobar.scene
  (:use :cl)
  (:export :render)
  (:nicknames :scene))

(defpackage :foobar.asteroids
  (:use :cl)
  (:nicknames :asteroids))
