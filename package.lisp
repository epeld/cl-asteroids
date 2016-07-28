
; shadow? schedule-task etc
(defpackage :gui-thread
  (:use :trivial-main-thread :common-lisp)
  (:export :with-body-in-gui-thread :call-in-gui-thread))


(defpackage :asteroids
  (:use :gui-thread :common-lisp))
