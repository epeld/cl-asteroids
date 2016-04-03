
(defpackage :gui-thread
  (:use :simple-tasks :trivial-main-thread :common-lisp)
  (:export :with-body-in-gui-thread :call-in-gui-thread))
