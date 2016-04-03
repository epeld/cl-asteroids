(in-package :gui-thread)


(defclass gui-task (blocking-call-task) ())


(defmethod run-task ((task gui-task))
  (handler-bind ((condition (lambda (c) (invoke-debugger c))))
    (call-next-method)))


(defun call-in-gui-thread (function &key (runner trivial-main-thread:*runner*))
  (ensure-main-runner-started :runner runner)
  (simple-tasks:call-as-task
   function runner
   'gui-task))


(defmacro with-body-in-gui-thread (&body body)
  `(call-in-gui-thread (lambda () ,@body)))
