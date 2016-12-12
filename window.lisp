
(in-package :peldan.window)

(defclass base-window (glut:window)
  ()
  (:documentation "Just a base class so we can do some initialization"))

(defmethod glut:display-window :before ((w base-window))
  (gl:clear-color 0 0.5 0.2 0))

(defmethod glut:display ((w base-window))
  (gl:clear :color-buffer)
  (glut:swap-buffers))

;;
;; Window Management
;; 
(defun create-window (&optional (title "Super Duper Window"))
  "Create a new window, pushing it to the global window list *windows*"
  (let ((win (make-instance 'base-window)))
    (setf (glut:title win) title)
    win))


(defun event-loop ()
  "Display a window, and run the event loop"
  (glut:display-window (create-window)))
