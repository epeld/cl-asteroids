
(in-package :window)

(defclass base-window (glut:window)
  ((event-callback :type function
               :accessor event-fn
               :documentation "Callback, accepting input events")
   (renderer :accessor render-fn
             :documentation "The function that is to be used for rendering"
             :initform (constantly nil)))
  (:documentation "Just a base class so we can do some initialization"))


;;
;; Rendering

(defmethod glut:display-window :before ((w base-window))
  (gl:clear-color 0 0.5 0.2 0))

(defmethod glut:display ((window base-window))
  (funcall (render-fn window)))

;;
;; Event Processing

(declaim (inline report-event))
(defun report-event (window event)
  "Report an event"
  (when (funcall (event-fn window) event)
    (glut:post-redisplay)))

(defun make-event (&rest args)
  "Construct an input event"
  (the keyword (first args))
  args)

(defmethod glut:tick ((window base-window))
  (report-event window (make-event :tick)))



;; 
;; Event Callbacks

(defmethod glut:keyboard ((w base-window) key x y)
  (report-event w (make-event :keydown key x y)))


(defmethod glut:special ((w base-window) special x y)
  (report-event w (make-event :keydown special x y :special)))


(defmethod glut:keyboard-up ((w base-window) key x y)
  (report-event w (make-event :keyup key x y)))


(defmethod glut:special-up ((w base-window) special x y)
  (report-event w (make-event :keyup special x y :special)))

;;
;; Event Loop
;;

(defun log-event (event)
  "Log an input event to stdout"
  (format t "~s~%" event))

(defun event-loop (&key
                     (title (symbol-name (gensym "Foobar")))
                     (event-callback #'log-event)
                     (tick-interval 1000))
  "Display a window, and run the event loop"
  (the function event-callback)
  
  (funcall event-callback (make-event :enter-event-loop title))
  
  (let ((window (make-instance 'base-window
                               :title title
                               :tick-interval tick-interval)))
    
    (setf (event-fn window) event-callback)
   
    ;; This will block the current thread until window is closed:
    (glut:display-window window)
    (glut:disable-tick window))

  (funcall event-callback (make-event :exit-event-loop title)))
