
(in-package :peldan.window)

(defclass base-window (glut:window)
  ((event-sink :type function
               :accessor event-fn
               :documentation "Callback, accepting input events")
   (message-source :type function
                   :accessor message-fn
                   :documentation "Zero-arg polling function for getting commands (messages)")
   (scene :accessor next-scene
          :documentation "A reference to the next scene to render, if any."))
  (:documentation "Just a base class so we can do some initialization"))


;;
;; Rendering

(defmethod glut:display-window :before ((w base-window))
  (gl:clear-color 0 0.5 0.2 0))

(defmethod glut:display ((w base-window))
  (gl:clear :color-buffer)

  ;; Poll for new scene to draw
  (when (next-scene w)
    (scene:render (next-scene w))
    (glut:swap-buffers)))

;;
;; Messages

(defun make-message (type payload)
  (the keyword type)
  (list type payload))

(defun message-type (message)
  (the keyword (first message)))

(defun message-payload (message)
  (second message))

(defun process-messages (w)
  "Process window commands and find the latest scene"
  (do (scene
       (msg (funcall (message-fn w)) (funcall (message-fn w))))
      ((null msg)
       (when scene
         (setf (next-scene w) scene)
         (glut:post-redisplay)))

    (case (message-type msg)
      (:scene (setf scene (message-payload msg)))
      (:command (process-command w (message-payload msg))))))

;;
;; Event Processing

(defmethod glut:tick ((w base-window))
  (process-messages w))

(declaim (inline report-event))

(defun report-event (window event)
  "Report an event"
  (funcall (event-fn window) event))


(defun make-event (&rest args)
  "Construct an input event"
  (the keyword (first args))
  args)

;; 
;; Event Callbacks

(defmethod glut:keyboard ((w base-window) key x y)
  (report-event w (make-event :keydown key x y)))


(defmethod glut:special ((w base-window) special x y)
  (report-event w (make-event :keydown special x y)))


(defmethod glut:keyboard-up ((w base-window) key x y)
  (report-event w (make-event :keyup key x y)))


(defmethod glut:special-up ((w base-window) special x y)
  (report-event w (make-event :keyup special x y)))

;;
;; Event Loop
;;

(defun event-loop (event-sink scene-source &optional (title "Foobar"))
  "Display a window, and run the event loop"
  (the function event-sink)
  (the function scene-source)
  
  (funcall event-sink (make-event :enter-event-loop title))
  
  (let ((window (make-instance 'base-window)))
    (setf (glut:title window) title)
    (setf (event-fn window) event-sink)
    (setf (scene-fn window) scene-source)

    ;; This will block the current thread until window is closed:
    (glut:display-window window))
  
  (funcall event-sink (make-event :exit-event-loop title)))
