
(in-package :window)

(defclass base-window (glut:window)
  ((event-callback :type function
               :accessor event-fn
               :documentation "Callback, accepting input events")
   (tick-callback :type function
                   :accessor tick-fn
                   :documentation "Callback, for when a 'tick' has happened")
   (scene :accessor next-scene
          :documentation "A reference to the next scene to render, if any."
          :initform nil))
  (:documentation "Just a base class so we can do some initialization"))


;;
;; Rendering

(defmethod glut:display-window :before ((w base-window))
  (gl:clear-color 0 0.5 0.2 0))

(defmethod glut:display ((w base-window))
  (gl:clear :color-buffer)

  ;; Poll for new scene to draw
  (when (next-scene w)
    (unwind-protect
         
         ;; Draw
         (progn (scene:render (next-scene w))
                (glut:swap-buffers))

      ;; Always clear the scene
      (setf (next-scene w) nil))))

;;
;; Event Processing

(defmethod glut:tick ((w base-window))
  (funcall (tick-fn w) w))

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

(defun log-tick (window)
  "Report a window tick"
  (format t "Tick! (~a)~%" (glut:title window)))

(defun event-loop (&key
                     (title (symbol-name (gensym "Foobar")))
                     (event-callback #'log-event)
                     (tick-callback #'log-tick)
                     (tick-interval 1000))
  "Display a window, and run the event loop"
  (the function event-callback)
  
  (funcall event-callback (make-event :enter-event-loop title))
  
  (let ((window (make-instance 'base-window
                               :tick-interval tick-interval)))
    (setf (glut:title window) title)
    (setf (event-fn window) event-callback)
    (setf (tick-fn window) tick-callback)
   
    ;; This will block the current thread until window is closed:
    (glut:display-window window)
    (glut:disable-tick window))

  (funcall event-callback (make-event :exit-event-loop title)))
