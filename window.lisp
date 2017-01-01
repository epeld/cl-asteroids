
(in-package :window)

(defclass base-window (glut:window)
  ((event-callback :type function
               :accessor event-fn
               :documentation "Callback, accepting input events")
   (renderer :accessor render-fn
             :documentation "The function that is to be used for rendering"
             :initform (constantly nil)))
  (:documentation "Just a base class so we can do some initialization"))


;; This is useful during development since it allows us to keep
;; the main loop running
(defmacro with-ignore-restart (&body body)
  "Adds a restart that allows the user to ignore this error."
  `(restart-case
       (progn ,@body)
     (ignore ()
       :report "Ignore this error")))

;;
;; Rendering

(defmethod glut:display-window :before ((w base-window))
  (glut:ignore-key-repeat 1)
  (gl:clear-color 0 0.2 0.3 0))

(defmethod glut:display ((window base-window))
  (with-ignore-restart
    (funcall (render-fn window))))

;;
;; Event Processing

(declaim (inline report-event))
(defun report-event (window event)
  "Report an event"
  (when (with-ignore-restart
          (funcall (event-fn window) event))
    (glut:post-redisplay)))

(defun make-event (&rest args)
  "Construct an input event"
  (the keyword (first args))
  args)

(defmethod glut:tick ((window base-window))
  (report-event window (make-event :tick)))


;;
;; Screenshots
;; 
(defun get-pixels (window)
  (with-ignore-restart
    (let* ((width (glut:width window))
           (height (glut:height window))
           (data (gl:read-pixels 0 0 width height :rgb :unsigned-byte)))

      (zpng:write-png
       (make-instance 'zpng:png
                      :color-type :truecolor
                      :width width
                      :height height
                      :image-data
                      (make-array (length data) :element-type '(unsigned-byte 8)
                                  :initial-contents data))
       "test.png"))))

;; 
;; Event Callbacks

;; TODO define event classes
(defmethod glut:keyboard ((w base-window) key x y)
  (when (eq key #\k)
    (get-pixels w))
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
                     (renderer (constantly nil))
                     (tick-interval 1000))
  "Display a window, and run the event loop"
  (the function event-callback)

  ;; TODO use trivial-main-thread to always call this
  ;; from the main thread.

  ;; TODO wrap callback funcalls in error handler
  ;; to avoid aborting main thread

  (let ((window (make-instance 'base-window
                               :title title
                               :tick-interval tick-interval)))

    
    (setf (event-fn window) event-callback)
    (setf (render-fn window) renderer)

    (report-event window (make-event :enter-event-loop title))
   
    ;; This will block the current thread until window is closed:
    (glut:display-window window)
    (glut:disable-tick window)

    (report-event window (make-event :exit-event-loop title))))
