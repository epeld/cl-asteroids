
(in-package :asteroids)

(defvar *window* nil)

(defmacro with-temporary-matrix (&body body)
  `(progn (gl:push-matrix)
          (unwind-protect 
               (progn ,@body)               
            (gl:pop-matrix))))

(defmacro with-option-to-move-on (&body forms)
  `(restart-case 
      (progn ,@forms)
    (move-on ()
      :report "Move on, ignoring the error")))

(defclass game-object ()
  ((x :accessor x-position
      :initform 0
      :initarg :x)
   
   (y :accessor y-position
      :initform 0
      :initarg :y)
   
   (angle :accessor angle
          :initform 0)
   
   (angular-velocity :accessor angular-velocity
                      :initform 0)
   
   (x-heading :accessor x-heading
              :initform 0)

   (y-heading :accessor y-heading
              :initform 0)))


(defun wrap-position (object &optional (w 0.5) (h 0.5))
  
  ;; Add 10% to the dimensions to make sure the whole object is outside the screen
  (let ((w (* w 1.1)))
    (with-slots (x) object
      (if (< x (- w))
          (setf x w))
    
      (if (> x w)
          (setf x (- w)))))
  
  (let ((h (* h 1.1)))
    (with-slots (y) object
    
      (if (< y (- h))
          (setf y h))
    
      (if (> y h)
          (setf y (- h))))))


(defun integrate-object (object delta)
  (with-slots (x y angle x-heading y-heading angular-velocity) object
    
    ;; Scale by passed amount of time!
    (let ((hx (* x-heading delta))
          (hy (* y-heading delta))
          (av (* angular-velocity delta)))
      
      (incf x hx)
      (incf y hy)
      (incf angle av))))


(defgeneric draw-object (game-object))
(defgeneric update-object (game-object number))



(defvar test-object (make-instance 'game-object))



(defmethod update-object ((object game-object) (delta number))
  (integrate-object object delta)
  (wrap-position object))


(defmethod draw-object :around ((object game-object))
  (with-slots (x y angle) object
    (with-temporary-matrix
      (gl:translate x y 0)
      (gl:scale 0.1 0.1 1)            ; TODO let object determine its size?
      (gl:rotate angle 0 0 1)
      (call-next-method))))


;; TODO this needs some work..
(defun spaceship-vertices ()
  (let ((vs (circle-vertices 3)))
    (cons (loop for coord in (car vs) collect (* 1.3 coord)) (cdr vs))))


(defmethod draw-object ((object game-object))
  (gl:color 0.8 0.2 0.1)
  (polygon (spaceship-vertices)))


(defclass game-window (glut:window integrator)
  (asteroids spaceship)
  (:default-initargs :pos-x 100 :pos-y 100 :width 500 :height 500
                     :tick-interval 20
                     :mode '(:single :rgb) :title "Asteroids"))


(defmethod get-current-time ((window game-window))
  (/ (glut:get :elapsed-time) 1000))


(defmethod integrate ((window game-window))
  (with-slots (asteroids spaceship) window
    (let ((delta (compute-delta window)))
      
      (loop for asteroid in *asteroids*
         do (update-object asteroid delta))
      (update-object test-object delta)
      (quote (update-object spaceship delta)))))



(defmacro with-periodic-rotation (period &body body)
  (let ((elapsed (gensym)))
    `(let ((,elapsed (glut:get :elapsed-time)))
         (rotation ,elapsed ,period)
         ,@body)))


(defmethod glut:display-window :before ((w game-window))
  ;; Select clearing color.
  (gl:clear-color 0 0 0 0)
  ;; Initialize viewing values.
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 1 0 1 -1 1))


(defun asteroid-vertices (&optional (n 5))
  (loop for i upto n
     collect (let* ((rads (/ (* 2 pi i) n))
                    (radius (+ 0.7 (* 0.3 (cos (+ (/ pi 7) (* 3 rads)))))))
               (list (* radius (cos rads)) 
                     (* radius (sin rads))
                     0))))


(let* ((max-count 15)
       (cache (make-array max-count :initial-element nil)))
  
  (defun circle-vertices (&optional (n 5))
    (assert (< n max-count))
    (or (aref cache n)
        (setf (aref cache n) 
              (loop for i upto n
                 collect (let ((rads (/ (* 2 pi i) n)))
                           (list (cos rads) (sin rads) 0)))))))


(defun polygon (vertices)
  (gl:with-primitive :polygon
    (loop for vertex in vertices
       do (apply #'gl:vertex vertex))))


(defun rotation (elapsed period)
  (gl:rotate (* (/ 360 period) (mod elapsed period)) 0 0 1))


(defun thrust (object)
  
  (with-slots (angle x-heading y-heading) object
    (setf x-heading (* 0.2 (cos angle)))
    (setf y-heading (* -0.2 (sin angle)))))


(defun stop-moving (object)
  (setf (y-heading object) 0)
  (setf (x-heading object) 0))

(stop-moving test-object)
(setf (y-heading test-object) 0.05)
(float (mod (angle test-object) 360))

(defmethod glut:keyboard ((w game-window) key x y)
  (case key 
    (#\a (setf (angular-velocity test-object) 90))
    (#\d (setf (angular-velocity test-object) -90))
    (#\w (thrust test-object))))


(defmethod glut:keyboard-up ((w game-window) key x y)
  (case key 
    (#\a (setf (angular-velocity test-object) 0))
    (#\d (setf (angular-velocity test-object) 0))))

(defmethod glut:display ((w game-window))
  (with-option-to-move-on
    (gl:clear :color-buffer)
    (gl:color 0.3 0.4 1)
    
    (gl:matrix-mode :modelview)
    (with-temporary-matrix
      (gl:translate 0.5 0.5 0)
      (loop for a in *asteroids*
           do (draw-object a))
      (draw-object test-object))
    
    (gl:flush)))


(defmethod glut:tick ((window game-window))
  (with-option-to-move-on 
    (integrate window)
    (glut:post-redisplay)))


(defun run-game ()
  (let ((win (make-instance 'game-window)))
    (setf *window* win)
    (setf (glut:title win) "Hello")
    (glut:display-window win)))


(quote (gui-thread:with-body-in-gui-thread 
         (run-game)))

(quote (gui-thread:with-body-in-gui-thread 
         (glut:leave-main-loop)))


