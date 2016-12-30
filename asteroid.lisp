
(in-package :asteroids)

;;
;; Vector math
(declaim (inline vector-add))
(declaim (inline vector-scale))
(declaim (inline vector-unit))
(declaim (inline vector-zero))
(declaim (inline vector-dot))
(declaim (inline vector-limit))
(declaim (inline vector-norm-2))


(defun vector-zero ()
  "Return the zero vector"
  (list 0 0))

(defun vector-add (v1 v2)
  "Add two 2d-vectors"
  (list (+ (first v1) (first v2))
        (+ (second v1) (second v2))))


(defun vector-scale (k v)
  "Scale a vector by a coefficient 'k'"
  (list (* k (first v))
        (* k (second v))))


(defun vector-unit (angle)
  "Return a unit vector with the given angle"
  (let ((rads (/ (* 2 pi angle)
                 360)))
    (list (cos rads) (sin rads))))


(defun vector-dot (v1 v2)
  "Calculate the dot product of two vectors"
  (+ (* (first v1) (first v2))
     (* (second v1) (second v2))))

(defun vector-norm-2 (v)
  "Return a vector's length, squared"
  (vector-dot v v))


(defun vector-limit (norm v)
  "Limit a vector's length, forcing it to be less than 'norm'"
  (let ((actual-2 (vector-norm-2 v)))
    (if (> actual-2 (* norm norm))
        (vector-scale (/ norm (sqrt actual-2))
                      v)
        v)))


;;
;; Asteroids definitions
;; 


(defparameter *max-ship-velocity*
  1.0
  "Max velocity for ship, in terms of ship-lengths per second")

(defparameter *projectile-velocity*
  2.0
  "The velocity of the ship's missiles")


(defparameter *ship-thrust-acceleration*
  1.0)

(defclass ship ()
  ((position :type list
             :accessor object-position
             :initform (vector-zero)
             :documentation "The ship's position")
   (rotation :type number
             :accessor object-rotation
             :initform 0
             :documentation "The ship's rotation")
   (heading :type list
            :accessor object-heading
            :initform (vector-zero)
            :documentation "The ship's heading"))
  (:documentation "The entity representing the player's ship"))


(defclass projectile ()
  ((position :type list
             :accessor object-position
             :initform (vector-zero)
             :initarg :position
             :documentation "The object's position")
   (rotation :type number
             :accessor object-rotation
             :initarg :rotation
             :initform 0
             :documentation "The object's rotation")
   (heading :type list
            :accessor object-heading
            :initform (vector-zero)
            :initarg :heading
            :documentation "The object's heading")
   (lifetime :type number
             :accessor object-lifetime
             :initform 0.5
             :initarg :lifetime
             :documentation "The object's lifetime, in time units"))
  (:documentation "The projectile that the ship can fire"))


(defclass rock ()
  ((position :type list
             :accessor rock-position
             :initform (vector-zero)
             :documentation "The rock's position")
   (rotation :type number
             :accessor rock-rotation
             :initform 0
             :documentation "The rock's rotation")
   (heading :type list
            :accessor rock-heading
            :initform (vector-zero)
            :documentation "The rock's heading"))
  (:documentation "The rocks that we want to shoot at"))


(defclass player ()
  ((turning :type keyword
            :accessor player-turning
            :initform nil
            :documentation "Indicates if the direction the player is turning")
   (thrusting :type keyword
              :accessor player-thrusting
              :initform nil
              :documentation "Indicates if the player is trying to move forward")
   (shooting :type keyword
              :accessor player-shooting
              :initform nil
              :documentation "Indicates if the player is trying to shoot"))
  (:documentation "Represents information about a player (human or computer)"))


(defclass asteroids-game ()
  ((ship :type ship
         :accessor asteroids-ship
         :initarg :ship
         :documentation "The ship belonging to the player")
   
   (player :type player
                 :accessor game-player
                 :initform (make-instance 'player))

   (projectile :type projectile
               :accessor asteroids-projectile
               :initform nil
               :documentation "The one live projectile instance")

   (rocks :type list
          :accessor asteroids-rocks
          :initform nil
          :documentation "The list of rocks"))
  
  (:documentation "A game of asteroids"))


(defun new-game ()
  "Create a new game"
  (make-instance 'asteroids-game
                 :ship (make-instance 'ship)))

(defun notify-event (game event)
  "Notify the game that an input event happened"
  ;(format t "~a~%" event)a
  ;(format t "Event ~a. Rotation: ~a~%" event (object-rotation (asteroids-ship game)))
  (case (first event)
    (:tick
     t)
    
    (:keydown
     (case (second event)
       (#\a (setf (player-turning (game-player game))
                  :left))

       (#\d (setf (player-turning (game-player game))
                  :right))

       (#\w (setf (player-thrusting (game-player game))
                  :thrusting))

       (#\Space (setf (player-shooting (game-player game))
                      :shooting))))

    (:keyup
     (case (second event)
       (#\w (setf (player-thrusting (game-player game))
                  nil))

       (#\a (setf (player-turning (game-player game))
                  nil))

       (#\d (setf (player-turning (game-player game))
                  nil))

       (#\Space (setf (player-shooting (game-player game))
                      nil))))))

;;
;;  Rendering
;;

;(declaim (inline gl-render-ship))
(defun gl-render-ship (x y rotation)
  "Perform the GL operations to render a ship"
  
  ;; Setup
  (gl:color 1.0 1.0 1.0)
  (gl:translate x y 0)
  (gl:scale 0.05 0.05 0)
  (gl:rotate rotation 0 0 1)

  ;; Draw
  (gl:begin :line-loop)
  (gl:vertex 1 0)
  (gl:vertex (cos (* 5 (/ pi 6))) (sin (* 5 (/ pi 6))))
  (gl:vertex (cos (* -5 (/ pi 6))) (sin (* -5 (/ pi 6))))
  (gl:end)

  ;;
  ;; For debugging:
  ;;
  ;; Draw a central dot
  (when nil
    (gl:begin :points)
    (gl:vertex 0 0)
    (gl:end))
  
  (when nil    
    (gl:begin :line-loop)
    (loop for i upto 20
       do
         (let ((rad (/ (* 2 pi i)
                       20)))
           (gl:vertex (cos rad) (sin rad))))
    (gl:end)))


(defun render-ship (ship)
  "Render a ship"
  (the ship ship)
  (with-slots (position rotation) ship
    (let ((x (car position))
          (y (cadr position)))

      (gl:push-matrix)
      (unwind-protect (gl-render-ship x y rotation)
        (gl:pop-matrix)))))


(defun render-projectile (projectile)
  "Render a projectile"
  (gl:push-matrix)
  (unwind-protect
       (with-slots (position rotation) projectile
         (let ((x (first position))
               (y (second position)))

           (gl:color 1.0 0.1 0.1)
           (gl:translate x y 0)
           (gl:scale 0.05 0.05 0)
           (gl:rotate rotation 0 0 1)

           (gl:begin :lines)
           (gl:vertex 0 0 0)
           (gl:vertex -0.5 0 0)
           (gl:end)))
    (gl:pop-matrix)))


(defun render-rock (rock)
  "Render a rock"
  ;; TODO use proper drawing routine
  (unwind-protect
       (with-slots (position rotation) rock
         (let ((x (first position))
               (y (second position)))

           (gl:color 1.0 1.0 1.0)
           (gl:translate x y 0)
           (gl:scale 0.05 0.05 0)
           (gl:rotate rotation 0 0 1)

           
           (gl:begin :line-loop)
           (loop for i upto 7
              do
                (let ((rad (/ (* 2 pi i)
                              20)))
                  (gl:vertex (cos rad) (sin rad))))
           (gl:end)))
    (gl:pop-matrix)))


(defun render-game (game)
  "Render a game"
  (gl:color 0 0 0)

  ;; Clear previous screen
  (gl:clear-color 0 0.2 0.3 0)
  (gl:clear :color-buffer)

  (with-slots (ship projectile rocks) game
    (render-ship ship)
    (when projectile
      (render-projectile projectile))
    (loop for rock in rocks do
         (render-rock rock)))

  (glut:swap-buffers))


;;
;; Update logic

(defun thrust-ship (ship tstep)
  "Apply thrust to ship"
  (setf (object-heading ship)
        (vector-limit *max-ship-velocity*
                      (vector-add (object-heading ship)
                                  (vector-scale (* tstep *ship-thrust-acceleration*)
                                                (vector-unit (object-rotation ship)))))))



(defun turn-ship (ship tstep turning)
  "Turn ship appropriately"
  (case turning
    (:left
     (incf (object-rotation ship)
           (* tstep 360)))

    (:right
     (decf (object-rotation ship)
           (* tstep 360)))))


(defun projectile-heading (rotation)
  "Calculates the heading vector of a projectile using a rotation angle"
  (vector-scale *projectile-velocity*
                (vector-unit rotation)))


(defun ship-projectile (ship)
  "Create a projectile emanating from ship"
  (make-instance 'projectile
                 :position (object-position ship)
                 :rotation (object-rotation ship)
                 :heading (projectile-heading (object-rotation ship))))


(defun update-position (object tstep)
  "Update a game object's position using its heading"
  (setf (object-position object)
        (vector-add (object-position object)
                    (vector-scale tstep (object-heading object)))))


(defun check-collision (object1 object2)
  "TODO"
  nil)


(defun spend-lifetime (object tstep)
  "Reduce an object's lifetime, returning the remaining amount of lifetime"
  (setf (object-lifetime object)
        (max 0 (- (object-lifetime object) tstep))))

(defun update-game (game tstep)
  "Step the game forward `tstep` units of time"
  (with-slots (ship projectile player rocks) game

    ;;
    ;; Player
    ;; 
    (with-slots (turning thrusting shooting) player
      (when turning (turn-ship ship tstep turning))
      (when thrusting (thrust-ship ship tstep))
      (when (and shooting (null projectile))
        (setf projectile (ship-projectile ship))))

    ;;
    ;; Ship
    ;; 
    (update-position ship tstep)

    ;;
    ;; Projectile
    ;; 
    (when projectile
      (update-position projectile tstep)

      (when (zerop (spend-lifetime projectile tstep))
        (setf projectile nil)))

    ;;
    ;; Rocks
    ;;
    (loop for rock in rocks
       do
         (update-position rock tstep)
       when
         (and projectile
              (check-collision projectile rock))
       collect
         rock)))

;;
;;  Top Level API
;; 
(defun game-loop (&key (game (new-game)))
  "Enter a game loop running the specified game"
  (window:event-loop :title "Asteroids"
                     :event-callback (lambda (event)
                                       (notify-event game event))
                     :renderer (lambda ()
                                 (update-game game 0.020)
                                 (render-game game))
                     :tick-interval 20))


;; TODO move rendering into asteroid-rendering.lisp
;; TODO move event processing into asteroid-events.lisp

(quote (game-loop))
