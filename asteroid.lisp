
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


(defvar *max-ship-velocity*
  0.05
  "Max velocity for ship, in terms of ship-lengths per second")

(defclass ship ()
  ((position :type list
             :accessor ship-position
             :initform (vector-zero)
             :documentation "The ship's position")
   (rotation :type number
             :accessor ship-rotation
             :initform 0
             :documentation "The ship's rotation")
   (heading :type list
            :accessor ship-heading
            :initform (vector-zero)
            :documentation "The ship's heading"))
  (:documentation "The entity representing the player's ship"))


(defclass missile ()
  ((position :type list
             :accessor ship-position
             :initform (vector-zero)
             :documentation "The ship's position")
   (rotation :type number
             :accessor ship-rotation
             :initform 0
             :documentation "The ship's rotation"))
  (:documentation "The projectile that the ship can fire"))


(defclass player-state ()
  ((turning :type keyword
            :accessor player-turning
            :initform nil
            :documentation "Indicates if the direction the player is turning")
   (thrusting :type thrusting
              :accessor player-thrusting
              :initform nil
              :documentation "Indicates if the player is trying to move forward"))
  (:documentation "Represents information about a player (human or computer)"))


(defclass asteroids-game ()
  ((ship :type ship
         :accessor asteroids-ship
         :initarg :ship
         :documentation "The ship belonging to the player")
   
   (player-state :type player-state
                 :accessor game-player
                 :initform (make-instance 'player-state))

   (projectile :type missile
               :accessor asteroids-projectile
               :initform nil
               :documentation "The one live projectile instance")
   )
  (:documentation "A game of asteroids"))


(defun new-game ()
  "Create a new game"
  (make-instance 'asteroids-game
                 :ship (make-instance 'ship)))

(defun notify-event (game event)
  "Notify the game that an input event happened"
  (format t "~a~%" event)
  ;(format t "Event ~a. Rotation: ~a~%" event (ship-rotation (asteroids-ship game)))
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
                  :thrusting))))

    (:keyup
     (case (second event)
       (#\w (setf (player-thrusting (game-player game))
                  nil))

       (#\a (setf (player-turning (game-player game))
                  nil))

       (#\d (setf (player-turning (game-player game))
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

  ;; Draw a central dot
  (when nil
    (gl:begin :points)
    (gl:vertex 0 0)
    (gl:end))

  ;;
  ;; For debugging:
  ;;


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


(defun render-game (game)
  "Render a game"
  (gl:color 0 0 0)

  ;; Clear previous screen
  (gl:clear-color 0 0.2 0.3 0)
  (gl:clear :color-buffer)

  ;; Ship
  (with-slots (ship) game
    (render-ship ship))

  ;; TODO rocks
  
  (glut:swap-buffers))


;;
;; Update logic

(defun thrust-ship (ship tstep)
  "Apply thrust to ship"
  (setf (ship-heading ship)
        (vector-limit *max-ship-velocity*
                      (vector-add (ship-heading ship)
                                  (vector-scale (* tstep 0.1)
                                                (vector-unit (ship-rotation ship)))))))


(defun turn-ship (ship tstep turning)
  "Turn ship appropriately"
  (case turning
    (:left
     (incf (ship-rotation ship)
           (* tstep 360)))

    (:right
     (decf (ship-rotation ship)
           (* tstep 360)))))


(defun update-game (game tstep)
  "Step the game forward `tstep` units of time"
  (let ((ship (asteroids-ship game)))
    (with-slots (turning thrusting) (game-player game)
      (when turning (turn-ship ship tstep turning))
      (when thrusting (thrust-ship ship tstep)))


    ;; Update position
    (setf (ship-position ship)
          (vector-add (ship-position ship) (ship-heading ship)))))

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
