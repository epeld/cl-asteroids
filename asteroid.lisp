
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
(declaim (inline vector-norm))


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

(defun vector-norm (v)
  "Return a vector's length"
  (sqrt (vector-norm-2 v)))


(defun vector-limit (norm v)
  "Limit a vector's length, forcing it to be less than 'norm'"
  (let ((actual-2 (vector-norm-2 v)))
    (if (> actual-2 (* norm norm))
        (vector-scale (/ norm (sqrt actual-2))
                      v)
        v)))


(defun vector-warp (v top-left bottom-right)
  "'Warp' a vector to stay inside the indicated rectangle"
  (let ((x (if (< (first v)
                  (first top-left))
               (first bottom-right)
               (if (< (first bottom-right)
                      (first v))
                   (first top-left)
                   (first v))))
        
        (y (if (< (second v)
                  (second top-left))
               (second bottom-right)
               (if (< (second bottom-right)
                      (second v))
                   (second top-left)
                   (second v)))))
    (list x y)))

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

(defparameter *rock-minimum-split-scale*
  0.05)


(defclass physical-object ()
  ((position :type list
             :accessor object-position
             :initform (vector-zero)
             :initarg :position
             :documentation "The object's position")
   (scale :type number
          :accessor object-scale
          :initform 0.05
          :initarg :scale
          :documentation "The object's scale (used for collision detection and rendering)")
   (rotation :type number
             :accessor object-rotation
             :initform 0
             :initarg :rotation
             :documentation "The object's rotation")
   (rotation-speed :type number
                   :accessor object-rotation-speed
                   :initform 0
                   :initarg :rotation-speed
                   :documentation "Rate of change of the object's rotation")
   (heading :type list
            :accessor object-heading
            :initform (vector-zero)
            :initarg :heading
            :documentation "The object's heading")
   (warps :accessor object-warps-p
          :initform t
          :documentation "Flag indicating if this object will warp when reaching screen edges"))
  (:documentation "Represents something in the game with physical properties"))


(defclass ship (physical-object)
  ((alive :initform t
          :accessor object-alive-p
          :documentation "Flag indicating if ship is alive"))
  (:documentation "The entity representing the player's ship"))


(defclass rock (physical-object)
  ((num-vertices :type number
                 :accessor rock-num-vertices
                 :initform 9
                 :initarg :num-vertices
                 :documentation "The vertex count to use when rendering"))
  (:documentation "The rocks that we want to shoot at"))


(defclass projectile (physical-object)
  ((lifetime :type number
             :accessor object-lifetime
             :initform 1.0
             :initarg :lifetime
             :documentation "The object's lifetime, in time units"))
  (:documentation "The projectile that the ship can fire"))

(defmethod initialize-instance :after ((projectile projectile) &rest args)
  (declare (ignore args))
  (setf (object-warps-p projectile)
        nil))


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
           :initform (make-instance 'player)
           :documentation "Information about what the human player is up to")

   (projectile :type projectile
               :accessor asteroids-projectile
               :initform nil
               :documentation "The one live projectile instance")

   (rocks :type list
          :accessor asteroids-rocks
          :initform nil
          :initarg :rocks
          :documentation "The list of rocks"))
  
  (:documentation "A game of asteroids"))


(defun random-rock ()
  "Create a random rock"
  (let ((r (+ 0.5 (random 1.0)))
        (arg (random 360.0)))
    
    (make-instance 'rock
                 :position (vector-scale r (vector-unit arg))
                 :heading (vector-scale 0.1 (vector-unit (random 360)))
                 :rotation 0
                 :scale (+ 0.05 (random 0.1))
                 :num-vertices (+ 7
                                  (* 2 (random 2)))
                 :rotation-speed (- (random 360)
                                    180))))

(defun new-game ()
  "Create a new game"
  (make-instance 'asteroids-game
                 :ship (make-instance 'ship)
                 :rocks (loop repeat 10 collect (random-rock))))

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

(defmacro with-temp-matrix (&body body)
  "Pushes the current matrix, executes body, then pops"
  `(progn (gl:push-matrix)
          (unwind-protect
               (progn ,@body))
          (gl:pop-matrix)))

(defmacro with-gl-primitives (primitive-type &body body)
  "Wrap body in a (gl:begin primitive-type) (gl:end) pair"
  `(progn (gl:begin ,primitive-type)
          (unwind-protect
               (progn ,@body)
            (gl:end))))

(defmacro with-object-coords (object &rest clauses)
  "Temporarily switch to an object-centric coordinate system.
Expects clauses of the form: (primitive-type body*) which will be used in with-gl-primitives to set up a gl rendering context."
  `(with-temp-matrix
     (gl-object-transform ,object)
     ,@(loop for clause in clauses
          unless
            (eq (first clause)
                :ignore)
          collect
            `(with-gl-primitives ,(first clause)
               ,@ (rest clause)))))


(declaim (inline gl-vertex-unit))
(defun gl-vertex-unit (radians &optional (radius 1.0))
  "Use gl:vertex with a point from the x-y plane unit circle"
  (gl:vertex (* radius (cos radians)) (* radius (sin radians)) 0))


(declaim (inline gl-object-transform))
(defun gl-object-transform (object)
  "Translate the coordinate system so that object is at center and a ship has length 1.0.
Rotate the axes so that the x-axis is aligned with the object"
  (with-slots (position rotation scale) object
    (let ((x (car position))
          (y (cadr position)))

      (gl:translate x y 0)
      (gl:scale scale scale 0)
      (gl:rotate rotation 0 0 1))))


(defun render-ship (ship)
  "Render a ship"
  (the ship ship)
  (gl:color 1.0 1.0 1.0)
  
  (with-object-coords ship
    (:line-loop
     (gl-vertex-unit 0)
     (gl-vertex-unit (* 5 (/ pi 6)))
     (gl-vertex-unit (* -5 (/ pi 6))))

    (:ignore (:line-loop
              (loop for i upto 20 do
                   (gl-vertex-unit (/ (* 2 pi i)
                                      20)))))

    (:points
     (gl:vertex 0 0))))


(defun render-projectile (projectile)
  "Render a projectile"
  (gl:color 1.0 0.1 0.1)
  (with-object-coords projectile
    (:lines
     (gl:vertex 0 0 0)
     (gl:vertex -0.5 0 0)
     (gl:vertex 0 0.25 0)
     (gl:vertex 0 -0.25 0))
    (:points
     (gl:vertex -1.0 0 0))))


(defun render-rock (rock)
  "Render a rock"
  (gl:color 0.7 0.8 0.7)
  (with-object-coords rock
    (:line-loop
     (let ((num-vertices (rock-num-vertices rock)))
       (flet ((vertex (i)
                (let ((rad (/ (* 2 pi i)
                              num-vertices))
                      (r (cond ((zerop (mod i 7))
                                0.5)

                               ((and (< num-vertices 8)
                                     (zerop (mod i 4)))
                                0.7)

                               (t 1.0))))
                  
                  (gl-vertex-unit rad r))))
         (loop for i upto num-vertices do (vertex i)))))))


(defun render-game (game)
  "Render a game"
  (gl:color 0 0 0)

  ;; Clear previous screen
  (gl:clear-color 0 0.2 0.3 0)
  (gl:clear :color-buffer)

  (with-slots (ship projectile rocks) game
    (when (object-alive-p ship)
      (render-ship ship))
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


(defun warp-object (object top-left bottom-right)
  "Warp an object, forcing it to stay inside the rect"
  (setf (object-position object)
        (vector-warp (object-position object)
                     top-left bottom-right)))


(defun update-rotation (object tstep)
  "Ingerate an object's rotation"
  (incf (object-rotation object)
        (* tstep
           (object-rotation-speed object))))


(defun update-position (object tstep)
  "Update a game object's position using its heading"
  (setf (object-position object)
        (vector-add (object-position object)
                    (vector-scale tstep (object-heading object)))))


(defgeneric update-object (object tstep)
  (:documentation "Update an object"))


(defmethod update-object ((object physical-object) tstep)
  (update-rotation object tstep)
  (update-position object tstep)
  
  (when (object-warps-p object)
    (warp-object object
                 (list -1.05 -1.05)
                 (list 1.05 1.05))))


(defmethod update-object :after ((object projectile) tstep)
  (spend-lifetime object tstep))


(defun check-collision (object1 object2)
  "Check if two objects collide"
  (let ((x (+ (object-scale object1)
              (object-scale object2)))

        (d-2 (vector-norm-2 (vector-add (object-position object1)
                                        (vector-scale -1 (object-position object2))))))
    
    (< d-2 (* x x))))


(defun spend-lifetime (object tstep)
  "Reduce an object's lifetime, returning the remaining amount of lifetime"
  (setf (object-lifetime object)
        (max 0 (- (object-lifetime object) tstep))))


(defun split-rock (rock)
  "Split a rock into two"
  (with-slots (position heading scale) rock
    (when (> scale *rock-minimum-split-scale*)
      (flet ((make-rock ()
               (make-instance 'rock
                              :position position
                              :heading (vector-scale (vector-norm heading)
                                                     (vector-unit (random 360)))
                              :rotation 0
                              :scale (* scale (+ 0.7
                                                 (random 0.3)))
                              :num-vertices (+ 7
                                               (* 2 (random 2)))
                              :rotation-speed (- (random 360)
                                                 180))))
    
        (list (make-rock) (make-rock))))))


(defun control-ship (game tstep)
  "Apply player actions to ship"
  (with-slots (ship projectile player) game
    
    (with-slots (turning thrusting shooting) player
      (when turning (turn-ship ship tstep turning))
      (when thrusting (thrust-ship ship tstep))
      (when (and shooting (null projectile))
        (setf projectile (ship-projectile ship))))))


(defun update-objects (game tstep)
  "Update all the game's objects"
  (with-slots (ship projectile rocks) game

    (update-object ship tstep)
    (loop for rock in rocks do (update-object rock tstep))
    (when projectile
      (update-object projectile tstep)
      (when (zerop (object-lifetime projectile))
        (setf projectile nil)))))


(defun ship-collisions (game)
  "Handle ship collisions, if any"
  (with-slots (ship rocks) game
    (when (and (object-alive-p ship)
               (some (lambda (rock)
                       (check-collision ship rock))
                     rocks))
      
      (format t "Game Over!~%")
      (setf (object-alive-p ship)
            nil))))


(defun projectile-rock-collisions (game)
  "Handle ship collisions, if any"
  (with-slots (rocks projectile) game
    (when projectile
      
      (let ((rock (find-if (lambda (rock)
                             (check-collision projectile rock))
                           rocks)))
        (when rock
          (setf projectile nil)
          (setf rocks (append (split-rock rock) (remove rock rocks))))))))


(defun collision-detect (game)
  "Find all relevant collisions in game"
  (ship-collisions game)
  (projectile-rock-collisions game))



(defun update-game (game tstep)
  "Step the game forward `tstep` units of time"
  (control-ship game tstep)
  (update-objects game tstep)
  (collision-detect game))

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
(quote (ql:quickload :cl-game))
