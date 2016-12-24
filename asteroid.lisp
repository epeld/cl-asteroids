
(in-package :asteroids)


(defclass ship ()
  ((position :type list
             :accessor ship-position
             :initform '(0 0)
             :documentation "The ship's position")
   (rotation :type number
             :accessor ship-rotation
             :initform 0
             :documentation "The ship's rotation"))
  (:documentation "The entity representing the player's ship"))

(defclass asteroids-game ()
  ((ship :type ship
         :accessor asteroids-ship
         :initarg :ship
         :documentation "The ship belonging to the player"))
  (:documentation "A game of asteroids"))


(defun render-ship (ship)
  "Render a ship"
  (destructuring-bind (x y) (ship-position ship)
    ;; Setup
    (gl:color 1.0 1.0 1.0)
    (gl:translate x y 0)
    (gl:rotate (ship-rotation ship) 0 0 1)

    ;; Draw
    (gl:begin :line-loop)
    (gl:vertex 0 -1)
    (gl:vertex (sqrt 2) (sqrt 2))
    (gl:vertex -(sqrt 2) (sqrt 2))
    (gl:end)))


(defun render-game (game)
  "Render a game"
  (gl:color 0 0 0))
  (gl:clear :color-buffer)
  (with-slots (ship) game
    (render-ship ship)))


(defun game-loop (&key (game (new-game)))
  "Enter a game loop running the specified game"
  (window:event-loop :title "Asteroids"))


(defun new-game ()
  "Create a new game"
  (make-instance 'asteroids-game
                 :ship (make-instance 'ship)))
