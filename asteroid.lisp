
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


(defun new-game ()
  "Create a new game"
  (make-instance 'asteroids-game
                 :ship (make-instance 'ship)))
