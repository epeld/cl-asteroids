
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
                 :initform (make-instance 'player-state)))
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
                  :right))))

    (:keyup
     (setf (player-turning (game-player game))
           nil))))

;;
;;  Rendering
;; 
(defun render-ship (ship)
  "Render a ship"
  (the ship ship)
  (with-slots (position rotation) ship
    (let ((x (car position))
          (y (cadr position)))

      (gl:push-matrix)
      (unwind-protect
           (progn
             ;; Setup
             (gl:color 1.0 1.0 1.0)
             (gl:translate x y 0)
             (gl:scale 0.5 0.5 0)
             (gl:rotate rotation 0 0 -1)

             ;; Draw
             (gl:begin :line-loop)
             (gl:vertex 1 0)
             (gl:vertex (cos (* 2 (/ pi 3))) (sin (* 2 (/ pi 3))))
             (gl:vertex (cos (* -2 (/ pi 3))) (sin (* -2 (/ pi 3))))
             (gl:end)

             (gl:begin :points)
             (gl:vertex 0 0)
             (gl:end)

             ;;
             ;; For debugging:
             ;;
             #|

      
             (gl:begin :line-loop)
             (loop for i upto 20
             do
             (let ((rad (/ (* 2 pi i)
             20)))
             (gl:vertex (cos rad) (sin rad))))
             (gl:end)
             |#

             )
        (gl:pop-matrix)))))


(defun render-game (game)
  "Render a game"
  (gl:color 0 0 0)
  (gl:clear :color-buffer)
  (with-slots (ship) game
    (render-ship ship))
  (glut:swap-buffers))


;;
;; Update logic
(defun update-game (game tstep)
  "Step the game forward `tstep` units of time"
  (let ((ship (asteroids-ship game)))
    (with-slots (turning) (game-player game)
      (case turning
        (:left
         (setf (ship-rotation ship)
               (- (ship-rotation ship)
                  (* 2 tstep pi))))

        (:right
         (setf (ship-rotation ship)
               (+ (ship-rotation ship)
                  (* 2 tstep pi))))))

    ;; TODO move the ship forward here
    (with-slots (thrusting) (game-player game)
      :todo)))

;;
;;  Top Level API
;; 
(defun game-loop (&key (game (new-game)))
  "Enter a game loop running the specified game"
  (window:event-loop :title "Asteroids"
                     :event-callback (lambda (event)
                                       (notify-event game event))
                     :renderer (lambda ()
                                 (update-game game 0.3)
                                 (render-game game))))


;; TODO move rendering into asteroid-rendering.lisp
;; TODO move event processing into asteroid-events.lisp

(quote (game-loop))
