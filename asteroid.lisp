
(in-package :asteroids)


(defclass asteroid (game-object)
  ((vertices :accessor vertices :initform 5 :initarg :vertices)
   (seed :accessor seed :initform (random 255) :initarg :seed)))


(defun random-between (lower upper)
  (+ lower (random (- upper lower))))


(defun random-vertex-count ()
  (random-between 3 8))


(defun unit-circle (&optional (rads (* (/ pi 180) (random 360))))
  (values (sin rads) (cos rads)))


(defun randomize-heading (object)
  (let ((speed (* 0.1 (random-between 1 3))))
    (multiple-value-bind (hx hy) (unit-circle)
      
      (setf (x-heading object) (* speed hx))
      (setf (y-heading object) (* speed hy))
      (setf (angular-velocity object) (random-between -30 30))))
  object)


(defun randomize-position (object &optional (r 0.5))
  "Randomize the object's position in the circle band between 0.5r and r"
  (let ((r (* 0.5 r (random-between 1 2))))
    (multiple-value-bind (x y) (unit-circle)
      
      (setf (x-position object) (* r x))
      (setf (y-position object) (* r y))))
  object)


(defun random-asteroid ()
  (let ((object (make-instance 'asteroid
		 :vertices (random-vertex-count))))
    
    (randomize-position object)
    (randomize-heading object)))


(defun random-color (seed)
  (loop for i upto 3
       collect (/ seed 255)))


(defmethod draw-object ((object asteroid))
  (apply #'gl:color (random-color (seed object)))
  (polygon (circle-vertices (vertices object))))

(defvar *asteroids* (loop for i upto 5 collect (random-asteroid)))
