
(in-package :asteroids)

(defclass integrator ()
  ((last-update :accessor last-update :initarg :last-update)))


(defgeneric get-current-time (integrator))


(defgeneric integrate (integrator))


(defmethod integrate :after ((int integrator))
  (setf (last-update int) (get-current-time int)))


;(setf (angular-velocity test-object) 10)
;(setf (y-position test-object) 0.2)
;(setf (y-heading test-object) 0.2)


(defun compute-delta (integrator &optional (time (get-current-time integrator)))
  (if (slot-boundp integrator 'last-update)
      (- time (last-update integrator))
      0))

