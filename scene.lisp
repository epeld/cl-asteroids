
(in-package :scene)


;;
;;  Scene Objects
;; 

(defun object-position (object)
  "Return an object's position"
  ;; TODO
  (list 0 0 0))


(defun object-rotation (object)
  "Return an object's rotation as a ???"
  (list 0 1 0 0))


(defun use-object-coordinates (object)
  "Translate, rotate and scale coordinate space to use object coordinates"
  (let ((position (object-position object))
        (rotation (object-rotation object)))

    (apply #'gl:translate position)
    (apply #'gl:rotate rotation)))


(defun render-object (object)
  "Render an object"
  (gl:with-pushed-matrix
    (use-object-coordinates object)

    ;; TODO parameterize based on object type?
    (glut:wire-cube 0.5)))


;;
;; Scene
;;

(defun scene-objects (scene)
  "Get the objects within a scene"
  (getf scene :items))


(defun render (scene)
  "Render a full scene"
  (gl:matrix-mode :modelview)
  (loop for obj in (scene-objects scene)
     do (render-object obj)))
