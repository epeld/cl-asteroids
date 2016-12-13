
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


(defun object-color (object)
  "Return an object's primary color"
  ;; TODO
  (list 1 1 1))


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

    (apply #'gl:color (object-color object))
    
    ;; TODO parameterize based on object type?
    (glut:wire-cube 0.5)))


;;
;;  Camera
;; 


(defun scene-camera (scene)
  "Get the camera used by the scene"
  :todo)


(defun use-camera-projection (camera)
  "Adapt the projection transform to reflect the given camera's view"
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 60 (/ 640 320) 1 20) ; TODO figure out what we want here
  ) 

;;
;;  Scene
;;

(defun scene-objects (scene)
  "Get the objects within a scene"
  (getf scene :items))


(defun render (scene)
  "Render a full scene"

  (use-camera-projection (scene-camera scene))
  
  (gl:matrix-mode :modelview)
  (loop for obj in (scene-objects scene)
     do (render-object obj)))
