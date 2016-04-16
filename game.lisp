;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;; hello.lisp --- Lisp version of hello.c (Red Book examples)
;;;
;;; Original C version contains the following copyright notice:
;;;   Copyright (c) 1993-1997, Silicon Graphics, Inc.
;;;   ALL RIGHTS RESERVED

;;; This is a simple, introductory OpenGL program.

;;; Declare initial window size, position, and display mode (single
;;; buffer and RGBA).  Open window with "hello" in its title bar.
;;; Call initialization routines.  Register callback function to
;;; display graphics.  Enter main loop and process events.

(in-package :asteroids)

(defvar *window* nil)

(defmacro with-option-to-move-on (&body forms)
  `(restart-case 
      (progn ,@forms)
    (move-on ()
      :report "Move on, ignoring the error")))

(defclass game-object ()
  ((x :accessor x-position
      :initform 0
      :initarg :x)
   
   (y :accessor y-position
      :initform 0
      :initarg :y)
   
   (angle :accessor angle
          :initform 0)
   
   (angular-velocity :accessor angular-velocity
                      :initform 0)
   
   (x-heading :accessor x-heading
              :initform 0)

   (y-heading :accessor y-heading
              :initform 0)))


(defgeneric draw-object (game-object))


(defvar test-object (make-instance 'game-object))


(defclass game-window (glut:window)
  (asteroids spaceship)
  (:default-initargs :pos-x 100 :pos-y 100 :width 500 :height 500
                     :tick-interval 50
                     :mode '(:single :rgb) :title "Hiyoo!"))


(defmacro with-temporary-matrix (&body body)
  `(unwind-protect 
        (progn (gl:push-matrix)
               ,@body)               
     (gl:pop-matrix)))


(defmacro with-periodic-rotation (period &body body)
  (let ((elapsed (gensym)))
    `(let ((,elapsed (glut:get :elapsed-time)))
         (rotation ,elapsed ,period)
         ,@body)))


(defmethod glut:display-window :before ((w game-window))
  ;; Select clearing color.
  (gl:clear-color 0 0 0 0)
  ;; Initialize viewing values.
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 1 0 1 -1 1))


(defun asteroid-vertices (&optional (n 5))
  (loop for i upto n
     collect (let* ((rads (/ (* 2 pi i) n))
                    (radius (+ 0.7 (* 0.3 (cos (+ (/ pi 7) (* 3 rads)))))))
               (list (* radius (cos rads)) 
                     (* radius (sin rads))
                     0))))


(defun circle-vertices (&optional (n 5))
  (loop for i upto n
       collect (let ((rads (/ (* 2 pi i) n)))
                 (list (cos rads) (sin rads) 0))))


(defmacro polygon (vertices)
  `(gl:with-primitive :polygon
     ,@(loop for vertex in vertices
          collect `(gl:vertex ,@vertex))))


(defmacro asteroid (&optional (n 5))
  `(polygon ,(asteroid-vertices n)))


(defun rotation (elapsed period)
  (gl:rotate (* (/ 360 period) (mod elapsed period)) 0 0 1))


(defmethod glut:display ((w game-window))
  (with-option-to-move-on
    (gl:clear :color-buffer)
    (gl:color 0.3 0.4 1)
    
    (gl:matrix-mode :modelview)
    (with-temporary-matrix
      (gl:translate 0.5 0.5 0)
      (with-periodic-rotation 10000
        (gl:scale 0.5 0.5 1)
        (asteroid)))
    (gl:flush)))


(defmethod glut:tick ((w game-window))
;  (format t "elapsed ~A~%" (glut:get :elapsed-time))
  (glut:post-redisplay))


(defun rb-hello ()
  (let ((win (make-instance 'game-window)))
    (setf *window* win)
    (setf (glut:title win) "Hello")
    (glut:display-window win)))


(quote (gui-thread:with-body-in-gui-thread 
         (rb-hello)))

