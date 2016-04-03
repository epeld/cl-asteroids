

(defmacro with-option-to-move-on (&body forms)
  `(restart-case 
      (progn ,@forms)
    (move-on ()
      :report "Move on, ignoring the error")))





(defclass klass1 () ())

(defvar a (make-instance 'klass1))

(defclass klass2 (klass1) ())

(defvar b (make-instance 'klass2))

(defgeneric my-method (klass1))

(defmethod my-method ((k klass1))
  (error "FOOP"))

(defmethod my-method :around ((k klass2))
  (with-option-to-move-on 
    (call-next-method)))

(my-method b)


