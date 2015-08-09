(in-package titechfes)
;------------------gameobject------------------
(defgeneric update-all (objects))
(defgeneric draw-all (objects))
(defgeneric update-object (object))
(defgeneric draw-object (object))


(defmacro init-object (object &rest group)
  `(progn
     (with-slots (width height image) ,object
       (setf width (sdl:width image))
       (setf height (sdl:height image)))
     ,@(loop for g in group collect `(push ,object (container ,g)))))

(defclass obj-group ()
  ((container :initarg :container :initform nil :accessor container)))

(defmethod update-all ((group obj-group))
  (dolist (obj (container group))
    (update-object obj)))

(defmethod draw-all((group obj-group))
  (dolist (obj (container group))
    (draw-object obj)))

;;;wall
(defclass map-group ()
  ((container :initarg :container :initform nil :accessor container)))

(define-class wall ()
  (x 0) ;; why don't have accessor get-x?
  (y 0)
  (width 32)
  (height 32)
  (image (get-image :wall)))

(defmethod update-object ((w wall)))

(defmethod draw-object ((w wall))
  (with-slots (x y width height image) w
    (sdl:draw-surface-at-* image
			   (- x (/ width 2))
			   (- y (/ height 2)))))




;------------------collide------------------
(defgeneric rect-collide (a b))

(defmethod rect-collide (a b)
  (and (< (- (x a) (/ (width a) 2))
	      (+ (x b) (/ (width b) 2)))
       (< (- (x b) (/ (width b) 2))
	  (+ (x a) (/ (width a) 2)))
       (< (- (y a) (/ (height a) 2))
	      (+ (y b) (/ (height b) 2)))
       (< (- (y b) (/ (height b) 2))
	  (+ (y a) (/ (height a) 2)))))

