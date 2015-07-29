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


;;;player
(define-class player ()
  (x 0)
  (y 0)
  (width 24)
  (height 32)
  (image (error "image not initialized"))
  (vx 3)
  (vy 5)
  (jump-ok nil)
  (jump-accel 0))

(defun get-map-state (x y)
  (if (and (<= 0 x)
	   (<= 0 y)
	   (< x 320)
	   (< y 320))
      (aref *map* (truncate y 32) (truncate x 32))
      0))

(defmethod update-object ((p player))
  (with-slots (x y width height vx vy 
		 jump-ok jump-accel) p
    (with-slots (right left up down spdup) *keystate*
      (let ((nx x) (ny y))
	(whens
	  ((and up jump-ok)
	   (setf jump-ok nil) (setf jump-accel 10)))
	(incf vy 2)
	(when (> vy 10) (setf vy 10))
	(when (< 0 jump-accel) (setf vy -8))
	(decf jump-accel 1)
	(incf ny vy)
	(whens 
	 ;;up
	 ((equal 1 (get-map-state nx (- ny (truncate height 2))))
	  (setf ny (+ (* (+ (truncate (- ny (truncate height 2)) 32) 1)
			 32)
		      (truncate height 2)))
	  (setf vy 0))
	 ;;bottom1
	 ((equal 1 (get-map-state (- nx 8) 
				  (+ ny (truncate height 2))))
	  (setf ny (- (* (truncate (+ ny (truncate height 2)) 32)
			 32)
		      (truncate height 2)))
	  (setf jump-ok t))
	 ;;bottom2
	 ((equal 1 (get-map-state (+ nx 8) 
				  (+ ny (truncate height 2))))
	  (setf ny (- (* (truncate (+ ny (truncate height 2)) 32)
			 32)
		      (truncate height 2)))
	  (setf jump-ok t)))
	(whens
	  (right (incf nx vx))
	  (left  (decf nx vx))
	  ;;left1
	  ((equal 1 (get-map-state (- nx (truncate width 2)) 
				   (- ny 10)))
	   (setf nx (+ (* (+ (truncate (- nx (truncate width 2))32) 1)
			  32)
		       (truncate width 2))))
	  ;;left2
	  ((equal 1 (get-map-state (- nx (truncate width 2)) 
				   (+ ny 10)))
	   (setf nx (+ (* (+ (truncate  (- nx (truncate width 2)) 32) 1)
			  32)
		       (truncate width 2))))
	  ;;right1
	  ((equal 1 (get-map-state (+ nx (truncate width 2)) 
				   (- ny 10)))
	   (setf nx (- (* (truncate (+ nx (truncate width 2)) 32) 
			  32)
		       (truncate width 2))))
	  ;;right2
	  ((equal 1 (get-map-state (+ nx (truncate width 2)) 
				   (+ ny 10)))
	   (setf nx (- (* (truncate  (+ nx (truncate width 2)) 32) 
			  32)
		       (truncate width 2)))))
	(setf x nx)
	(setf y ny)))))
  
(defmethod draw-object ((p player))
    (with-slots (x y width height image) p
      (sdl:draw-surface-at-* image
			     (- x (/ width 2))
			     (- y (/ height 2)))))

;;;wall
(defclass map-group ()
  ((container :initarg :container :initform nil :accessor container)))

(define-class wall ()
  (x 0)
  (y 0)
  (width 32)
  (height 32)
  (image (error "image not initialized")))

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

