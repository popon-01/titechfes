(in-package titechfes)

;;;player
(define-class player ()
  (x 0 get-x)
  (y 0 get-y)
  (width 24)
  (height 32)
  (image (error "image not initialized"))
  (vx 0)
  (vy 0)
  (jump-ok nil)
  (jump-accel 0)
  (jump-cool 0)
  (dash-ok nil)
  (dash-aceel 0)
  (dash-cool 0))

(defun get-map-state (x y)
  (if (and (<= 0 x)
	   (<= 0 y)
	   (< x 320)
	   (< y 320))
      (aref *map* (truncate y 32) (truncate x 32))
      0))

(defmethod update-object ((p player))
  (with-slots (x y width height vx vy 
		 jump-ok jump-accel jump-cool
		 dash-ok dash-aceel dash-cool) p
    (with-slots (right left jump down shot dash) *keystate*
      (let ((nx x) (ny y))
	(setf vx 0)
	(whens
	  (left (decf vx 5))
	  (right (incf vx 5))
	  ((and jump jump-ok (zerop jump-cool))
	   (setf jump-ok nil) (setf jump-accel 10))
	  ((and dash dash-ok (zerop dash-cool))
	   (setf dash-ok nil) (setf dash-aceel 10)))
	(incf vy 2)
	(when (> vy 10) (setf vy 10))
	(when (< 0 jump-accel) (setf vy -8))
	(decf jump-accel)
	(whens
	  ((> dash-aceel 0) (decf dash-aceel))
	  ((> jump-cool 0) (decf jump-cool))
	  ((> dash-cool 0) (decf dash-cool)))
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
	  (when (not jump-ok)
	    (setf jump-ok t)
	    (setf jump-cool 10)))
	 ;;bottom2
	 ((equal 1 (get-map-state (+ nx 8) 
				  (+ ny (truncate height 2))))
	  (setf ny (- (* (truncate (+ ny (truncate height 2)) 32)
			 32)
		      (truncate height 2)))
	  (when (not jump-ok)
	    (setf jump-ok t)
	    (setf jump-cool 10))))
	(incf nx vx)
	(whens
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
