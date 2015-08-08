(in-package titechfes)

;;;player
(define-class player ()
  (x 0 get-x)
  (y 0 get-y)
  (width 24)
  (height 32)
  (image (error "image not initialized"))
  (vx 0)
  (vvx 0)
  (vy 0)
  (jump-ok nil)
  (jump-cool 0)
  (dash-ok t)
  (dash-cool 0)
  (dir-right t))

(defun get-map-state (x y)
  (if (and (<= 0 x)
	   (<= 0 y)
	   (< x 320)
	   (< y 320))
      (aref *map* (truncate y 32) (truncate x 32))
      0))

(defmethod update-object ((p player))
  (with-slots (x y width height 
		 vx vvx vy image
		 jump-ok jump-cool
		 dash-ok dash-cool
		 dir-right) p
    (with-slots (right left jump down shot dash) *keystate*
      (let ((nx x) (ny y))
	(setf vx 0)
	(whens
	  ((and left dash-ok) 
	   (decf vx 5) (setf dir-right nil))
	  ((and right dash-ok)
	   (incf vx 5) (setf dir-right t))
	  ((and jump jump-ok (zerop jump-cool))
	   (setf jump-ok nil) (setf vy -16))
	  ((and dash dash-ok (zerop dash-cool))
	   (setf dash-ok nil)
	   (if left
	       (setf vvx -20)
	       (setf vvx 20))))
	(if dir-right
	    (setf image *playerimage-r*)
	    (setf image *playerimage-l*))
	(incf vy)
	(whens ((< vvx 0) (incf vvx 2))
	       ((> vvx 0) (decf vvx 2))
	       ((> vy 10) (setf vy 10)))
	(whens
	  ((> jump-cool 0) (decf jump-cool))
	  ((> dash-cool 0) (decf dash-cool)))
	;;move vertical
	(if (< vy -10)
	    (incf ny -10)
	    (incf ny vy))
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
	  (whens ((not jump-ok)
		  (setf jump-ok t) (setf jump-cool 10))
		 ((and (not dash-ok) (<= -5 vvx) (<= vvx 5))
		  (setf dash-ok t) (setf dash-cool 10))))
	 ;;bottom2
	 ((equal 1 (get-map-state (+ nx 8) 
				  (+ ny (truncate height 2))))
	  (setf ny (- (* (truncate (+ ny (truncate height 2)) 32)
			 32)
		      (truncate height 2)))
	  (whens ((not jump-ok)
		  (setf jump-ok t) (setf jump-cool 10))
		 ((and (not dash-ok) (<= -5 vvx) (<= vvx -5))
		  (setf dash-ok t) (setf dash-cool 10)))))
	;;move holizontal
	(cond ((> vvx 10) (incf vx 10))
	      ((< vvx -10) (incf vx -10))
	      (t (incf vx vvx)))
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
