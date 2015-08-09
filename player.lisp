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
  (in-air nil)
  (jump-cool 0)
  (dash-ok t)
  (while-dash nil)
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
		 in-air jump-cool
		 dash-ok while-dash dash-cool
		 dir-right) p
    (with-slots (right left jump down shot dash) *keystate*
      (let ((nx x) (ny y))
	(setf vx 0)
	(whens
	  ((and left (not while-dash)) 
	   (decf vx 5) (setf dir-right nil))
	  ((and right (not while-dash))
	   (incf vx 5) (setf dir-right t))
	  ((and jump in-air (zerop jump-cool))
	   (setf in-air nil) (setf vy -16))
	  ((and dash dash-ok (zerop dash-cool))
	   (setf while-dash t)
	   (setf dash-ok nil)
	   (if dir-right
	       (setf vvx 20)
	       (setf vvx -20))))
	(if dir-right
	    (setf image *playerimage-r*)
	    (setf image *playerimage-l*))
	(when (and while-dash (<= -5 vvx) (<= vvx 5))
	  (setf while-dash nil dash-cool 10))
	(incf vy)
	(whens ((< vvx 0) (incf vvx 2))
	       ((> vvx 0) (decf vvx 2))
	       ((> vy 10) (setf vy 10)))
	(whens
	  ((> jump-cool 0) (decf jump-cool))
	  ((> dash-cool 0) (decf dash-cool)))
	;;move vertical
	(cond (while-dash (incf ny 0)) 
	      ((< vy -10) (incf ny -10))
	      (t (incf ny vy)))
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
	  (whens ((not in-air)
		  (setf in-air t) (setf jump-cool 10))
		 ((and (not dash-ok) (not while-dash))
		  (setf dash-ok t))))
	 ;;bottom2
	 ((equal 1 (get-map-state (+ nx 8) 
				  (+ ny (truncate height 2))))
	  (setf ny (- (* (truncate (+ ny (truncate height 2)) 32)
			 32)
		      (truncate height 2)))
	  (whens ((not in-air)
		  (setf in-air t) (setf jump-cool 10))
		 ((and (not dash-ok) (not while-dash))
		  (setf dash-ok t)))))
	;;move horizontal
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
