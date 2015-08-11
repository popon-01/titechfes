(in-package titechfes)

;;;player
(define-class player (gameobject)
  (image-r (get-image :player-r))
  (image-l (get-image :player-l))
  (nx 0)
  (ny 0)
  (vx 0)
  (vvx 0)
  (vy 0)
  (in-air nil)
  (jump-cool 0)
  (dash-ok t)
  (while-dash nil)
  (dash-cool 0)
  (dir-right t))


(defmethod collide ((p player) (chip wall))
  (with-slots (nx ny width height vy
		  in-air jump-cool
		  dash-ok while-dash) p 
    (whens
     ;;up
     ((and (< (- (get-x chip) 16) nx)
	   (< nx (+ (get-x chip) 16))
	   (< (- (get-y chip) 16) (- ny (truncate height 2)))
	   (< (- ny (truncate height 2)) (+ (get-y chip) 16)))
      (setf ny (+ (get-y chip) 32))
      (setf vy 0))
     ;;bottom1
     ((and (< (- (get-x chip) 16) (- nx 12)) 
	   (< (- nx 12) (+ (get-x chip) 16))
	   (< (- (get-y chip) 16) (+ ny (truncate height 2)))
	   (< (+ ny (truncate height 2)) (+ (get-y chip) 16)))
      (setf ny (- (get-y chip) 32))
      (whens ((not in-air)
	      (setf in-air t) (setf jump-cool 10))
	     ((and (not dash-ok) (not while-dash))
	      (setf dash-ok t))))
     ;;bottom2
     ((and (< (- (get-x chip) 16) (+ nx 12))
	   (< (+ nx 12) (+ (get-x chip) 16))
	   (< (- (get-y chip) 16) (+ ny (truncate height 2)))
	   (< (+ ny (truncate height 2)) (+ (get-y chip) 16)))
      (setf ny (- (get-y chip) 32))
      (whens ((not in-air)
	      (setf in-air t
		    jump-cool 10))
	     ((and (not dash-ok) (not while-dash))
	      (setf dash-ok t))))
     ;;left1
     ((and (< (- (get-x chip) 16) (+ nx (truncate width 2)))
	   (< (+ nx (truncate width 2)) (+ (get-x chip) 16))
	   (< (- (get-y chip) 16) (- ny 10))
	   (< (- ny 10) (+ (get-y chip) 16)))
      (setf nx (- (get-x chip) 28)))
     ;;left2
     ((and (< (- (get-x chip) 16) (+ nx (truncate width 2)))
	   (< (+ nx (truncate width 2)) (+ (get-x chip) 16))
	   (< (- (get-y chip) 16) (+ ny 10))
	   (< (+ ny 10) (+ (get-y chip) 16)))
      (setf nx (- (get-x chip) 28)))
     ;;right1
     ((and (< (- (get-x chip) 16) (- nx (truncate width 2)))
	   (< (- nx (truncate width 2)) (+ (get-x chip) 16))
	   (< (- (get-y chip) 16) (- ny 10))
	   (< (- ny 10) (+ (get-y chip) 16)))
      (setf nx (+ (get-x chip) 28)))
     ;;right2
     ((and (< (- (get-x chip) 16) (- nx (truncate width 2)))
	   (< (- nx (truncate width 2)) (+ (get-x chip) 16))
	   (< (- (get-y chip) 16) (+ ny 10))
	   (< (+ ny 10) (+ (get-y chip) 16)))
      (setf nx (+ (get-x chip) 28))))))

(defmethod update-object ((p player) game)
  (with-slots (x y nx ny 
		 width height 
		 vx vvx vy
		 image image-r image-l
		 in-air jump-cool
		 dash-ok while-dash dash-cool
		 dir-right) p
    (with-slots (right left jump down shot dash) (keystate game)
      (setf vx 0
	    image (if dir-right image-r image-l)
	    nx x
	    ny y)
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
      (cond ((> vvx 10) (incf vx 10))
	    ((< vvx -10) (incf vx -10))
	    (t (incf vx vvx)))
      (incf nx vx)
      (dolist (chip (mapchips game))
	(collide p chip))
      (setf x nx
	    y ny))))
  
