(in-package titechfes)

;;;player
(define-class player (gameobject)
  (image-r (get-image :player-r))
  (image-l (get-image :player-l))
  (vx 0)
  (vvx 0)
  (vy 0)
  (in-air t)
  (jump-cool 0)
  (dash-ok t)
  (while-dash nil)
  (dash-cool 0)
  (dir-right t))

(defun get-left (px w)
  (- px (truncate w 2)))
(defun get-top (py h)
  (- py (truncate h 2)))

(defun rect-collision-judge (rec1 rec2)
  (and (< (sdl:x rec1) (sdl:x2 rec2))
       (< (sdl:x rec2) (sdl:x2 rec1))
       (< (sdl:y rec1) (sdl:y2 rec2))
       (< (sdl:y rec2) (sdl:y2 rec1))))


(defmethod collide ((ply player) (chip wall))
  (with-slots (vx vy
		  in-air jump-cool
		  dash-ok while-dash) ply
    (sdl:with-rectangle (chip-rec (sdl:rectangle 
				   :x (get-left (get-x chip) 
						(width chip)) 
				   :y (get-top (get-y chip)
					       (height chip))
				   :w (width chip)
				   :h (height chip)))
      ;;horizontal
      (sdl:with-rectangle (ply-x-rec (sdl:rectangle 
				      :x (get-left (+ (get-x ply) vx)
						   (width ply))
				      :y (get-top (get-y ply) 
						  (height ply))
				      :w (width ply)
				      :h (height ply)))
	(when (rect-collision-judge chip-rec ply-x-rec)
	  (if (plusp vx)
	      (setf vx (- (get-x chip) 
			  (+ (truncate (width chip) 2)
			     (truncate (width ply) 2))
			  (get-x ply)))
	      (setf vx (- (get-x chip)
			  (- (+ (truncate (width chip) 2)
				(truncate (width ply) 2))) 
			  (get-x ply))))))
      ;;vertical
      (sdl:with-rectangle (ply-y-rec (sdl:rectangle 
				      :x (- (get-x ply) 12)
				      :y (- (+ (get-y ply) vy) 16)
				      :w (width ply)
				      :h (height ply)))
	(when (rect-collision-judge chip-rec ply-y-rec)
	  (if (plusp vy)
	      (progn (setf vy (- (get-y chip)
				 (+ (truncate (height chip) 2)
				    (truncate (height ply) 2))
				 (get-y ply)))
		     (whens (in-air
			     (setf in-air nil
				   jump-cool 10))
			    ((and (not dash-ok) (not while-dash))
			     (setf dash-ok t))))
	      (setf vy (- (get-y chip)
			  (- (+ (truncate (height chip) 2)
				(truncate (height ply) 2)))
			  (get-y ply)))))))))

(defmethod update-object ((p player) game)
  (with-slots (x y width height 
		 vx vvx vy
		 image image-r image-l
		 in-air jump-cool
		 dash-ok while-dash dash-cool
		 dir-right alive) p
    (with-slots (right left jump down shot dash) (keystate game)
      (setf vx 0
	    image (if dir-right image-r image-l))
      ;;keyevents
      (whens
       ((and left (not while-dash)) 
	(decf vx 5) (setf dir-right nil))
       ((and right (not while-dash))
	(incf vx 5) (setf dir-right t))
       ((and jump (not in-air) (zerop jump-cool))
	(setf in-air t
	      vy -16))
       ((and dash dash-ok (zerop dash-cool))
	(setf while-dash t
	      dash-ok nil
	      vvx (if dir-right 20 -20)))
       (down (setf alive nil)))
      ;;alive-detect
      ;;acceleration
      (if while-dash (setf vy 0) (incf vy))
      (when (> vy 10) (setf vy 10))
      (whens ((< vvx 0) (incf vvx 2))
	     ((> vvx 0) (decf vvx 2)))
      (cond ((> vvx 10) (incf vx 10))
	    ((< vvx -10) (incf vx -10))
	    (t (incf vx vvx)))
      ;;flags
      (whens
       ((> jump-cool 0) (decf jump-cool))
       ((> dash-cool 0) (decf dash-cool)))
      (when (and while-dash (<= -5 vvx) (<= vvx 5))
	(setf while-dash nil dash-cool 10))
      ;;move
      (dolist (chip (mapchips game))
	(collide p chip))
      (incf x vx)
      (incf y vy))))
  
