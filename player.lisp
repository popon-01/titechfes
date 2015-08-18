(in-package titechfes)

;;;player
(define-class player (gameobject)
  (image-r (get-image :player-r))
  (image-l (get-image :player-l))
  (max-hp 200)
  (hp 200)
  (vx 0)
  (vvx 0)
  (vy 0)
  (in-air t)
  (jump-cool 0)
  (dash-ok t)
  (while-dash nil)
  (dash-cool 0)
  (shot-name "Knife")
  (shot-func #'shot-boomerang)
  (shot-cool 0)
  (dir-right t))


(defun player-keyevents (ply game)
  (with-slots (vx vy vvx in-air jump-cool
		  dash-ok while-dash dash-cool
		  shot-func shot-cool
		  dir-right alive) ply
      (with-slots (right left jump down shot dash) (keystate game)
	(whens
	  ((and shot (zerop shot-cool))
	   (funcall shot-func ply game))
	  ((and left (not while-dash)) 
	   (decf vx 3) (setf dir-right nil))
	  ((and right (not while-dash))
	   (incf vx 3) (setf dir-right t))
	  ((and jump (not in-air) (zerop jump-cool))
	   (setf in-air t
		 vy -16))
	  ((and dash dash-ok (zerop dash-cool))
	   (setf while-dash t
		 dash-ok nil
		 vvx (if dir-right 20 -20)))
	  (down 
	   (setf alive nil))))))

(defun player-accelerarion (ply)
  (with-slots  (vx vy vvx while-dash) ply
    (if while-dash (setf vy 0) (incf vy *gravity*))
    (when (> vy 10) (setf vy 10))
    (whens ((< vvx 0) (incf vvx 2))
	   ((> vvx 0) (decf vvx 2)))
    (cond ((> vvx 10) (incf vx 10))
	  ((< vvx -10) (incf vx -10))
	  (t (incf vx vvx)))))

(defun player-flag-update (ply)
  (with-slots (vvx jump-cool dash-cool 
		   shot-cool while-dash) ply
    (whens
      ((> jump-cool 0) (decf jump-cool))
      ((> dash-cool 0) (decf dash-cool))
      ((> shot-cool 0) (decf shot-cool)))
    (when (and while-dash (<= -5 vvx) (<= vvx 5))
      (setf while-dash nil dash-cool 10))))

(defmethod update-object ((ply player) game)
  (with-slots (vx image image-r image-l dir-right) ply
    (setf vx  0
	  image (if dir-right image-r image-l))
    (player-keyevents ply game)
    ;;(alive-detect)
    (player-accelerarion ply)
    (player-flag-update ply)))

(defmethod draw-object :before ((ply player) game)
  (incf (get-x ply) (vx ply))
  (incf (get-y ply) (vy ply)))
  
(definteract-method collide (ply player) (chip wall)
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
				      :x (get-left (get-x ply)
						   (width ply))
				      :y (get-top (+ (get-y ply) vy)
						  (height ply))
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
