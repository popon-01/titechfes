(in-package titechfes)

;;;player
(define-class player (gameobject)
  (image (get-image :player-l))
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
  (dir-right t)
  (muteki nil)
  (muteki-count 0))


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
		   shot-cool while-dash
		   muteki muteki-count) ply
    (whens
      ((> jump-cool 0) (decf jump-cool))
      ((> dash-cool 0) (decf dash-cool))
      ((> shot-cool 0) (decf shot-cool)))
    (when (and while-dash (<= -5 vvx) (<= vvx 5))
      (setf while-dash nil dash-cool 10))
    (if (and muteki (zerop muteki-count))
	(setf muteki nil)
	(decf muteki-count))))

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
  
