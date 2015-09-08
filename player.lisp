(in-package titechfes)

;;;player
(define-class player (gamecharacter)
  (image (get-image :player-l))
  (image-r (get-image :player-r))
  (image-l (get-image :player-l))
  (max-hp 200)
  (hp 200)
  (velocity 3)
  (dx 0) (vx 0) (vvx 0)
  (dy 0) (vy 0)
  (in-air t)
  (jump-cool 0) (jump-cooltime 20)
  (jump-count 1) (max-jump 2)
  (jump-accel -16)
  (while-dash nil)
  (dash-cool 0) (dash-cooltime 20)
  (dash-count 1) (max-dash 2)
  (dash-accel 20)
  (shot-name "Knife")
  (shot-func #'shot-knife)
  (shot-cool 0)
  (dir-right t)
  (muteki nil)
  (muteki-count 0)
  (score 0)
  (bullet-list (vector 'knife nil nil))
  (bullet-i 0)
  (bullet-num 1))

(defun player-keyevents (ply game)
  (with-slots (vx vy vvx velocity 
		  jump-count jump-accel
		  jump-cool jump-cooltime
		  while-dash
		  dash-count dash-accel
		  dash-cool dash-cooltime
		  shot-func shot-cool
		  dir-right alive) ply
      (with-slots (right left jump down shot dash weapon) (keystate game)
	(whens
	  ((and (key-pressed-p shot) (zerop shot-cool))
	   (funcall shot-func ply game))
	  ((and (key-pressed-p left) (not while-dash))
	   (decf vx velocity) (setf dir-right nil))
	  ((and (key-pressed-p right) (not while-dash))
	   (incf vx velocity) (setf dir-right t))
	  ((and (key-down-p jump) (plusp jump-count) (zerop jump-cool))
	   (setf while-dash nil
		 jump-cool jump-cooltime
		 vy jump-accel)
	   (decf jump-count))
	  ((key-down-p weapon) (change-bullet ply))
	  ((and (key-down-p dash) (plusp dash-count) (zerop dash-cool))
	   (setf while-dash t
		 dash-cool dash-cooltime
		 vvx (if dir-right dash-accel (- dash-accel)))
	   (decf dash-count))))))

(defun player-accelerarion (ply)
  (with-slots  (dx dy vx vy vvx rvx rvy while-dash) ply
    (incf vy *gravity*)
    (when (and while-dash (plusp vy)) (setf vy 0)) 
    (when (> vy 10) (setf vy 10))
    (whens ((< vvx 0) (setf vvx (min (+ vvx 2) 0)))
	   ((> vvx 0) (setf vvx (max (- vvx 2) 0))))
    (cond ((> vvx 10) (incf vx 10))
	  ((< vvx -10) (incf vx -10))
	  (t (incf vx vvx)))
    (setf dx (+ vx rvx)
	  dy (+ vy rvy)
	  rvx 0 rvy 0)))

(defun player-flag-update (ply)
  (with-slots (vvx velocity
		   jump-cool shot-cool
		   dash-cool while-dash  
		   muteki muteki-count) ply
    (whens
      ((> jump-cool 0) (decf jump-cool))
      ((> dash-cool 0) (decf dash-cool))
      ((> shot-cool 0) (decf shot-cool)))
    (when (and (<= (- velocity) vvx) 
	       (<= vvx velocity))
      (setf while-dash nil))))

(defmethod update-object ((ply player) game)
  (call-next-method)
  (when (and (minusp (vy ply)) (zerop (dy ply)))
    (setf (vy ply) 0))
  (when (not (while-dash ply))
    (setf (in-air ply) (not (and (plusp (vy ply)) 
				 (zerop (dy ply))))))
  (if (in-air ply) (player-in-air ply) (player-landed ply))
  (with-slots (vx image image-r image-l dir-right) ply
    (setf vx  0
	  image (if dir-right image-r image-l))
    (player-keyevents ply game)
    (player-accelerarion ply)
    (player-flag-update ply)))


(defun player-in-air (ply)
  (setf (dash-cooltime ply) 20)
  (when (equal (jump-count ply) (max-jump ply))
    (decf (jump-count ply))))

(defun player-landed (ply)
  (setf (jump-count ply) (max-jump ply)
	(dash-count ply) (max-dash ply)
	(dash-cooltime ply) 40))

(defun change-bullet (player)
  (setf (bullet-i player)
	(mod (1+ (bullet-i player))
	     (bullet-num player)))
  (update-bullet player))

(defmethod update-bullet ((player player))
  (setf (shot-name player)
	(format nil "~@(~a~)" (elt (bullet-list player)
				   (bullet-i player)))
	(shot-func player)
	(symbol-function 
	 (symbolicate 'shot- 
		      (elt (bullet-list player)
			   (bullet-i player))))))

(defmethod get-bullet (bsym (player player))
  (with-accessors ((blis bullet-list) (bi bullet-i)
		   (bnum bullet-num)) player
    (unless (position bsym blis)
      (if (< bnum 3)
	  (letrec ((i 0))
	    (if (elt blis i)
		(rec (1+ i))
		(progn (setf (elt blis i) bsym
			     bnum (1+ bnum)
			     bi i)
		       (update-bullet player))))
	  (progn (setf (elt blis bi) bsym)
		 (update-bullet player))))))
