(in-package titechfes)


(define-class enemy (gamecharacter)
  (dx 0) (vx 0)
  (dy 0) (vy 0)
  (hp 100)
  (atk 0)
  (dir-right t)
  (muteki nil)
  (muteki-count 0))

(define-class land-enemy (enemy))

(defmethod update-object ((enem land-enemy) game)
  (call-next-method)
  (incf (vy enem) *gravity*)
  (when (> (vy enem) 10) (setf (vy enem) 10)))

(defmethod update-object :after ((enem enemy) game)
  (setf (dx enem) (floor (vx enem)) 
	(dy enem) (floor (vy enem))))

(define-class enemy-bullet (bullet)
  (vx 0)
  (vy 0)
  (atk 0))

(defmacro change-enemy-state (enem states &body body)
  `(progn 
     ,@(loop for b in body
	  collect `(setf (,(car b) enem) 
	   (cond ,@(mapcar (lambda (state val) 
			     (list `(equal (state ,enem) ,state) val))
			   states (cdr b)))))))

;;aomura

(define-class aomura (land-enemy)
  (hp 200)
  (atk 20)
  (vx 2)
  (image (get-image :enemy-l))
  (image-r (get-image :enemy-r))
  (image-l (get-image :enemy-l))
  (turn-routine 40)
  (jump-routine 75))

(defmethod update-object ((enem aomura) game)
  (call-next-method)
  (with-slots (image-r image-l 
		       turn-routine jump-routine) enem
    (setf (image enem) (if (plusp (vx enem)) image-r image-l))
    (whens ((zerop turn-routine)
	    (setf (vx enem) (- (vx enem))
		  turn-routine 80))
	   ((zerop jump-routine)
	    (setf (vy enem) -16
		  jump-routine 75)))
    (whens ((plusp turn-routine) (decf turn-routine))
	   ((plusp jump-routine) (decf jump-routine)))))

;;flying

(define-class flying (enemy)
  (image (get-image :enemy2-l))
  (image-l (get-image :enemy2-l))
  (image-r (get-image :enemy2-r))
  (atk 20))

(defmethod update-object ((enem flying) game)
  (call-next-method)
  (let ((move-dir
	 (dir-univec (get-x enem) (get-y enem)
		     (get-x (player game)) (get-y (player game)))))
    (incf (vx enem) (* 0.2 (first move-dir)))
    (incf (vy enem) (* 0.2 (second move-dir))))
  (when (> (vec-abs (vx enem) (vy enem)) 2)
    (let ((uvec (univec (vx enem) (vy enem))))
      (setf (vx enem) (* 2 (first uvec))
	    (vy enem) (* 2 (second uvec)))))
  (setf (image enem) (if (plusp (vx enem))
			 (image-r enem) (image-l enem))))

;;flying2
(define-class flying2 (enemy)
  (image (get-image :enemy2-l))
  (image-l (get-image :enemy2-l))
  (image-r (get-image :enemy2-r))
  (y-theta 0)
  (updown-omega 6)
  (atk 20))

(defmethod update-object ((enem flying2) game)
  (call-next-method)
  (incf (y-theta enem) (updown-omega enem))
  (setf (y-theta enem) (mod (y-theta enem) 360))
  (setf (vy enem) (floor (* pi (cos (rad (y-theta enem))))))
  (incf (vx enem) (if (< (get-x enem) (get-x (player game))) 0.2 -0.2))
  (setf (vx enem) (clamp (vx enem) -5 5)))

;;fly-and-stop
(define-class fly-and-stop (enemy)
  (image (get-image :enemy2-l))
  (image-l (get-image :enemy2-l))
  (image-r (get-image :enemy2-r))
  (velocity 2)
  (state :stop)
  (act-routine 0)
  (fly-time 30)
  (stop-time 30)
  (atk 20))

(defmethod update-object ((enem fly-and-stop) game)
  (call-next-method)
  (if (plusp (act-routine enem))
      (decf (act-routine enem))
      (let ((move-dir
	    (dir-univec (get-x enem) (get-y enem)
			(get-x (player game)) (get-y (player game)))))
	(let ((new-vx (floor (* (velocity enem) (first move-dir))))
	      (new-vy (floor (* (velocity enem) (second move-dir)))))
	  (change-enemy-state enem (:fly :stop)
	    (state :stop :fly)
	    (act-routine (stop-time enem) (fly-time enem))
	    (vx 0 new-vx)
	    (vy 0 new-vy))))))

;;tullet

(define-class tullet-bullet (enemy-bullet)
  (image (get-image :ebul))
  (vx -2)
  (atk 10))

(define-class tullet (land-enemy)
  (image (get-image :enemy2-l))
  (atk 20)
  (shot-routine 100))

(defmethod update-object ((enem tullet) game)
  (call-next-method)
  (if (zerop (shot-routine enem))
      (let ((ebul (make-instance 'tullet-bullet)))
	(setf (get-x ebul) (- (get-x enem)
			      (truncate (width enem) 2)
			      (truncate (width ebul) 2))
	      (get-y ebul) (get-y enem))
	(push-game-object ebul game)
	(push ebul (enemy-bullets game))
	(setf (shot-routine enem) 100))
      (decf (shot-routine enem))))

