(in-package titechfes)


(define-class enemy (gamecharacter)
  (dx 0) (vx 0)
  (dy 0) (vy 0)
  (hp 100)
  (atk 0)
  (dir-right t)
  (muteki nil)
  (muteki-count 0))

(define-class land-enemy (enemy)
  (in-air t))

(define-class air-enemy (enemy))

(defmethod update-object ((enem land-enemy) game)
  (call-next-method)
  (when (and (minusp (vy enem)) (zerop (dy enem)))
   (setf (vy enem) 0))
  (setf (in-air enem) (not (and (plusp (vy enem)) 
				(zerop (dy enem)))))
  (if (in-air enem)
      (incf (vy enem) *gravity*) (setf (vy enem) 0.1))
  (when (> (vy enem) 10) (setf (vy enem) 10)))


(defmethod update-object  ((enem enemy) game)
  (call-next-method)
  (setf (dx enem) (vx enem) 
	(dy enem) (vy enem)))

(defmethod knock-back ((obj gameobject) (e land-enemy))
  (setf (vx e) (pmif (plusp (- (get-x e) (get-x obj)))
		     (knock-back-atk obj))
	(vy e) -10))

(defmethod knock-back ((obj gameobject) (e air-enemy))
  (let ((knock-dir (univec (- (vx obj) (vx e))
			   (- (vy obj) (vy e)))))
    (setf (vx e) (* (knock-back-atk obj) (first knock-dir))
	  (vy e) (* (knock-back-atk obj) (second knock-dir)))))

(define-class enemy-bullet (gameobject)
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

(defun image-turn (char)
  (setf (image char) 
	(if (plusp (vx char)) 
	    (image-r char)
	    (image-l char))))

;; kuribo

(define-class kuribo (land-enemy)
  (hp 30)
  (atk 10)
  (xspeed 1.4)
  (vx 1.4)
  (turn-% 1)
  (image (get-image :enemy-l))
  (image-r (get-image :enemy-r))
  (image-l (get-image :enemy-l))
  (find-player nil)
  (search-range 100))

(defmethod update-object ((e kuribo) game)
  (call-next-method)
  (image-turn e)
  (when (not (muteki e))
    (if (find-player e)
	(setf (vx e) 
	      (pmif (<= (get-x e) (get-x (player game)))
		    (xspeed e)))
	(whens ((< (random 1000) (* 10 (turn-% e)))
		(setf (vx e) (- (vx e))))
	       ((< (distance e (player game)) (search-range e))
		(setf (find-player e) t))))))

(defmethod knock-back ((char gamecharacter) (e kuribo))
  (call-next-method)
  (setf (find-player e) t))

;;kuribo-tullet

(define-class kuribo-tullet (kuribo)
  (bul-velocity 10)
  (shot-time 60)
  (shot-routine 60))

(define-class kuribo-bullet (enemy-bullet)
  (image (get-image :ebul))
  (atk 20))

(defmethod update-object ((e kuribo-tullet) game)
  (call-next-method)
  (when (find-player e)
    (if (plusp (shot-routine e))
	(decf (shot-routine e))
	(let* ((shot-theta (+ (random 40) 300))
	       (bul-vx (* (bul-velocity e) (cos (rad shot-theta))))
	       (bul-vy (* (bul-velocity e) (sin (rad shot-theta))))
	       (ebul (make-instance 'kuribo-bullet
				    :x (get-x e) :y (get-y e)
				    :vx (pmif 
					 (<= (get-x e)
					     (get-x (player game)))
					 bul-vx)
				    :vy bul-vy)))
	  (push-game-object ebul game)
	  (setf (shot-routine e) (shot-time e))))))

(defmethod update-object ((ebul kuribo-bullet) game)
  (setf (vy ebul) (clamp (+ (vy ebul) *gravity*) -10 10))
  (call-next-method))

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
	   ((and (not (in-air enem)) (zerop jump-routine))
	    (setf (vy enem) -16
		  jump-routine 75)))
    (whens ((plusp turn-routine) (decf turn-routine))
	   ((plusp jump-routine) (decf jump-routine)))))

#|
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
|#

;;flying2
(define-class flying2 (air-enemy)
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
  (incf (vx enem) (if (< (get-x enem) (get-x (player game))) 0.2 -0.2))
  (when (not (muteki enem))
    (setf (vy enem) (* pi (cos (rad (y-theta enem)))))
    (setf (vx enem) (clamp (vx enem) -5 5))))



;;fly-and-stop
(define-class fly-and-stop (air-enemy)
  (image (get-image :enemy2-l))
  (image-l (get-image :enemy2-l))
  (image-r (get-image :enemy2-r))
  (velocity 2)
  (vec-x 0) (vec-y 0)
  (bul-velocity 5)
  (shot-routine 40)
  (state :stop)
  (act-routine 0)
  (fly-time 60)
  (stop-time 60)
  (atk 20))

(define-class fas-bullet (enemy-bullet)
  (image (get-image :ebul))
  (atk 10))

(defun change-fas-state (enem game)
  (let* ((to-player-dir
	  (dir-univec (get-x enem)
		      (get-y enem)
		      (get-x (player game))
		      (get-y (player game))))
	 (new-vx (* (velocity enem) (first to-player-dir)))
	 (new-vy (* (velocity enem) (second to-player-dir))))
    (change-enemy-state enem (:fly :stop)
      (state :stop :fly)
      (act-routine (stop-time enem) (fly-time enem))
      (vec-x new-vx 0)
      (vec-y new-vy 0))))

(defun fas-shot (enem game)
  (let* ((to-player-dir
	  (dir-univec (get-x enem)
		      (get-y enem)
		      (get-x (player game))
		      (get-y (player game))))
	 (bul-vx (* (bul-velocity enem) (first to-player-dir)))
	 (bul-vy (* (bul-velocity enem) (second to-player-dir)))
	 (ebul (make-instance 'fas-bullet
			      :x (get-x enem) :y (get-y enem)
			      :vx bul-vx :vy bul-vy)))
      (push-game-object ebul game)))

(defmethod update-object ((enem fly-and-stop) game)
  (call-next-method)
  (when (and (equal (state enem) :stop)
	     (zerop (mod (+ (act-routine enem) 30)
			 (shot-routine enem))))
    (fas-shot enem game))
  (when (not (muteki enem))
    (if (plusp (act-routine enem))
	(progn
	  (decf (act-routine enem))
	  (setf (vx enem) (vec-x enem)
		(vy enem) (vec-y enem)))
	(change-fas-state enem game))))

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
	(setf (shot-routine enem) 100))
      (decf (shot-routine enem))))

(defmethod knock-back ((char gamecharacter) (e tullet)))


;;demon-gate

(define-class demon-gate (land-enemy)
  (image (get-image :enemy-l))
  (hp 400)
  (atk 20)
  (summon-routine 450)
  (summon-time 450))

(defmethod update-object ((e demon-gate) game)
  (call-next-method)
  (if (plusp (summon-routine e))
      (decf (summon-routine e))
      (let ((minion (make-instance 'kuribo
			      :x (get-x e) :y(get-y e)
			      :vx -1.4 :vy -10)))
	(push-game-object minion game)
	(setf (summon-routine e) (summon-time e)))))


(defmethod knock-back ((char gamecharacter) (e demon-gate)))



