(in-package titechfes)


(define-class enemy (gamecharacter)
  (dx 0) (vx 0)
  (dy 0) (vy 0)
  (hp 100)
  (atk 0)
  (dir-right t)
  (muteki nil)
  (muteki-count 0)
  (item-table  '(score-item score-item score-item 
		 weapon-item recovery-item)))

(defmethod kill ((enemy enemy) game)
  (call-next-method)
  (item-drop enemy game))

(defmethod item-drop ((enemy enemy) game)
  (let ((item (random-elt (item-table enemy))))
    (push-game-object
     (if (eq 'weapon-item item)
	 (make-instance 
	  item 
	  :x (get-x enemy) 
	  :y (get-y enemy)
	  :weapon (random-elt
		   '(knife axe javelin bomb boomerang)))
	 (make-instance 
	  item 
	  :x (get-x enemy) 
	  :y (get-y enemy))) 
     game)))

(define-class land-enemy (enemy)
  (in-air t)
  (hp 120))

(define-class tullet-enemy (land-enemy))

(define-class air-enemy (enemy)
  (hp 60))

(defmethod update-object ((enem land-enemy) game)
  (call-next-method)
  (when (and (minusp (vy enem)) (zerop (dy enem)))
    (setf (vy enem) 0))
  (setf (in-air enem) (not (and (plusp (vy enem)) 
				(zerop (dy enem)))))
  (if (in-air enem)
      (incf (vy enem) *gravity*) (setf (vy enem) 0.1))
  (when (> (vy enem) 10) (setf (vy enem) 10)))

(defmethod update-object ((enem tullet-enemy) game)
  (call-next-method)
  (setf (vx enem) 0))

(defmethod update-object  ((enem enemy) game)
  (call-next-method)
  (setf (dx enem) (+ (vx enem) (rvx enem)) 
	(dy enem) (+ (vy enem) (rvy enem))
	(rvx enem) 0 (rvy enem) 0))

(defmethod knock-back ((obj gameobject) (e land-enemy))
  (setf (vx e) (pmif (plusp (- (get-x e) (get-x obj)))
		     (knock-back-atk obj))
	(vy e) (- (* 3 (knock-back-atk obj)))))

(defmethod knock-back ((obj gameobject) (e air-enemy))
  (let ((knock-dir (univec (- (vx obj) (vx e))
			   (- (vy obj) (vy e)))))
    (setf (vx e) (* (knock-back-atk obj) (first knock-dir))
	  (vy e) (* (knock-back-atk obj) (second knock-dir))
	  (find-player e) t)))

(defmethod knock-back ((obj gameobject) (e tullet-enemy)))

(define-class enemy-bullet (bullet)
  (vx 0)
  (vy 0)
  (atk 20))

(defmacro change-enemy-state (enem states &body body)
  `(progn 
     ,@(loop for b in body
	  collect `(setf (,(car b) enem) 
	   (cond ,@(mapcar (lambda (state val) 
			     (list `(equal (state ,enem) ,state) val))
			   states (cdr b)))))))

(defun search-player (enemy game)
  (setf (find-player enemy)
	(or (find-player enemy)
	    (< (distance enemy (player game))
	       (search-range enemy)))))

;; kuribo

(define-class kuribo (land-enemy)
  (hp 70)
  (atk 10)
  (xspeed 1.4)
  (vx 1.4)
  (p-turn 0.4)
  (turn-routine (make-timer 40))
  (knock-back-timer (make-timer 20))
  (image (get-image :enemy-l))
  (image-r (get-image :enemy-r))
  (image-l (get-image :enemy-l))
  (find-player nil)
  (state #'kuribo-walk)
  (search-range 150))

(defmethod update-object ((e kuribo) game)
  (call-next-method)
  (image-turn e)
  (search-player e game)
  (funcall (state e) e game))

(defstate kuribo-walk (e game)
  (declare (ignore game))
  (when (and (funcall (turn-routine e))
	     (< (random 1.0) (p-turn e)))
    (setf (vx e) (pmif (minusp (vx e))
		       (xspeed e))))
  (when (find-player e)
    (setf (state e) #'kuribo-walk-to-player)))

(defstate kuribo-walk-to-player (e game)
  (setf (vx e) 
	(pmif (<= (get-x e) (get-x (player game)))
	      (xspeed e))))

(defstate kuribo-knock-back (e game)
  (declare (ignore game))
  (when (funcall (knock-back-timer e))
    (setf (state e) #'kuribo-walk-to-player)))


(defmethod knock-back ((obj gameobject) (e kuribo))
  (call-next-method)
  (setf (find-player e) t
	(state e) #'kuribo-knock-back))

;;kuribo-tullet

(define-class kuribo-tullet (kuribo)
  (bullet-speed 5)
  (shot-time 60)
  (shot-routine (charge-timer 60)))

(define-class kuribo-bullet (enemy-bullet)
  (image (get-image :ebul))
  (image-l (get-image :ebul))
  (image-r (get-image :ebul))
  (atk 30))

(defmethod update-object ((e kuribo-tullet) game)
  (call-next-method)
  (funcall (shot-routine e) :charge)
  (and (find-player e)
       (funcall (shot-routine e) :shot)
       (let* ((bul-vx (pmif (<= (get-x e) 
				(get-x (player game)))
			    (* (bullet-speed e)
			       (clamp (random 1.5) 1 1.5))))
	      (bul-vy (- (* (bullet-speed e)
			    (clamp (random 1.5) 1 1.5))))
	      (ebul (make-instance 
		     'kuribo-bullet
		     :x (+ (get-x e) (* 4 bul-vx))
		     :y (+ (get-y e) (* 4 bul-vy))
		     :vx bul-vx
		     :vy bul-vy)))
	 (push-game-object ebul game))))

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


;;flying2
(define-class flying2 (air-enemy)
  (image (get-image :enemy2-l))
  (image-l (get-image :enemy2-l))
  (image-r (get-image :enemy2-r))
  (y-theta 0)
  (find-player nil)
  (search-range 200)
  (updown-omega 6)
  (atk 20))

(defmethod update-object ((enem flying2) game)
  (call-next-method)
  (search-player enem game)
  (when (find-player enem)
    (incf (y-theta enem) (updown-omega enem))
    (setf (y-theta enem) (mod (y-theta enem) 360))
    (incf (vx enem) (if (< (get-x enem) (get-x (player game))) 0.2 -0.2))
    (when (not (muteki enem))
      (setf (vy enem) (+ (* pi (cos (rad (y-theta enem))))
			 (* 0.005  (- (get-y (player game))
				      (get-y enem)
				      100)))
	    (vx enem) (clamp (vx enem) -5 5)))))

;;fly-and-stop
(define-class fly-and-stop (air-enemy)
  (image (get-image :enemy2-l))
  (image-l (get-image :enemy2-l))
  (image-r (get-image :enemy2-r))
  (velocity 2)
  (vec-x 0) (vec-y 0)
  (bullet-speed 5)
  (shot-routine 40)
  (state :stop)
  (act-routine 0)
  (search-range 200)
  (find-player nil)
  (fly-time 60)
  (stop-time 60)
  (atk 20))

(define-class fas-bullet (enemy-bullet)
  (image (get-image :ebul))
  (image-l (get-image :ebul))
  (image-r (get-image :ebul))
  (atk 10))

(defun shot-to-player (enem bullet game)
  (let ((ebul (make-instance bullet))
	(uvec (uvec enem (player game))))
    (setf (get-x ebul) (+ (get-x enem)
			  (* 30 (first uvec)))
	  (get-y ebul) (+ (get-y enem)
			  (* 30 (second uvec)))
	  (vx ebul)
	  (* (bullet-speed enem) (first uvec))
	  (vy ebul) 
	  (* (bullet-speed enem) (second uvec)))
    (push-game-object ebul game)))

(defun change-fas-state (enem game)
  (let* ((to-player-dir
	  (dir-univec (get-x enem)
		      (get-y enem)
		      (get-x (player game))
		      (- (get-y (player game)) 100)))
	 (new-vx (* (velocity enem) (first to-player-dir)))
	 (new-vy (* (velocity enem) (second to-player-dir))))
    (change-enemy-state enem (:fly :stop)
      (state :stop :fly)
      (act-routine (stop-time enem) (fly-time enem))
      (vec-x new-vx 0)
      (vec-y new-vy 0))))

(defmethod update-object ((enem fly-and-stop) game)
  (call-next-method)
  (search-player enem game)
  (when (find-player enem)
    (when (and (equal (state enem) :stop)
	       (zerop (mod (+ (act-routine enem) 30)
			   (shot-routine enem))))
      (shot-to-player enem 'fas-bullet game))
  (when (not (muteki enem))
    (if (plusp (act-routine enem))
	(progn
	  (decf (act-routine enem))
	  (setf (vx enem) (vec-x enem)
		(vy enem) (vec-y enem)))
	(change-fas-state enem game)))))

;;tullet

(define-class tullet-bullet (enemy-bullet)
  (image (get-image :ebul))
  (image-l (get-image :ebul))
  (image-r (get-image :ebul))
  (vx -2)
  (atk 10))

(define-class tullet (tullet-enemy)
  (image (get-image :enemy2-l))
  (atk 20)
  (bullet-speed-x -4)
  (bullet-speed-y 0)
  (shot-routine 60)
  (item-table  '(score-item score-item score-item 
		 weapon-item recovery-item)))

(defmethod update-object ((enem tullet) game)
  (call-next-method)
  (if (zerop (shot-routine enem))
      (progn (tullet-shot enem game)
	     (setf (shot-routine enem) 100))
      (decf (shot-routine enem))))

(defmethod tullet-shot ((enem tullet) game)
  (let ((ebul (make-instance 'tullet-bullet)))
    (setf (get-x ebul) (- (get-x enem)
			  (truncate (width enem) 2)
			  (truncate (width ebul) 2))
	  (get-y ebul) (get-y enem)
	  (vx ebul) (bullet-speed-x enem)
	  (vy ebul) (bullet-speed-y enem))
    (push-game-object ebul game)))


;;demon-gate

(define-class demon-gate (tullet-enemy)
  (image (get-image :demon-gate))
  (hp 250)
  (atk 20)
  (summon-timer (charge-timer 300))
  (summon-list nil)
  (summon-limit 5))

(defmethod update-object ((e demon-gate) game)
  (call-next-method)
  (with-slots (summon-timer summon-list summon-limit) e
    (setf summon-list (remove-if-not #'alive summon-list))
    (when (and (< (length summon-list) summon-limit)
	       (funcall summon-timer :charge)
	       (funcall summon-timer :shot))
      (let ((minion (make-instance 'kuribo
				   :x (get-x e) :y(get-y e)
				   :vx -1.4 :vy -10)))
	(push-game-object minion game)
	(push minion summon-list)))))


;; snipe-tullet
(define-class snipe-tullet (tullet)
  (image-l (get-image :enemy2-l))
  (image-r (get-image :enemy2-r))
  (find-player nil)
  (search-range 300)
  (bullet-speed 4))

(defmethod update-object ((enem snipe-tullet) game)
  (search-player enem game)
  (setf (image enem) 
	(if (< (get-x enem) (get-x (player game)))
	    (image-r enem)
	    (image-l enem)))
  (call-next-method))

(defmethod tullet-shot ((enem snipe-tullet) game)
  (let ((e->p (a-to-b-vector enem (player game)))
	(dist (distance enem (player game))))
    (when (find-player enem)
      (let ((ebul (make-instance 'tullet-bullet))
	    (uvec (mapcar (lambda (x) (float (/ x dist)))
			  e->p)))
	(setf (get-x ebul) (+ (get-x enem)
			      (* 30 (first uvec)))
	      (get-y ebul) (+ (get-y enem)
			      (* 30 (second uvec)))
	      (vx ebul)
	      (* (bullet-speed enem) (first uvec))
	      (vy ebul) 
	      (* (bullet-speed enem) (second uvec)))
	(push-game-object ebul game)))))

;; big
(define-class big (land-enemy)
  (hp 500)
  (atk 30)
  (lspeed 1.8)
  (hspeed 5.0)
  (vx -1.8)
  (dash-timer (make-timer 60))
  (stop-timer (make-timer 20))
  (turn-timer (make-timer 60))
  (p-turn 0.4)
  (dash-recovery (charge-timer 60))
  (find-player nil)
  (search-range 200)
  (image (get-image :big-l))
  (image-l (get-image :big-l))
  (image-r (get-image :big))
  (armer nil)
  (knock-back-timer (make-timer 15))
  (state #'big-walk)
  (item-table  '(dash-up jump-up)))

(defmethod knock-back ((obj gameobject) (b big))
  (setf (find-player b) t)
  (unless (armer b)
    (setf (state b) #'big-knock-back)
    (setf (vx b) (pmif (plusp (- (get-x b) (get-x obj)))
		       (* 0.5 (knock-back-atk obj)))
	  (vy b) -5)))

(defmethod update-object ((b big) game)
  (call-next-method)
  (search-player b game)
  (image-turn b)
  (funcall (state b) b game))

(defstate big-walk (b game)
  (declare (ignore game))
  (when (and (funcall (turn-timer b))
		      (< (random 1.0) (p-turn b)))
	     (setf (vx b)
		   (pmif (minusp (vx b)) (lspeed b))))
  (when (find-player b)
    (setf (state b) #'big-walk-to-player)))

(defstate big-walk-to-player (b game)
  (funcall (dash-recovery b) :charge)
  (setf (vx b)
	(pmif (< (get-x b) (get-x (player game)))
	      (lspeed b)))
  (when (funcall (dash-recovery b) :shot)
    (setf (state b) #'big-stop)))

(defstate big-stop (b game)
  (setf (armer b) t (vx b) 0)
  (when (funcall (stop-timer b))
    (setf (state b) #'big-dash
	  (vx b) (pmif (< (get-x b) 
			  (get-x (player game)))
		       (hspeed b)))))

(defstate big-dash (b game)
  (declare (ignore game))
  (setf (atk b) 50)
  (when (funcall (dash-timer b))
    (setf (state b) (if (find-player b)
			#'big-walk-to-player
			#'big-walk)
	  (armer b) nil
	  (atk b) 30)))

(defstate big-knock-back (b game)
  (declare (ignore game))
  (funcall (dash-recovery b) :charge)
  (when (funcall (knock-back-timer b))
    (setf (state b) 
	  (cond ((funcall (dash-recovery b) :shot)
		 #'big-stop)
		((find-player b)
		 #'big-walk-to-player)
		(t #'big-walk))
	  (armer b) nil)))
