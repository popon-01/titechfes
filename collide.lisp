(in-package titechfes)

(defun try-move (obj1 obj2 &key (dx1 0) (dy1 0) (dx2 0) (dy2 0))
  (not (and (< (- (+ (get-x obj1) dx1) (/ (width obj1) 2))
	       (+ (+ (get-x obj2) dx2) (/ (width obj2) 2)))
	    (< (- (+ (get-x obj2) dx2) (/ (width obj2) 2))
	       (+ (+ (get-x obj1) dx1) (/ (width obj1) 2)))
	    (< (- (+ (get-y obj1) dy1) (/ (height obj1) 2))
	       (+ (+ (get-y obj2) dy2) (/ (height obj2) 2)))
	    (< (- (+ (get-y obj2) dy2) (/ (height obj2) 2))
	       (+ (+ (get-y obj1) dy1) (/ (height obj1) 2))))))

(defun adjust-dx (move-obj obj2)
  (setf (dx move-obj) (- (get-x obj2)
			 (if (plusp (- (get-x obj2) (get-x move-obj)))
			     (+ (truncate (width obj2) 2)
				(truncate (width move-obj) 2))
			     (- (+ (truncate (width obj2) 2)
				   (truncate (width move-obj) 2))))
			 (get-x move-obj))))

(defun adjust-dy (move-obj obj2)
  (setf (dy move-obj) (- (get-y obj2)
			 (if (plusp (- (get-y obj2) (get-y move-obj)))
			     (+ (truncate (height obj2) 2)
				(truncate (height move-obj) 2))
			     (- (+ (truncate (height obj2) 2)
				   (truncate (height move-obj) 2))))
			 (get-y move-obj))))

(defun dir-detect (target obj)
  (let* ((vx1 (+ (truncate (width target) 2)
		 (truncate (width obj) 2)))
	 (vy1 (+ (truncate (height target) 2)
		 (truncate (height obj) 2)))
	 (vx2 (abs (- (get-x target) (get-x obj))))
	 (vy2 (abs (- (get-y target) (get-y obj))))
	 (res (- (* vx1 vy2) (* vy1 vx2))))
    (cond ((plusp res) "y")
	  ((minusp res) "x")
	  (t "bound"))))

(defgeneric set-muteki (obj))
(defmethod set-muteki ((ply player))
  (setf (muteki ply) t
	(muteki-count ply) *player-mutekitime*))
(defmethod set-muteki ((enem enemy))
  (setf (muteki enem) t
	(muteki-count enem) *enemy-mutekitime*))


(defgeneric collide (obj-a obj-b game))
(defcollide (obj-a gameobject) (obj-b gameobject))

;;player-behavior

(defun player-landed (ply)
  (setf (in-air ply) nil
	(jump-count ply) (max-jump ply)
	(dash-count ply) (max-dash ply)
	(dash-cooltime ply) 40))

(defcollide (ply player) (chip wall)
  (with-slots (dx dy) ply
    (when (not (try-move ply chip :dx1 dx :dy1 dy))
      (let ((dir (dir-detect ply chip)))
	(cond ((equal dir "y") (progn
				 (adjust-dy ply chip)
				 (if (< (get-y ply) (get-y chip))
				     (player-landed ply))))
	      ((equal dir "x") (adjust-dx ply chip)))))))

(defcollide (ply player) (enem enemy)
  (when (and (rect-collide ply enem) (not (muteki ply)))
    (decf (hp ply) (atk enem))
    (set-muteki ply)
    (multiple-value-bind (dir-x dir-y)
	(dir-univec (get-x enem) (get-y enem)
		    (get-x ply) (get-y ply))
      (incf (vvx ply) (floor (* 10 dir-x)))
      (setf (vy ply) (floor (* 10 dir-y))))))

(defcollide (ply player) (ebul enemy-bullet)
  (when (rect-collide ply ebul)
    (when (not (muteki ply))
      (decf (hp ply) (atk ebul))
      (set-muteki ply))
    (setf (alive ebul) nil)))

;;enemy-behavior
(defcollide (enem enemy) (chip wall)
  (with-slots (dx dy) enem
    (when (not (try-move enem chip :dx1 dx :dy1 dy))
      (let ((dir-x (- (get-x chip) (get-x enem)))
	    (dir-y (- (get-y chip) (get-y enem))))
	(if (< (abs dir-x) (abs dir-y))
	    (adjust-dy enem chip)
	    (adjust-dx enem chip))))))

(defcollide (enem flying) (chip wall)
  (with-slots (dx dy vx vy) enem
    (when (not (try-move enem chip :dx1 dx :dy1 dy))
      (let ((dir-x (- (get-x chip) (get-x enem)))
	    (dir-y (- (get-y chip) (get-y enem))))
	(if (< (abs dir-x) (abs dir-y))
	    (progn 
	      (adjust-dy enem chip)
	      (setf vy (* 3 (- vy))))
	    (progn
	      (adjust-dx enem chip)
	      (setf vx (* 3 (- vx)))))))))

;;enemy-bullet-behavior
(defcollide (ebul enemy-bullet) (chip wall)
  (when (rect-collide ebul chip) (setf (alive ebul) nil)))

;;bullet-behavior
(defcollide (bul bullet) (chip wall)
  (when (rect-collide bul chip) (kill bul)))

(defcollide (enem enemy) (bul bullet)
  (when (rect-collide enem bul)
    (when (not (muteki enem))
      (decf (hp enem) (atk bul))
      (set-muteki enem))
    (setf (alive bul) (penetrate bul))))

(defcollide (bul penetrate) (chip wall))

(defcollide (bul bomb) (chip wall)
  (when (and (rect-collide bul chip) 
	     (equal (state bul) "bomb"))
    (make-explosion bul)))

(defcollide (enem enemy) (bul bomb)
  (when (rect-collide enem bul)
    (cond ((equal (state bul) "bomb") 
	   (make-explosion bul))
	  ((equal (state bul) "explosion")
	   (decf (hp enem) (atk bul))))))

(defcollide (bul axe) (chip wall)
  (whens ((and (not (try-move bul chip :dx1 (vx bul) :dy1 (vy bul)))
	       (<= (vy bul) 0)
	       (< (abs (- (get-x bul) (get-x chip)))
		  (abs (- (get-y bul) (get-y chip)))))
	  (setf (vy bul) (+ (get-y chip)
			    (truncate (height chip) 2)
			    (truncate (height bul) 2)
			    (- (get-y bul)))))
	 ((rect-collide bul chip) (kill bul))))
  

(defcollide (bul boomerang) (chip wall)
  (when (and (rect-collide bul chip) 
	     (equal (state bul) "go"))
    (setf (state bul) "back")))

(defcollide (bul boomerang) (ply player)
  (when (and (rect-collide bul ply)
	     (equal (state bul) "back"))
    (kill bul)
    (setf (shot-cool ply) (cool-time bul))))

(defcollide (item item) (p player)
  (when (rect-collide item p) (kill item)))

(defcollide (item weapon-item) (p player)
  (when (rect-collide item p)
    (change-bullet (weapon item) p)
    (call-next-method)))
