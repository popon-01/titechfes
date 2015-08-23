(in-package titechfes)

(defun try-move (obj1 obj2 &key (dx1 0) (dy1 0) (dx2 0) (dy2 0))
  (sdl:with-rectangle (obj1-rec (sdl:rectangle 
				   :x (+ (get-left obj1) dx1) 
				   :y (+ (get-top obj1) dy1)
				   :w (width obj1)
				   :h (height obj1)))
    (sdl:with-rectangle (obj2-rec (sdl:rectangle 
				    :x (+ (get-left obj2) dx2)
				    :y (+ (get-top obj2) dy2)
				    :w (width obj2)
				    :h (height obj2)))
      (not (rect-collision-judge obj1-rec obj2-rec)))))

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


(defgeneric collide (obj-a obj-b game))
(defcollide (obj-a gameobject) (obj-b gameobject))

;;player-behavior

(defun player-landed (ply)
  (whens ((and (in-air ply))
	  (setf (in-air ply) nil
		(jump-cool ply) 10))
	 ((and (not (dash-ok ply)) (not (while-dash ply)))
	  (setf (dash-ok ply) t))))

(defcollide (ply player) (chip wall)
  (with-slots (dx dy) ply
    (when (not (try-move ply chip :dx1 dx :dy1 dy))
      (let ((dir-x (- (get-x chip) (get-x ply)))
	    (dir-y (- (get-y chip) (get-y ply))))
	(if (< (abs dir-x) (abs dir-y))
	    (progn
	      (if (plusp dy) (player-landed ply))
	      (adjust-dy ply chip))
	    (adjust-dx ply chip))))))

(defcollide (ply player) (ebul enemy-bullet)
  (when (rect-collide ply ebul)
    (when (not (muteki ply))
      (decf (hp ply) (atk ebul))
      (setf (muteki ply) t
	    (muteki-count ply) *player-mutekitime*))
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

;;enemy-bullet-behavior
(defcollide (ebul enemy-bullet) (chip wall)
  (when (rect-collide ebul chip) (setf (alive ebul) nil)))

;;bullet-behavior
(defcollide (bul bullet) (chip wall)
  (when (rect-collide bul chip) (setf (alive bul) nil)))

(defcollide (enem enemy) (bul bullet)
  (when (rect-collide enem bul)
    (when (not (muteki enem))
      (decf (hp enem) (atk bul))
      (setf (muteki enem) t
	    (muteki-count enem) *enemy-mutekitime*))
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

(defcollide (bul boomerang) (chip wall)
  (when (and (rect-collide bul chip) 
	     (equal (state bul) "go"))
    (setf (state bul) "back")))

(defcollide (bul boomerang) (ply player)
  (when (and (rect-collide bul ply)
	     (equal (state bul) "back"))
    (setf (alive bul) nil
	  (shot-cool ply) (cool-time bul))))

(defcollide (item item) (p player)
  (when (rect-collide item p) (kill item)))

(defcollide (item weapon-item) (p player)
  (when (rect-collide item p)
    (change-bullet (weapon item) p)
    (call-next-method)))
