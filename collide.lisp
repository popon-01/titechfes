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
  (setf (dx move-obj) 
	(- (get-x obj2)
	   (pmif (plusp (- (get-x obj2) (get-x move-obj)))
		 (+ (truncate (width obj2) 2)
		    (truncate (width move-obj) 2)))
	   (get-x move-obj))))

(defun adjust-dy (move-obj obj2)
  (setf (dy move-obj) 
	(- (get-y obj2)
	   (pmif (plusp (- (get-y obj2) (get-y move-obj)))
		 (+ (truncate (height obj2) 2)
		    (truncate (height move-obj) 2)))
	   (get-y move-obj))))

(defun dir-detect (target obj)
  (let* ((vx1 (+ (truncate (width target) 2)
		 (truncate (width obj) 2)))
	 (vy1 (+ (truncate (height target) 2)
		 (truncate (height obj) 2)))
	 (vx2 (abs (- (get-x target) (get-x obj))))
	 (vy2 (abs (- (get-y target) (get-y obj))))
	 (res (- (* vx1 vy2) (* vy1 vx2))))
    (cond ((plusp res) :y)
	  ((minusp res) :x))))

(defgeneric collide (obj-a obj-b game))
(defcollide (obj-a gameobject) (obj-b gameobject))

;;player-behavior

(defcollide (chr gamecharacter) (chip wall)
  (with-slots (dx dy) chr
    (when (not (try-move chr chip :dx1 dx :dy1 dy))
      (let ((dir (dir-detect chr chip)))
	(cond ((eq dir :y) (adjust-dy chr chip))
	      ((eq dir :x) (adjust-dx chr chip)))))))

(defcollide (chr player) (chip move-wall)
  (call-next-method)
  (when (and (rect-collide= chr chip)
	     (< (abs (- (get-x chip) (get-x chr)))
		(ash (+ (width chip) (width chr)) -1))
	     (< (get-y chr) (get-y chip)))
    (incf (rvx chr) (vx chip))
    (incf (rvy chr) (vy chip))))

(defcollide (ply player) (enem enemy)
  (when (rect-collide ply enem)
    (attack enem ply)))

(defcollide (ply player) (ebul enemy-bullet)
  (when (rect-collide ply ebul)
    (attack ebul ply)
    (kill ebul)))

(defcollide (e enemy) (ebul enemy-bullet)
  (when (rect-collide e ebul)
    (kill ebul)))

(defcollide (wall damage-wall) (player player)
  (call-next-method)
  (when (rect-collide= player wall)
    (attack wall player)))

(defcollide (wall break-wall) (bullet bullet)
  (when (rect-collide wall bullet)
    (call-next-method)
    (attack bullet wall)))

(defcollide (wall break-wall) (bullet penetrate)
  (when (rect-collide wall bullet)
    (attack bullet wall)))
(defcollide (wall break-wall) (bomb bomb)
  (call-next-method)
  (when (and (rect-collide wall bomb)
	     (string= (state bomb) "explosion"))
    (attack bomb wall)))

(defcollide (wall easy-break-wall) (player player)
  (call-next-method)
  (when (and (rect-collide= wall player)
	     (< (abs (- (get-x wall) (get-x player)))
		(ash (+ (width wall) (width player)) -1))
	     (or (and (in-air player) (< (vy player) 0)
		      (< (get-y wall) (get-y player)))
		 (and (= (1+ (jump-cool player))
			 (jump-cooltime player))
		      (< (get-y player) (get-y wall)))))
    (attack player wall)))

;;enemy-bullet-behavior
(defcollide (ebul enemy-bullet) (chip wall)
  (when (rect-collide ebul chip) (kill ebul)))

;;bullet-behavior
(defcollide (bul bullet) (chip wall)
  (when (rect-collide bul chip) (kill bul)))

(defcollide (enem enemy) (bul bullet)
  (when (rect-collide enem bul)
    (attack bul enem)
    (setf (alive bul) (penetrate bul))))

(defcollide (bul penetrate) (chip wall))

(defcollide (bul bomb) (chip wall)
  (when (rect-collide chip bul)
    (push-game-object (make-instance 'bomb-exp
				     :x (get-x bul) :y (get-y bul))
		      game)
    (kill bul)))

(defcollide (enem enemy) (bul bomb)
  (when (rect-collide enem bul)
    (push-game-object (make-instance 'bomb-exp
				     :x (get-x bul) :y (get-y bul))
		      game)
    (kill bul)))

(defcollide (bul axe) (chip wall)
  (call-next-method)
  (when (and (not (try-move bul chip :dx1 (vx bul) :dy1 (vy bul)))
	     (<= (vy bul) 0)
	     (< (abs (- (get-x bul) (get-x chip)))
		(abs (- (get-y bul) (get-y chip)))))
    (setf (vy bul) (+ (get-y chip)
		      (truncate (height chip) 2)
		      (truncate (height bul) 2)
		      (- (get-y bul))))))

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
  (when (rect-collide item p) 
    (item-effect item p game)
    (kill item)))
