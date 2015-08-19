(in-package titechfes)

;;player-behavior
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

(definteract-method collide (ply player) (ebul enemy-bullet)
  (when (rect-collide ply ebul)
    (when (not (muteki ply))
      (decf (hp ply) (atk ebul))
      (setf (muteki ply) t
	    (muteki-count ply) *player-mutekitime*))
    (setf (alive ebul) nil)))

;;enemy-behavior
(definteract-method collide (enem enemy) (chip wall)
  (with-slots (vx vy) enem
    (sdl:with-rectangle (chip-rec (sdl:rectangle 
				   :x (get-left (get-x chip) 
						(width chip)) 
				   :y (get-top (get-y chip)
					       (height chip))
				   :w (width chip)
				   :h (height chip)))
      ;;horizontal
      (sdl:with-rectangle (enem-x-rec (sdl:rectangle 
				      :x (get-left (+ (get-x enem) vx)
						   (width enem))
				      :y (get-top (get-y enem) 
						  (height enem))
				      :w (width enem)
				      :h (height enem)))
	(when (rect-collision-judge chip-rec enem-x-rec)
	  (if (plusp vx)
	      (setf vx (- (get-x chip) 
			  (+ (truncate (width chip) 2)
			     (truncate (width enem) 2))
			  (get-x enem)))
	      (setf vx (- (get-x chip)
			  (- (+ (truncate (width chip) 2)
				(truncate (width enem) 2))) 
			  (get-x enem))))))
      ;;vertical
      (sdl:with-rectangle (enem-y-rec (sdl:rectangle 
				       :x (get-left (get-x enem)
						    (width enem))
				       :y (get-top (+ (get-y enem) vy) 
						   (height enem))
				       :w (width enem)
				       :h (height enem)))
	(when (rect-collision-judge chip-rec enem-y-rec)
	  (if (plusp vy)
	      (setf vy (- (get-y chip)
			  (+ (truncate (height chip) 2)
			     (truncate (height enem) 2))
			  (get-y enem)))
	      (setf vy (- (get-y chip)
			  (- (+ (truncate (height chip) 2)
				(truncate (height enem) 2)))
			  (get-y enem)))))))))

;;enemy-bullet-behavior
(definteract-method collide (ebul enemy-bullet) (chip wall)
  (when (rect-collide ebul chip) (setf (alive ebul) nil)))

;;bullet-behavior
(definteract-method collide (bul bullet) (chip wall)
  (when (rect-collide bul chip) (setf (alive bul) nil)))

(definteract-method collide (enem enemy) (bul bullet)
  (when (rect-collide enem bul)
    (when (not (muteki enem))
      (decf (hp enem) (atk bul))
      (setf (muteki enem) t
	    (muteki-count enem) *enemy-mutekitime*))
    (setf (alive bul) (penetrate bul))))

(definteract-method collide (bul penetrate) (chip wall))

(definteract-method collide (bul bomb) (chip wall)
  (when (and (rect-collide bul chip) 
	     (equal (state bul) "bomb"))
    (make-explosion bul)))

(definteract-method collide (enem enemy) (bul bomb)
  (when (rect-collide enem bul)
    (cond ((equal (state bul) "bomb") 
	   (make-explosion bul))
	  ((equal (state bul) "explosion")
	   (decf (hp enem) (atk bul))))))

(definteract-method collide (bul boomerang) (chip wall)
  (when (and (rect-collide bul chip) 
	     (equal (state bul) "go"))
    (setf (state bul) "back")))

(definteract-method collide (bul boomerang) (ply player)
  (when (and (rect-collide bul ply)
	     (equal (state bul) "back"))
    (setf (alive bul) nil
	  (shot-cool ply) (cool-time bul))))
