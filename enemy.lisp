(in-package titechfes)

(define-class enemy (gameobject)
  (vx 0)
  (vy 0)
  (hp 100)
  (muteki nil)
  (muteki-count 0))

(defmethod update-object :before ((enem enemy) game)
  (when (muteki enem)
    (if (zerop (muteki-count enem))
	(setf (muteki enem) nil)
	(decf (muteki-count enem)))))

(defmethod draw-object :before ((enem enemy) game)
  (incf (get-x enem) (vx enem))
  (incf (get-y enem) (vy enem)))

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



;;aomura

(define-class aomura (enemy)
  (image-r (get-image :enemy-r))
  (image-l (get-image :enemy-l))
  (turn-routine 20)
  (jump-routine 75))

(defmethod update-object ((enem aomura) game)
  (with-slots (image-r image-l 
		       turn-routine jump-routine) enem
    (setf (image enem) (if (plusp (vx enem)) image-r image-l))
    (incf (vy enem) *gravity*)
    (whens ((zerop turn-routine)
	    (setf (vx enem) (- (vx enem))
		  turn-routine 75))
	   ((zerop jump-routine)
	    (setf (vy enem) -16
		  jump-routine 75)))
    (whens ((plusp turn-routine) (decf turn-routine))
	   ((plusp jump-routine) (decf jump-routine)))
    (when (<= (hp enem) 0) (setf (alive enem) nil))))

