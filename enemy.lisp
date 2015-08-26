(in-package titechfes)


(define-class enemy (gamecharacter)
  (dx 0) (vx 0)
  (dy 0) (vy 0)
  (hp 100)
  (atk 0)
  (dir-right t)
  (muteki nil)
  (muteki-count 0))


(define-class enemy-bullet (bullet)
  (vx 0)
  (vy 0)
  (atk 0))

;;aomura

(define-class aomura (enemy)
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
    (incf (vy enem) *gravity*)
    (whens ((zerop turn-routine)
	    (setf (vx enem) (- (vx enem))
		  turn-routine 80))
	   ((zerop jump-routine)
	    (setf (vy enem) -16
		  jump-routine 75)))
    (setf (dx enem) (vx enem) (dy enem) (vy enem))
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
  (multiple-value-bind (dir-x dir-y)
      (dir-univec (get-x enem) (get-y enem)
		  (get-x (player game)) (get-y (player game)))
    (incf (vx enem) (* 0.2 dir-x))
    (incf (vy enem) (* 0.2 dir-y)))
  (when (> (vec-abs (vx enem) (vy enem)) 2)
    (multiple-value-bind (uni-vx uni-vy) 
	(univec (vx enem) (vy enem))
      (setf (vx enem) (* 2 uni-vx)
	    (vy enem) (* 2 uni-vy))))
  (setf (image enem) (if (plusp (vx enem))
			 (image-r enem) (image-l enem)))
  (setf (dx enem) (floor (vx enem))
	(dy enem) (floor (vy enem))))


;;tullet

(define-class tullet-bullet (enemy-bullet)
  (image (get-image :ebul))
  (vx -2)
  (atk 10))

(define-class tullet (enemy)
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

