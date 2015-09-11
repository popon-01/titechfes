(in-package titechfes)

(define-class bullet (gameobject)
  (vx 0)
  (vy 0)
  (atk 20)
  (penetrate nil)
  (cool-time 0)
  (knock-back-atk 10))


;---template---
;(define-class name (bullet))
;(defmethod update-object ((bul name) game))
;(defun shot-name (ply game))

(defmacro shoot (weapon player game &rest rest)
  `(set-bullet (make-instance ',weapon ,@rest) ,player ,game))

(defun set-bullet (instance ply game)
  (let ((bul instance))
    (when (not (dir-right ply)) (setf (vx bul) (- (vx bul))))
    (setf (get-x bul) 
	  (funcall (if (dir-right ply) #'+ #'-)
		   (get-x ply)
		   (truncate (width ply) 2)
		   (truncate (width bul) 2))
	  (get-y bul) (get-y ply))
    (push-game-object bul game)
    (setf (shot-cool ply) (cool-time bul))))

;;;knife
(define-class knife (bullet)
  (image (get-image :knife))
  (vx 7)
  (life 40)
  (atk 20)
  (cool-time 10)
  (knock-back-atk 5))

(defmethod update-object ((bul knife) game)
  (call-next-method)
  (decf (life bul))
  (when (zerop (life bul)) (kill bul)))

(defun shot-knife (ply game)
  (shoot knife ply game))

;;;axe
(define-class axe (bullet)
  (image (get-image :axe))
  (ani-frame 3)
  (atk 50)
  (cool-time 15)
  (vx 1)
  (vy -20))

(defmethod update-object ((bul axe) game)
  (call-next-method)
  (incf (vy bul) *gravity*)
  (when (> (vy bul) 10) (setf (vy bul) 10)))


(defun shot-axe (ply game)
  (shoot axe ply game))

;;2way
(define-class two-way (bullet)
  (image (get-image :knife))
  (atk 20)
  (life 30)
  (cool-time 10)
  (vx 7))

(defmethod update-object ((bul two-way) game)
  (call-next-method)
  (decf (life bul))
  (when (zerop (life bul)) (kill bul)))

(defun shot-two-way (ply game)
  (shoot two-way ply game :vy -1)
  (shoot two-way ply game :vy -2))

;;penetrate

(define-class penetrate (bullet)
  (image (get-image :knife))
  (atk 20)
  (life 30)
  (cool-time 10)
  (vx 7))

(defmethod update-object ((bul penetrate) game)
  (call-next-method)
  (decf (life bul))
  (when (zerop (life bul)) (kill bul)))

(defun shot-penetrate (ply game)
  (shoot penetrate ply game))


;;javelin

(define-class javelin (bullet)
  (image (get-image :javelin))
  (atk 50)
  (life 30)
  (cool-time 50)
  (vx 14)
  (penetrate t))

(defmethod update-object ((bul javelin) game)
  (call-next-method)
  (decf (life bul))
  (when (zerop (life bul)) (kill bul)))

(defun shot-javelin (ply game)
  (shoot javelin ply game))

;;bomb

(define-class bomb (bullet)
  (image (get-image :axe))
  (atk 0)
  (cool-time 15)
  (vx 3)
  (vy -10))

(defmethod update-object ((bul bomb) game)
  (call-next-method)
  (incf (vy bul) *gravity*)
  (when (> (vy bul) 10) (setf (vy bul) 10)))

(defun shot-bomb (ply game)
  (shoot bomb ply game))

(define-class bomb-exp (bullet)
  (image (get-image :explosion))
  (atk 50)
  (life 10))

(defmethod update-object ((bul bomb-exp) game)
  (call-next-method)
  (decf (life bul))
  (when (zerop (life bul)) (kill bul)))

;;boomerang

(define-class boomerang (bullet)
  (image (get-image :boomerang))
  (ani-frame 3)
  (atk 20)
  (life 30)
  (cool-time 10)
  (vx 15)
  (penetrate t)
  (back-velocity 20)
  (state "go")
  (stay-count 30))

(defmethod update-object ((bul boomerang) game)
  (call-next-method)
  (cond ((equal (state bul) "go")
	 (if (plusp (vx bul))
	     (decf (vx bul))
	     (incf (vx bul)))
	 (when (zerop (vx bul))
	   (setf (state bul) "stay")))
	((equal (state bul) "stay")
	 (if (zerop (stay-count bul))
	     (setf (state bul) "back")
	     (decf (stay-count bul))))
	((equal (state bul) "back")
	 (let ((dx (- (get-x (player game)) (get-x bul)))
	       (dy (- (get-y (player game)) (get-y bul))))
	   (if (and (zerop dx) (zerop dy))
	       (setf (vx bul) 0
		     (vy bul) 0)
	       (setf (vx bul) (truncate (* (back-velocity bul) dx)
					(sqrt (+ (* dx dx) (* dy dy))))
		     (vy bul) (truncate (* (back-velocity bul) dy)
					(sqrt (+ (* dx dx) (* dy dy))))))))))

(defun shot-boomerang (ply game)
  (shoot boomerang ply game)
  (setf (shot-cool ply) 100000))
