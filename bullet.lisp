(in-package titechfes)

(define-class bullet (gameobject)
  (vx 0)
  (vy 0)
  image-l image-r
  (atk 20)
  (penetrate nil)
  (cool-time 0)
  (knock-back-atk 3))

(defmethod update-object ((bul bullet) game)
  (image-turn bul)
  (when (out-of-map-p bul game)
    (kill bul game))
  (call-next-method))

;---template---
;(define-class name (bullet))
;(defmethod update-object ((bul name) game))
;(defun shot-name (ply game))

(defmacro shoot (weapon player game &rest rest)
  `(set-bullet (make-instance ',weapon ,@rest) ,player ,game))

(defun set-bullet (instance ply game)
  (let ((bul instance))
    (when (not (dir-right ply)) (setf (vx bul) (- (vx bul))))
    (image-turn bul)
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
  (image (get-image :knife-l))
  (image-l (get-image :knife-l))
  (image-r (get-image :knife-r))
  (vx 7)
  (life 30)
  (atk 20)
  (cool-time 10)
  (knock-back-atk 0.8))

(defmethod update-object ((bul knife) game)
  (call-next-method)
  (decf (life bul))
  (when (zerop (life bul)) (kill bul game)))

(defun shot-knife (ply game)
  (shoot knife ply game))

;;;axe
(define-class axe (bullet)
  (image (get-image :axe-l))
  (image-l (get-image :axe-l))
  (image-r (get-image :axe-r))
  (ani-frame 3)
  (atk 30)
  (cool-time 30)
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
  (image (get-image :knife-l))
  (image-l (get-image :knife-l))
  (image-r (get-image :knife-r))
  (atk 20)
  (life 30)
  (cool-time 10)
  (vx 7))

(defmethod update-object ((bul two-way) game)
  (call-next-method)
  (decf (life bul))
  (when (zerop (life bul)) (kill bul game)))

(defun shot-two-way (ply game)
  (shoot two-way ply game :vy -1)
  (shoot two-way ply game :vy -2))

;;penetrate

(define-class penetrate (bullet)
  (image (get-image :knife-l))
  (image-l (get-image :knife-l))
  (image-r (get-image :knife-r))
  (atk 20)
  (life 30)
  (cool-time 10)
  (vx 7))

(defmethod update-object ((bul penetrate) game)
  (call-next-method)
  (decf (life bul))
  (when (zerop (life bul)) (kill bul game)))

(defun shot-penetrate (ply game)
  (shoot penetrate ply game))


;;javelin

(define-class javelin (bullet)
  (image (get-image :javelin-l))
  (image-l (get-image :javelin-l))
  (image-r (get-image :javelin-r))
  (atk 40)
  (life 30)
  (cool-time 60)
  (knock-back-atk 4)
  (vx 14)
  (penetrate nil))

(defmethod update-object ((bul javelin) game)
  (call-next-method)
  (decf (life bul))
  (when (zerop (life bul)) (kill bul game)))

(defun shot-javelin (ply game)
  (shoot javelin ply game))

;;bomb

(define-class bomb (bullet)
  (image (get-image :bomb-l))
  (image-l (get-image :bomb-l))
  (image-r (get-image :bomb-r))
  (atk 0)
  (cool-time 90)
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
  (image-l (get-image :explosion))
  (image-r (get-image :explosion))
  (atk 70)
  (ani-frame 4)
  (knock-back-atk 6)
  (life 30)
  (penetrate t))

(defmethod update-object ((bul bomb-exp) game)
  (call-next-method)
  (decf (life bul))
  (when (zerop (life bul)) (kill bul game)))

;;boomerang

(define-class boomerang (bullet)
  (image (get-image :boomerang-l))
  (image-l (get-image :boomerang-l))
  (image-r (get-image :boomerang-r))
  (ani-frame 3)
  (atk 25)
  (life 30)
  (cool-time 0)
  (limit-timer (make-timer 600))
  (vx 15)
  (penetrate t)
  (knock-back-atk 2)
  (back-velocity 20)
  (state :go)
  (stay-timer (make-timer 120)))

(defmethod update-object ((bul boomerang) game)
  (call-next-method)
  (unless (in-camera-p bul game)
    (kill bul game))
  (when (funcall (limit-timer bul))
    (kill bul game))
  (case (state bul)
    (:go
     (if (plusp (vx bul))
	 (decf (vx bul))
	 (incf (vx bul)))
     (when (zerop (vx bul))
       (setf (state bul) :stay)))
    (:stay
     (when (funcall (stay-timer bul))
       (setf (state bul) :back)))
    (:back
     (let ((dx (- (get-x (player game)) (get-x bul)))
	   (dy (- (get-y (player game)) (get-y bul))))
       (if (and (zerop dx) (zerop dy))
	   (setf (vx bul) 0
		 (vy bul) 0)
	   (setf (vx bul) (truncate (* (back-velocity bul) dx)
				    (sqrt (+ (* dx dx) (* dy dy))))
		 (vy bul) (truncate (* (back-velocity bul) dy)
				    (sqrt (+ (* dx dx) (* dy dy))))))))))

(defmethod kill ((bul boomerang) game)
  (return-boomerang)
  (call-next-method))

(let ((boomerang-exist-p nil))
  (defun boomerang-exist-p () boomerang-exist-p)
  (defun born-boomerang () (setf boomerang-exist-p t))
  (defun return-boomerang () (setf boomerang-exist-p nil)))

(defun shot-boomerang (ply game)
  (unless (boomerang-exist-p)
    (shoot boomerang ply game)
    (born-boomerang)))

