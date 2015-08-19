(in-package titechfes)

(define-class bullet (gameobject)
  (vx 0)
  (vy 0)
  (atk 0)
  (penetrate nil)
  (cool-time 0))

(defmethod draw-object :before ((bul bullet) game)
  (incf (get-x bul) (vx bul))
  (incf (get-y bul) (vy bul)))


;---template---
;(define-class name (bullet))
;(defmethod update-object ((bul name) game))
;(defun shot-name (ply game))

(defun set-bullet (instance ply game)
  (let ((bul instance))
    (when (not (dir-right ply)) (setf (vx bul) (- (vx bul))))
    (setf (get-x bul) (if (dir-right ply)
			  (+ (get-x ply)
			     (truncate (width ply) 2) 
			     (truncate (width bul) 2))
			  (- (get-x ply)
			     (truncate (width ply) 2) 
			     (truncate (width bul) 2)))
	  (get-y bul) (get-y ply))
    (push bul (all-object game))
    (push bul (bullets game))
    (setf (shot-cool ply) (cool-time bul))))

;;;knife
(define-class knife (bullet)
  (image (get-image :knife))
  (vx 7)
  (life 20)
  (atk 20)
  (cool-time 10))

(defmethod update-object ((bul knife) game)
  (decf (life bul))
  (when (zerop (life bul)) (setf (alive bul) nil)))

(defun shot-knife (ply game)
    (set-bullet (make-instance 'knife) ply game))

;;;axe
(define-class axe (bullet)
  (image (get-image :axe))
  (atk 50)
  (cool-time 15)
  (vx 1)
  (vy -20))

(defmethod update-object ((bul axe) game)
  (incf (vy bul) *gravity*)
  (when (> (vy bul) 10) (setf (vy bul) 10)))


(defun shot-axe (ply game)
  (set-bullet (make-instance 'axe) ply game))

;;2way
(define-class two-way (bullet)
  (image (get-image :knife))
  (atk 20)
  (life 30)
  (cool-time 10)
  (vx 7))

(defmethod update-object ((bul two-way) game)
  (decf (life bul))
  (when (zerop (life bul)) (setf (alive bul) nil)))

(defun shot-two-way (ply game)
  (set-bullet (make-instance 'two-way :vy -1) ply game)
  (set-bullet (make-instance 'two-way :vy -2) ply game))

;;penetrate

(define-class penetrate (bullet)
  (image (get-image :knife))
  (atk 20)
  (life 30)
  (cool-time 10)
  (vx 7))

(defmethod update-object ((bul penetrate) game)
  (decf (life bul))
  (when (zerop (life bul)) (setf (alive bul) nil)))

(defun shot-penetrate (ply game)
  (set-bullet (make-instance 'penetrate) ply game))


;;javelin

(define-class javelin (bullet)
  (image (get-image :knife))
  (atk 50)
  (life 30)
  (cool-time 50)
  (vx 14)
  (penetrate t))

(defmethod update-object ((bul javelin) game)
  (decf (life bul))
  (when (zerop (life bul)) (setf (alive bul) nil)))

(defun shot-javelin (ply game)
  (set-bullet (make-instance 'javelin) ply game))

;;bomb

(define-class bomb (bullet)
  (image (get-image :axe))
  (atk 0)
  (cool-time 15)
  (vx 3)
  (vy -10)
  (state "bomb")
  (life 10))

(defmethod update-object ((bul bomb) game)
  (cond ((equal (state bul) "bomb")
	 (incf (vy bul) *gravity*)
	 (when (> (vy bul) 10) (setf (vy bul) 10)))
	((equal (state bul) "explosion")
	 (decf (life bul))
	 (when (zerop (life bul)) (setf (alive bul) nil)))))


(defun shot-bomb (ply game)
  (set-bullet (make-instance 'bomb) ply game))

(defun make-explosion (bul)
  (setf (image bul) (get-image :explosion))
  (setf (state bul) "explosion"
	(width bul) (sdl:width (image bul))
	(height bul) (sdl:height (image bul))
	(vx bul) 0
	(vy bul) 0
	(atk bul) 50))


;;boomerang

(define-class boomerang (bullet)
  (image (get-image :knife))
  (atk 20)
  (life 30)
  (cool-time 10)
  (vx 15)
  (penetrate t)
  (back-velocity 20)
  (state "go")
  (stay-count 30))

(defmethod update-object ((bul boomerang) game)
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
  (set-bullet (make-instance 'boomerang) ply game)
  (setf (shot-cool ply) 100000))

