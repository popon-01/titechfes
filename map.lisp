(in-package titechfes)

(defun load-map (file-name game)
  (setf (map-size game) (list 0 0))
  (iter (for h upfrom 16 by 32)
	(for line in-file file-name using #'read-line)
	(setf (second (map-size game)) (+ 16 h))
	(appending
	 (iter (for w upfrom 16 by 32)
	       (for code in (split-sequence #\space line))
	       (setf (first (map-size game)) (+ 16 w))
	       (push-game-object (make-game-object code w h)
				 game)))))

(defun make-game-object (code x y)
  (cond ((string= "1" code) (make-instance 'wall :x x :y y))
	((string= "p" code) 
	 (make-instance 'player :x x :y y))
	((string= "a" code) 
	 (make-instance 'aomura :x x :y y :vx 2))
	((string= "t" code)
	 (make-instance 'tullet :x x :y y))
	((string= "i" code) (make-instance 'item :x x :y y))
	((string= "m" code) 
	 (make-instance 'move-wall :x x :y y :vx 1))
	((string= "k" code) 
	 (make-instance 'weapon-item :weapon 'knife :x x :y y))
	((string= "x" code)
	 (make-instance 'weapon-item :weapon 'axe :x x :y y))
	((string= "w" code)
	 (make-instance 'weapon-item :weapon 'two-way :x x :y y))
	((string= "b" code)
	 (make-instance 'weapon-item :weapon 'penetrate :x x :y y))
	((string= "j" code)
	 (make-instance 'weapon-item :weapon 'javelin :x x :y y))
	((string= "e" code)
	 (make-instance 'weapon-item :weapon 'bomb :x x :y y))
	((string= "+" code)
	 (make-instance 'weapon-item :weapon 'boomerang :x x :y y))))

(defmethod push-game-object ((obj null) game))
(defmethod push-game-object ((obj gameobject) (game game))
  (push obj (all-object game)))

(defmethod push-game-object ((w wall) (game game))
  (push w (mapchips game))
  (call-next-method))

(defmethod push-game-object ((p player) (game game))
  (setf (player game) p)
  (call-next-method))

(defmethod push-game-object ((e enemy) (game game))
  (push e (enemies game))
  (call-next-method))



