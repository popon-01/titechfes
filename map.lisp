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
(defmacro map-char-table (&rest table)
  `(cond ,@(mapcar (lambda (xs)
		     `((string= ,(car xs) code)
		       (make-instance ',(second xs)
				      ,@(cddr xs) :x x :y y))) table)))

(defun make-game-object (code x y)
  (map-char-table
   ("1" wall)
   ("p" player)
   ("a" aomura)
   ("t" tullet)
   ("f" flying2)
   ("i" item)
   ("m" move-wall :vx 1)
   ("k" weapon-item :weapon 'knife)
   ("x" weapon-item :weapon 'axe)
   ("w" weapon-item :weapon 'two-way)
   ("b" weapon-item :weapon 'penetrate)
   ("j" weapon-item :weapon 'javelin)
   ("e" weapon-item :weapon 'bomb)
   ("+" weapon-item :weapon 'boomerang)
   ("2" damage-wall)
   ("3" break-wall)
   ("4" easy-break-wall)
   ("r" recovery-item)
   ("c" score-item)
   ("s" jump-up)
   ("z" dash-up)))


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

(defmethod push-game-object ((b bullet) (game game))
  (push b (bullets game))
  (call-next-method))

(defmethod push-game-object ((b enemy-bullet) (game game))
  (push b (enemy-bullets game))
  (call-next-method))


