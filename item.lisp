(in-package titechfes)

(define-class item (gameobject)
  (image (get-image :wall)))

(defmethod item-effect ((item item) (player player) game))


(define-class weapon-item (item)
  weapon)

(defmethod initialize-instance :after ((item weapon-item) 
				       &key)
  (setf (image item)
	(get-image (make-keyword (symbolicate (weapon item)
					      '-item)))))

(defmethod item-effect ((item weapon-item) (player player)
			game)
  (get-bullet (weapon item) player)
  (call-next-method))

(define-class recovery-item (item)
  (ani-frame 10)
  (image (get-image :heat))
  (heal 50))

(defmethod item-effect ((item recovery-item) (player player)
			game)
  (setf (hp player) (clamp (+ (heal item) (hp player))
			   0 (max-hp player))))

(define-class score-item (item)
  (point 50)
  (image (get-image :coin))
  (ani-frame 10))

(defmethod item-effect ((item score-item) (player player)
			game)
  (incf (score player) (point item)))

(define-class jump-up (item) 
  (image (get-image :jump-up))
  (ani-frame 10))
(defmethod item-effect ((item jump-up) (player player) game)
  (incf (max-jump player)))

(define-class dash-up (item)
  (image (get-image :dash-up))
  (ani-frame 10))

(defmethod item-effect ((item dash-up) (player player) game)
  (incf (max-dash player)))

(define-class clear-item (item) 
  (image (get-image :goal))
  (ani-frame 10))
(defmethod item-effect ((item clear-item) (player player) game)
  (update-all game)
  (sdl:update-display)
  (change-state :stage-clear game))

(define-class stage-key (item) (image (get-image :key)))
(defmethod item-effect ((item stage-key) (player player) game)
  (setf (have-key player) t))
