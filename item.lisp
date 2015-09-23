(in-package titechfes)

(define-class item (gameobject)
  (image (get-image :wall)))
#|
(defmethod draw-object ((item item) game)
  (sdl:draw-filled-circle-* (round (x-in-camera (get-x item) game))
			    (round (y-in-camera (get-y item) game))
			    10
			    :color sdl:*green*))
|#
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
  (heal 50))

(defmethod item-effect ((item recovery-item) (player player)
			game)
  (setf (hp player) (clamp (+ (heal item) (hp player))
			   0 (max-hp player))))
(defmethod draw-object ((item recovery-item) game)
  (sdl:draw-filled-circle-* (round (x-in-camera (get-x item) game))
			    (round (y-in-camera (get-y item) game))
			    10
			    :color sdl:*green*))


(define-class score-item (item)
  (point 50)
  (image (get-image :coin)))
#|
(defmethod draw-object ((item score-item) game)
  (sdl:draw-filled-circle-* (round (x-in-camera (get-x item) game))
			    (round (y-in-camera (get-y item) game))
			    10
			    :color sdl:*yellow*))
|#
(defmethod item-effect ((item score-item) (player player)
			game)
  (incf (score player) (point item)))

(define-class jump-up (item) (image (get-image :jump-up)))
(defmethod item-effect ((item jump-up) (player player) game)
  (incf (max-jump player)))

(define-class dash-up (item)
  (image (get-image :dash-up)))
(defmethod item-effect ((item dash-up) (player player) game)
  (incf (max-dash player)))

(define-class clear-item (item) (image (get-image :goal)))
(defmethod item-effect ((item clear-item) (player player) game)
  (update-all game)
  (sdl:update-display)
  (change-state :stage-clear game))

(define-class stage-key (item) (image (get-image :key)))
(defmethod item-effect ((item stage-key) (player player) game)
  (setf (have-key player) t))
