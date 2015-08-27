(in-package titechfes)

(define-class item (gameobject)
  (image (get-image :wall)))

(defmethod draw-object ((item item) game)
  (sdl:draw-filled-circle-* (x-in-camera (get-x item) game)
			    (y-in-camera (get-y item) game)
			    10
			    :color sdl:*green*))

(defmethod item-effect ((item item) (player player) game))


(define-class weapon-item (item)
  weapon)

(defmethod item-effect ((item weapon-item) (player player)
			game)
  (change-bullet (weapon item) player)
  (call-next-method))

(define-class recovery-item (item)
  (heal 50))
(defmethod item-effect ((item recovery-item) (player player)
			game)
  (setf (hp player) (clamp (+ (heal item) (hp player))
			   0 (max-hp player))))

(define-class score-item (item)
  (point 50))
(defmethod draw-object ((item score-item) game)
  (sdl:draw-filled-circle-* (x-in-camera (get-x item) game)
			    (y-in-camera (get-y item) game)
			    10
			    :color sdl:*yellow*))

(defmethod item-effect ((item score-item) (player player)
			game)
  (incf (score player) (point item)))

(define-class jump-up (item))
(defmethod item-effect ((item jump-up) (player player) game)
  (incf (max-jump player)))

(define-class dash-up (item))
(defmethod item-effect ((item dash-up) (player player) game)
  (incf (max-dash player)))









