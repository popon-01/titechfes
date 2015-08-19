(in-package titechfes)

(define-class item (gameobject)
  (image (get-image :wall)))

(defmethod draw-object ((item item) game)
  (sdl:draw-filled-circle-* (x-in-camera (get-x item) game)
			    (y-in-camera (get-y item) game)
			    10
			    :color sdl:*green*))

(defmethod collide ((item item) (p player))
  (when (rect-collide item p) (kill item)))
(defmethod update-object ((item item) (game game))
  (collide item (player game)))

