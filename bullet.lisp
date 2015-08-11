(in-package titechfes)

(define-class bullet (gameobject)
  (vx 0)
  (vy 0)
  (atk 0))

(defmethod collide ((bul bullet) (chip wall))
  (when (rect-collide bul chip) (setf (alive bul) nil)))

(define-class knife (bullet)
  (image (get-image :bullet))
  (life 20))

(defmethod update-object ((bul knife) game)
  (decf (life bul))
  (when (zerop (life bul)) (setf (alive bul) nil))
  (dolist (chip (mapchips game))
    (collide bul chip)))

(defmethod draw-object :before ((bul knife) game)
  (incf (get-x bul) (vx bul))
  (incf (get-y bul) (vy bul)))

