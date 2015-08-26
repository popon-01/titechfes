(in-package titechfes)

;;;wall
(define-class wall (gameobject)
  (image (get-image :wall)))

(define-class move-wall (wall)
  (vx 0) (vy 0) frame
  (routine 120)
  (stop-frame 0)
  (stop-time 10))

(defmethod initialize-instance :after ((wall move-wall) &key)
  (setf (frame wall) (routine wall)))

(defmethod update-object ((wall move-wall) (game game))
  (when (plusp (stop-frame wall))
    (decf (stop-frame wall))
    (return-from update-object))
  (incf (get-x wall) (vx wall))
  (incf (get-y wall) (vy wall))
  (decf (frame wall))
  (when (zerop (frame wall))
    (setf (vx wall) (- (vx wall))
	  (vy wall) (- (vy wall))
	  (frame wall) (routine wall)
	  (stop-frame wall) (stop-time wall))))

(define-class damage-wall (wall)
  (atk 20))

(defmethod draw-object ((wall damage-wall) game)
  (call-next-method)
  (with-accessors ((x get-x) (y get-y) (w width) (h height))
      wall
    (let* ((cx (x-in-camera x game))
	   (cy (y-in-camera y game)))
      (sdl:draw-box-* (- cx (ash w -1)) (- cy (ash h -1) 10) 32 10
			       :color sdl:*red*))))
(define-class break-wall (wall gamecharacter)
  (hp 100)
  (muteki-time 0))

(defmethod update-object ((wall break-wall) game)
  (alive-detect wall)
  (dec-muteki-frame wall))

(define-class easy-break-wall (break-wall)
  (hp 30))
