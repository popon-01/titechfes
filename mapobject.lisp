(in-package titechfes)

;;;wall
(define-class wall (gameobject)
  (image (get-image :wall)))

(define-class move-wall (wall)
  (vx 0) (vy 0)
  (routine 120)
  (stop-time 10)
  (move-function nil))

(defmethod initialize-instance :after ((wall move-wall) &key)
  (setf (move-function wall) 
	(move-generator wall)))

(defun move-generator (mwall)
  (let ((frame (routine mwall))
	(state :move)
	(default-vx (vx mwall))
	(default-vy (vy mwall)))
    (lambda ()
      (decf frame)
      (when (zerop frame)
	(case state
	  (:move  (setf (vx mwall) 0
			(vy mwall) 0
			frame (stop-time mwall)
			state :stop))
	  (:stop (setf (vx mwall) (- default-vx)
		       (vy mwall) (- default-vy)
		       frame (routine mwall)
		       state :-move))
	  (:-move (setf (vx mwall) 0
			(vy mwall) 0
			frame (stop-time mwall)
			state :stop2))
	  (:stop2 (setf (vx mwall) default-vx
			(vy mwall) default-vy
			frame (routine mwall)
			state :move)))))))

(defmethod update-object ((wall move-wall) (game game))
  (call-next-method)
  (funcall (move-function wall)))

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
