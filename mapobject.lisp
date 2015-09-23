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
      (sdl:draw-box-* (round (- cx (ash w -1))) 
		      (round (- cy (ash h -1) 10)) 32 10
			       :color sdl:*red*))))

(define-class break-wall (wall)
  (max-hp 100)
  (hp 100))

(defmethod attack ((obj gameobject) (wall break-wall))
  (decf (hp wall) (atk obj)))

(defmethod update-object ((wall break-wall) game)
  (whens ((< (hp wall) (max-hp wall))
	  (setf (image wall) (get-image :dameged-wall)))
	 ((< (hp wall) (/ (max-hp wall) 2))
	  (setf (image wall) (get-image :dameged2-wall)))	 
	 ((not (plusp (hp wall)))
	  (kill wall game))))

(define-class easy-break-wall (break-wall)
  (max-hp 30)
  (hp 30))

(define-class locked-wall (wall)
  (image (get-image :locked-wall)))

(define-class switch (gameobject)
  image-off
  image-on
  color)

(defun switch-change (indicator game)
  (setf (getf (wall-flag game) indicator)
	(not (getf (wall-flag game) indicator))))

(defun switch-on-p (indicator game)
  (getf (wall-flag game) indicator))

(defun switch-image-turn (obj game)
  (setf (image obj) (if (switch-on-p (color obj) game)
			(image-on obj) (image-off obj))))

(defmethod update-object ((switch switch) game)
  (switch-image-turn switch game))

(define-class red-switch (switch)
  (image (get-image :switch-red-on))
  (image-off (get-image :switch-red-off))
  (image-on (get-image :switch-red-on))
  (color :red))

(define-class blue-switch (switch)
  (image (get-image :switch-blue-on))
  (image-off (get-image :switch-blue-off))
  (image-on (get-image :switch-blue-on))
  (color :blue))

(define-class yellow-switch (switch)
  (image (get-image :switch-yellow-on))
  (image-off (get-image :switch-yellow-off))
  (image-on (get-image :switch-yellow-on))
  (color :yellow))

(define-class green-switch (switch)
  (image (get-image :switch-green-on))
  (image-off (get-image :switch-green-off))
  (image-on (get-image :switch-green-on))
  (color :green))


(define-class switch-wall (wall)
  image-on image-off color)

(define-class active-wall (switch-wall))
(define-class inactive-wall (switch-wall))

(defmethod update-object ((wall switch-wall) game)
  (switch-image-turn wall game))

(define-class active-red-wall (active-wall)
  (image (get-image :wall-red-on))
  (image-off (get-image :wall-red-off))
  (image-on (get-image :wall-red-on))
  (color :red))

(define-class inactive-red-wall (inactive-wall)
  (image (get-image :wall-red-off))
  (image-off (get-image :wall-red-on))
  (image-on (get-image :wall-red-off))
  (color :red))

(define-class active-blue-wall (active-wall)
  (image (get-image :wall-blue-on))
  (image-off (get-image :wall-blue-off))
  (image-on (get-image :wall-blue-on))
  (color :blue))

(define-class inactive-blue-wall (inactive-wall)
  (image (get-image :wall-blue-off))
  (image-off (get-image :wall-blue-on))
  (image-on (get-image :wall-blue-off))
  (color :blue))

(define-class active-yellow-wall (active-wall)
  (image (get-image :wall-yellow-on))
  (image-off (get-image :wall-yellow-off))
  (image-on (get-image :wall-yellow-on))
  (color :yellow))

(define-class inactive-yellow-wall (inactive-wall)
  (image (get-image :wall-yellow-off))
  (image-off (get-image :wall-yellow-on))
  (image-on (get-image :wall-yellow-off))
  (color :yellow))

(define-class active-green-wall (active-wall)
  (image (get-image :wall-green-on))
  (image-off (get-image :wall-green-off))
  (image-on (get-image :wall-green-on))
  (color :green))

(define-class inactive-green-wall (inactive-wall)
  (image (get-image :wall-green-off))
  (image-off (get-image :wall-green-on))
  (image-on (get-image :wall-green-off))
  (color :green))
