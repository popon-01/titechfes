(in-package titechfes)
;------------------gameobject------------------
(defgeneric update-object (object game))
(defgeneric draw-object (object game))


(defun update-all (game)
  (dolist (obj (all-object game))
    (update-object obj game))
  (setf (all-object game) (remove-if-not #'alive (all-object game))
	(mapchips game) (remove-if-not #'alive (mapchips game))
	(enemies game) (remove-if-not #'alive (enemies game))
	(bullets game) (remove-if-not #'alive (bullets game))))

(defun draw-all (game)
  (dolist (obj (all-object game))
    (draw-object obj game)))

;;gameobject
(define-class gameobject ()
  (x 0 get-x)
  (y 0 get-y)
  width
  height
  (alive t)
  image)

(defmethod initialize-instance :after ((obj gameobject) &key)
  (with-slots (image width height) obj
    (setf width (sdl:width image)
	  height (sdl:height image))))

(defmethod draw-object ((obj gameobject) game)
  (with-slots (x y width height image) obj
    (sdl:draw-surface-at-* image
			   (- (x-in-camera x game) 
			      (/ width 2))
			   (- (y-in-camera y game) 
			      (/ height 2)))))
(defmethod update-object ((obj gameobject) game))

(defmethod kill ((obj gameobject)) (setf (alive obj) nil))
;;;wall
(define-class wall (gameobject)
  (image (get-image :wall)))

(define-class move-wall (wall)
  (vx 0) (vy 0) frame
  (routine 120)
  (stop-frame 0))
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
	  (stop-frame wall) 10)))
  

;------------------collide------------------
(defgeneric rect-collide (a b))

(defmethod rect-collide (a b)
  (and (< (- (get-x a) (/ (width a) 2))
	  (+ (get-x b) (/ (width b) 2)))
       (< (- (get-x b) (/ (width b) 2))
	  (+ (get-x a) (/ (width a) 2)))
       (< (- (get-y a) (/ (height a) 2))
	  (+ (get-y b) (/ (height b) 2)))
       (< (- (get-y b) (/ (height b) 2))
	  (+ (get-y a) (/ (height a) 2)))))

(defun get-left (px w)
  (- px (truncate w 2)))
(defun get-top (py h)
  (- py (truncate h 2)))

(defun rect-collision-judge (rec1 rec2)
  (and (< (sdl:x rec1) (sdl:x2 rec2))
       (< (sdl:x rec2) (sdl:x2 rec1))
       (< (sdl:y rec1) (sdl:y2 rec2))
       (< (sdl:y rec2) (sdl:y2 rec1))))
