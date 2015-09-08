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
  (x 0 get-x) (vx 0)
  (y 0 get-y) (vy 0)
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
			   (- (round (x-in-camera x game)) 
			      (/ width 2))
			   (- (round (y-in-camera y game)) 
			      (/ height 2)))))

(defmethod update-object ((obj gameobject) game)
  (incf (get-x obj) (vx obj))
  (incf (get-y obj) (vy obj)))

(defmethod kill ((obj gameobject)) (setf (alive obj) nil))

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

(defun get-left (obj)
  (- (get-x obj) (truncate (width obj) 2)))
(defun get-top (obj)
  (- (get-y obj) (truncate (height obj) 2)))

(defmethod rect-collide= (a b)
  (and (<= (- (get-x a) (/ (width a) 2))
	   (+ (get-x b) (/ (width b) 2)))
       (<= (- (get-x b) (/ (width b) 2))
	   (+ (get-x a) (/ (width a) 2)))
       (<= (- (get-y a) (/ (height a) 2))
	   (+ (get-y b) (/ (height b) 2)))
       (<= (- (get-y b) (/ (height b) 2))
	   (+ (get-y a) (/ (height a) 2)))))

(defun rect-collision-judge (rec1 rec2)
  (and (< (sdl:x rec1) (sdl:x2 rec2))
       (< (sdl:x rec2) (sdl:x2 rec1))
       (< (sdl:y rec1) (sdl:y2 rec2))
       (< (sdl:y rec2) (sdl:y2 rec1))))
