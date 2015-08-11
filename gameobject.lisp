(in-package titechfes)
;------------------gameobject------------------
(defgeneric update-object (object game))
(defgeneric draw-object (object game))
(defgeneric collide (obj-a obj-b))

(defun update-all (game)
  (dolist (obj (all-object game))
    (update-object obj game)))
(defun draw-all (game)
  (dolist (obj (all-object game))
    (draw-object obj game)))

;;gameobject
(define-class gameobject ()
  (x 0 get-x)
  (y 0 get-y)
  width
  height
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

;;;wall
(define-class wall (gameobject)
  (image (get-image :wall)))

;------------------collide------------------
(defgeneric rect-collide (a b))

(defmethod rect-collide (a b)
  (and (< (- (x a) (/ (width a) 2))
	  (+ (x b) (/ (width b) 2)))
       (< (- (x b) (/ (width b) 2))
	  (+ (x a) (/ (width a) 2)))
       (< (- (y a) (/ (height a) 2))
	  (+ (y b) (/ (height b) 2)))
       (< (- (y b) (/ (height b) 2))
	  (+ (y a) (/ (height a) 2)))))

