(in-package :titechfes)

;------------------main------------------

(defun load-png-image (src)
  (sdl:convert-to-display-format :surface (sdl:load-image src)
				 :enable-alpha t
				 :pixel-alpha T))

(defun lib-path (name)
  (destructuring-bind (name type) (split-sequence #\. name)
    (make-pathname :defaults *lib-path* :name name :type type)))

(defun main ()
  (sdl:with-init ()
    (sdl:window 320 320 :title-caption "lispbuilder-sdlサンプル")
    (setf (sdl:frame-rate) 60)
    (setf *playerimage* (load-png-image (lib-path "me.png")))
    (setf *wallimage* (load-png-image (lib-path "wall_g.png")))
    (setf *keystate* (make-instance 'titechfes-key))
    (let ((player (make-instance 'player 
				 :x 48 :y 48 :image *playerimage*))
	  (all-objects (make-instance 'obj-group))
	  (mapchips (make-instance 'map-group)))
      (init-object player all-objects)
      (with-open-file (in (lib-path "map.txt") :direction :input)
	(dotimes (i 10)
	  (dotimes (j 10)
	    (setf (aref *map* i j) (read in))
	    (when (equal (aref *map* i j) 1)
	      (init-object (make-instance 'wall
					  :x (+ (* 32 j) 16) 
					  :y (+ (* 32 i) 16)
					  :image *wallimage*)
			   all-objects mapchips)))))
      (sdl:update-display)
      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))
	(:key-down-event (:key key)
			 (if (sdl:key= key :sdl-key-escape)
			     (sdl:push-quit-event)
			     (update-key-state key t *keystate*)))
	(:key-up-event (:key key)
		       (update-key-state key nil *keystate*))
	(:idle (update-all all-objects)
	       (sdl:clear-display sdl:*black*)
	       (draw-all all-objects)
	       (sdl:update-display))))))
