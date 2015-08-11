(in-package :titechfes)

;------------------main------------------
(defun main ()
  (sdl:with-init ()
    (sdl:window 640 480 :title-caption "魔津村")
    (setf (sdl:frame-rate) 60)
    (load-images '(:wall "wall_g.png")
		 '(:bullet "knife.png")
		 '(:player-l "me2.png")
		 '(:player-r "me.png")
		 '(:enemy-l "enemy2.png")
		 '(:enemy-r "enemy.png"))
    (let ((game
	   (make-instance 'game 
	     :player 
	     (make-instance 'player 
		:x 48 
		:y 48 
		:image (get-image 
			:player-l))
	     :keystate 
	     (make-instance 'titechfes-key))))
      (push (player game) (all-object game))
      (let ((map-objects (load-map (lib-path "map.txt") 
				   game))
	    (enem1 (make-instance 'aomura
				  :x 300 :y 48 :vx 2
				  :image (get-image 
					  :enemy-l))))
	(setf (all-object game)
	      (nconc (all-object game) map-objects)
	      (mapchips game)
	      (nconc (mapchips game) map-objects))
	(push enem1 (all-object game))
	(push enem1 (enemies game)))
      (init-camera game)
      (sdl:update-display)
      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))
	(:key-down-event (:key key)
			 (if (sdl:key= key :sdl-key-escape)
			     (sdl:push-quit-event)
			     (update-key-state key t 
				(keystate game))))
	(:key-up-event (:key key)
		       (update-key-state key nil
			   (keystate game)))
	(:idle (update-all game)
	       (update-camera game)
	       (when (not (alive (player game)))
		 (sdl:push-quit-event))
	       (sdl:clear-display sdl:*black*)
	       (draw-all game)
	       (sdl:update-display))))))
