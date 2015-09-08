(in-package :titechfes)

(defun gameimage-load ()
  (load-images '(:wall "wall_g.png")
	       '(:knife "knife.png")
	       '(:axe "axe.png")
	       '(:explosion "explosion.png")
	       '(:player-l "me2.png")
	       '(:player-r "me.png")
	       '(:enemy-l "enemy2.png")
	       '(:enemy-r "enemy.png")
	       '(:enemy2-l "minienemy2.png")
	       '(:enemy2-r "minienemy1.png")
	       '(:ebul "ebul.png")))


(defun draw-info (game)
  (sdl:draw-box-* 180 15 100 10 :color sdl:*red*)
  (sdl:draw-box-* 180 15 (clamp (truncate (* (hp (player game)) 100)
					  (max-hp (player game)))
				0 100) 10 
				:color sdl:*green*)
  (sdl:draw-string-solid-* "hp" 160 10 )
  (sdl:draw-string-solid-* (format nil "~D" 
				   (hp (player game)))
			   280 10)
  (sdl:draw-string-solid-* (shot-name (player game))
			   320 10)
  (sdl:draw-string-solid-* "Score" 160 30)
  (sdl:draw-string-solid-* (format nil "~D" 
				   (score (player game)))
			   210 30))


;------------------main------------------
(defun run ()
  (sdl:with-init ()
    (sdl:window 640 480 :title-caption "魔津村")
    (sdl:initialise-default-font  sdl:*font-9x18b*)
    (gameimage-load)
    (setf (sdl:frame-rate) 60)
    (let ((game (make-instance 'game)))
      (load-map (lib-path "stage1.txt") game)
      (init-camera game)
      (sdl:update-display)
      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))
	(:key-down-event (:key key)
			 (if (sdl:key= key :sdl-key-escape)
			     (sdl:push-quit-event)
			     (update-key-state key 3
				(keystate game))))
	(:key-up-event (:key key)
		       (update-key-state key 2
			   (keystate game)))
	(:idle (update-all game)
	       (round-robin (rcurry #'collide game)
			    (all-object game))
	       (update-camera game)
	       (when (not (alive (player game)))
		 (sdl:push-quit-event))
	       (sdl:clear-display sdl:*black*)
	       (draw-all game)
	       (draw-info game)
	       (sdl:update-display)
	       (next-key-state (keystate game))
	       )))))
