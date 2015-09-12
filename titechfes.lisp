(in-package :titechfes)

(defparameter *game* nil)

(defun gameimage-load ()
  (load-images '(:wall "wall_g.png")
	       '(:knife-l "knife2-l.png")
	       '(:knife-r "knife2-r.png")
	       '(:javelin-l "javelin-l.png")
	       '(:javelin-r "javelin-r.png")
	       '(:explosion "explosion.png")
	       '(:player-l "me2.png")
	       '(:player-r "me.png")
	       '(:enemy-l "enemy2.png")
	       '(:enemy-r "enemy.png")
	       '(:enemy2-l "minienemy2.png")
	       '(:enemy2-r "minienemy1.png")
	       '(:ebul "ebul.png")
	       '(:big "big.png")
	       '(:big-l "big-l.png")
	       '(:title "title_touka.png"))
  (load-animations '(:boomerang-l "boomerang-l_ani.png"
		     24 24 96 24)
		   '(:boomerang-r "boomerang-r_ani.png"
		     24 24 96 24)
		   '(:axe-l "axe-l_ani.png"
		     24 24 96 24)
		   '(:axe-r "axe-r_ani.png"
		     24 24 96 24)
		   '(:bomb-l "bomb-l_ani.png"
		     24 24 96 24)
		   '(:bomb-r "bomb-r_ani.png"
		     24 24 96 24)))

;------------------main------------------
(defun run ()
  (sdl:with-init ()
    (sdl:window 640 480 :title-caption "魔津村")
    (sdl:initialise-default-font  sdl:*font-9x18b*)
    (gameimage-load)
    (setf (sdl:frame-rate) 60)
    (let ((game (make-instance 'game)))
      (init-camera game)
      (sdl:update-display)
      (setf *game* game)
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
	(:idle (run-state game)
	       (sdl:update-display)
	       (next-key-state (keystate game))
	       )))))
