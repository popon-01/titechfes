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
	       '(:demon-gate "demon-gate.png")
	       '(:enemy2-l "minienemy2.png")
	       '(:enemy2-r "minienemy1.png")
	       '(:ebul "ebul.png")
	       '(:big "big.png")
	       '(:big-l "big-l.png")
	       '(:coin "coin.png")
	       '(:dash-up "dash_up.png")
	       '(:jump-up "jump_up.png")
	       '(:goal "goal.png")
	       '(:knife-item "knife_item.png")
	       '(:axe-item "axe_item.png")
	       '(:boomerang-item "boomerang_item.png")
	       '(:javelin-item "javelin_item.png")
	       '(:bomb-item "bomb_item.png")
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

(defun new-joystick ()
  (let ((fp (sdl-cffi::sdl-joystick-open 0)))
    (if (sdl:is-valid-ptr fp)
	fp
	nil)))

;------------------main------------------
(defun run ()
  (sdl:with-init (sdl:sdl-init-joystick)
    (sdl:window 640 480 :title-caption "魔津村")
    (sdl:initialise-default-font  sdl:*font-9x18b*)
    (sdl-cffi::sdl-joystick-event-state sdl-cffi::sdl-enable)
    (gameimage-load)
    (setf (sdl:frame-rate) 60)
    (let ((game (make-instance 'game))
	  (joystick (new-joystick)))
      (init-camera game)
      (sdl:update-display)
      (setf *game* game)
      (sdl:with-events ()
	(:quit-event () 
	   (when (and joystick
		      (> (sdl-cffi::sdl-joystick-opened 0) 0))
	     (sdl-cffi::sdl-joystick-close joystick))
		     t)
	(:video-expose-event () (sdl:update-display))
	(:joy-axis-motion-event (:axis axis :value value)
	   (update-joy-state :axis axis value
			     (joystick (keystate game))))
	(:joy-button-up-event (:button button :state state)
	   (update-joy-state :button button state
			     (joystick (keystate game))))
	(:joy-button-down-event (:button button :state state)
	   (update-joy-state :button button state
			     (joystick (keystate game))))
	(:key-down-event (:key key)
			 (if (sdl:key= key :sdl-key-escape)
			     (sdl:push-quit-event)
			     (update-key-state key 3
				   (key (keystate game)))))
	(:key-up-event (:key key)
		       (update-key-state key 2
			   (key (keystate game))))
	(:idle (update-input (keystate game))
	       (run-state game)
;	       (print (up (joystick (keystate  game))))
	       (sdl:update-display)
	       (next-key-state (keystate game)))))))
