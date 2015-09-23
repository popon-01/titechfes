(in-package :titechfes)

(defparameter *game* nil)

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
	       (sdl:update-display)
	       (next-key-state (keystate game)))))))
