(in-package titechfes)

(defun x-center (game &optional (dx 0))
  (+ (ash (first (window-size game)) -1) dx)) 

(defun y-center (game &optional (dy 0))
  (+ (ash (second (window-size game)) -1) dy))

(defun draw-info (game)
  (sdl:draw-box-* 180 15 100 10 :color sdl:*red*)
  (sdl:draw-box-* 180 15 (clamp (truncate (* (hp (player game)) 100)
					  (max-hp (player game)))
				0 100) 10 
				:color sdl:*green*)
  (sdl:draw-string-solid-* "hp" 160 10 )
  (sdl:draw-string-solid-* (to-s (hp (player game)))
			   280 10)
  (sdl:draw-string-solid-* (shot-name (player game))
			   320 10)
  (sdl:draw-string-solid-* "Score" 160 30)
  (sdl:draw-string-solid-* (to-s (score (player game)))
			   210 30))

(defun start-game (stage-name game)
  (setf (bullets game) nil
	(all-object game) nil
	(mapchips game) nil
	(enemies game) nil
	(enemy-bullets game) nil
	(map-size game) nil
	(stage game) stage-name)
  (load-map (lib-path stage-name) game)
  (init-camera game))

(defun gaming-state (game)
  (update-all game)
  (round-robin (rcurry #'collide game)
	       (all-object game))
  (update-camera game)
  (when (not (alive (player game)))
    (setf (all-object game) 
	  (delete (player game) (all-object game)))
    (change-state :over game))
  (sdl:clear-display sdl:*black*)
  (draw-all game)
  (draw-info game))

(let ((cursor 0))
  (defun title-state (game)
    (sdl:clear-display sdl:*black*)
    (sdl:draw-string-solid-* 
     "MATSUMURA"
     (x-center game -60)
     (y-center game)
     :color sdl:*green*)
    (with-slots (up down jump) (keystate game)
      (whens ((key-down-p up) (decf cursor))
	     ((key-down-p down) (incf cursor))
	     ((key-down-p jump) (choice-stage game)))
      (setf cursor (mod cursor 3)))
    (sdl:draw-string-solid-* "stage 1" 
			     (x-center game -60)
			     (y-center game 30))
    (sdl:draw-string-solid-* "stage 2" 
			     (x-center game -60)
			     (y-center game 50))
    (sdl:draw-string-solid-* "stage 3" 
			     (x-center game -60)
			     (y-center game 70))
    (sdl:draw-string-solid-* "->" 
			     (x-center game -90)
			     (y-center game (nth cursor '(30 50 70)))))
#|    (sdl:draw-string-solid-* 
     (concatenate 'string "stage " (to-s (1+ cursor)))
     (x-center game -60)
     (y-center game 30)
     :color sdl:*green*))
|#
  (defun choice-stage (game)
    (let* ((name (concatenate 'string "stage" 
			      (to-s (1+ cursor)))))
      (change-state :game game)
      (start-game name game)
      (setf cursor 0))))

(let ((cursor 0))
  (defun gameover-state (game)
    (gaming-state game)
    (with-slots (up down jump) (keystate game)
      (whens ((key-down-p up) (decf cursor))
	     ((key-down-p down) (incf cursor))
	     ((key-down-p jump) 
	      (if (zerop cursor)
		  (progn (start-game (stage game) game)
			 (change-state :game game))
		  (change-state :title game))
	      (setf cursor 0))))
    (setf cursor (mod cursor 2))
    (sdl:draw-string-solid-* "GAMEOVER"
 			     (x-center game -40)
			     (y-center game))
    (sdl:draw-string-solid-* "restart"
			     (x-center game -40)
			     (y-center game 50))
    (sdl:draw-string-solid-* "title menu"
			     (x-center game -40)
			     (y-center game 70))
    (sdl:draw-string-solid-* "->"
			     (x-center game -60)
			     (y-center game (nth cursor
						 '(50 70))))))
