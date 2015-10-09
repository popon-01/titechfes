(in-package titechfes)

(defun x-center (game &optional (dx 0))
  (+ (ash (first (window-size game)) -1) dx)) 

(defun y-center (game &optional (dy 0))
  (+ (ash (second (window-size game)) -1) dy))

(defun get-icon (bsym)
  (cond ((eq bsym 'knife) (get-image :knife-icon))
	((eq bsym 'javelin) (get-image :javelin-icon))
	((eq bsym 'axe) (get-image :axe-icon))
	((eq bsym 'bomb) (get-image :bomb-icon))
	((eq bsym 'boomerang) (get-image :boomerang-icon))
	(t (get-image :knife-icon))))

(defun draw-hp-info (game)
  (sdl:draw-box-* 170 15 100 10 :color sdl:*red*)
  (sdl:draw-box-* 170 15 (clamp (truncate (* (hp (player game)) 100)
					  (max-hp (player game)))
				0 100) 10 
				:color sdl:*green*)
  (sdl:draw-string-solid-* "HP" 150 10 )
  (sdl:draw-string-solid-* (to-s (hp (player game)))
			   270 10))

(defun draw-weapon-info (game)
  (sdl:draw-string-solid-* (shot-name (player game))
			   300 10)
  (dotimes (i 3)
    (let ((bsym (elt (bullet-list (player game)) i)))
      (when bsym
	(sdl:draw-surface-at-* (get-icon bsym)
			       (+ 390 (* i 30)) 0))))
  (sdl:draw-rectangle-* (+ 390 (* (bullet-i (player game)) 30)) 0
			24 24 :color sdl:*yellow*))

(defun draw-score-info (game)
  (sdl:draw-string-solid-* "Score" 500 0)
  (let ((score-str (to-s (score (player game)))))
    (sdl:draw-string-solid-* score-str
			     (- 640 (* (length score-str) 10))
			     0)))

(defun draw-jump-info (game)
  (with-slots (max-jump jump-count) (player game)
    (sdl:draw-string-solid-* "Jump" 150 25)
    (dotimes (i max-jump)
      (let ((base (if (> (1+ i) jump-count) 50 0)))
	(sdl:draw-filled-circle-* (+ 195 (* i 10)) 33 5 
				  :color (sdl:color :r (- 255 base) 
						    :g (- 165 base)
						    :b (- 50 base)))))))

(defun draw-dash-info (game)
  (with-slots (max-dash dash-count) (player game)
    (sdl:draw-string-solid-* "Dash" 150 40)
    (dotimes (i max-dash)
      (let ((base (if (> (1+ i) dash-count) 50 0)))
	(sdl:draw-filled-circle-* (+ 195 (* i 10)) 48 5 
				  :color (sdl:color :r (- 50 base) 
						    :g (- 144 base)
						    :b (- 255 base)))))))

(defun draw-info (game)
  (when (have-key (player game))
    (sdl:draw-surface-at-* (get-image :key) 130 20))
  (draw-hp-info game)
  (draw-weapon-info game)
  (draw-score-info game)
  (draw-jump-info game)
  (draw-dash-info game))

(defun draw-background (image-name game)
  (let* ((image (get-image image-name))
	 (width (sdl:width image))
	 (height (sdl:height image))
	 (window-height (second (window-size game)))
	 (window-width (first (window-size game)))
	 (map-height (second (map-size game)))
	 (map-width (first (map-size game))))
    (iter (for h from (- map-height height)
	       downto (- height) by height)
	  (iter (for w from 0 to (+ map-width width) 
		     by width)
		(let ((cx (round (x-in-camera w game)))
		      (cy (round (y-in-camera h game))))
		  (when (and (<= (- width) cx window-width)
			     (<= (- height) cy window-height))
		    (sdl:draw-surface-at-* image cx cy)))))))
  

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
  (round-robin (lambda (obj1 obj2)
		 (when (and (< (abs (- (get-x obj1)
				       (get-x obj2))) 100)
			    (< (abs (- (get-y obj1)
				       (get-y obj2))) 100))
		   (collide obj1 obj2 game)))
	       (collide-object-list game))
  (update-camera game)
  (when (not (alive (player game)))
    (setf (all-object game) 
	  (delete (player game) (all-object game)))
    (change-state :over game))
  (sdl:clear-display sdl:*black*)
  (draw-background :back-4 game)
  (draw-all game)
  (draw-info game))

(let ((cursor 0))
  (defun title-state (game)
    (sdl:clear-display sdl:*black*)
    (let* ((logo (get-image :title))
	   (dx (- (ash (sdl:width logo) -1)))
	   (dy (- (ash (sdl:height logo) -1))))
      (sdl:draw-surface-at-* (get-image :title)
			     (x-center game dx)
			     (y-center game (- dy 60))))
    (with-slots (up down jump dash shot weapon start) 
	(keystate game)
      (whens ((key-down-p up) (decf cursor))
	     ((key-down-p down) (incf cursor))
	     ((some #'key-down-p 
		    (list jump dash shot weapon start))
	      (choice-stage game)))
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

  (defun choice-stage (game)
    (let* ((name (concatenate 'string "stage" 
			      (to-s (1+ cursor)))))
      (change-state :game game)
      (start-game name game)
      (setf cursor 0))))

(let ((cursor 0))
  (defun gameover-state (game)
    (gaming-state game)
    (with-slots (up down jump dash shot weapon start)
	(keystate game)
      (whens ((key-down-p up) (decf cursor))
	     ((key-down-p down) (incf cursor))
	     ((some #'key-down-p 
		    (list jump dash shot weapon start)) 
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

(defun stage-clear-state (game)
  (with-slots (jump dash shot weapon start) (keystate game)
    (when (some #'key-down-p 
		(list jump dash shot weapon start))
      (change-state :title game)))
  (sdl:draw-string-solid-* "CLEARED"
			   (x-center game -40)
			   (y-center game))
  (sdl:draw-string-solid-* "Press key to back title"
			   (x-center game -120)
			   (y-center game 30)))
