(in-package :titechfes)

(defparameter *camera-move-position* 240)

(defun init-camera (game)
  (setf (camera game) (list 0 0)))

;;unko
(defun camera-move (dx dy game)
  (setf (camera game)
	(list 
	 (clamp  (+ (first (camera game)) dx)
		 0
		 (- (first (map-size game))
		    (first (window-size game))))
	 (clamp (+ (second (camera game)) dy)
		0
		(- (second (map-size game))
		   (second (window-size game)))))))

(defun coordinate-in-camera (coord game)
  (mapcar #'+ coord (camera game)))
(defun x-in-camera (x game)
  (- x (first (camera game))))
(defun y-in-camera (y game)
  (- y (second (camera game))))
	 

(defun update-camera (game)
  (let* ((player (player game))
	 (px (x-in-camera (get-x player) game))
	 (py (y-in-camera (get-y player) game)))
    (cond ((< px *camera-move-position*)
	   (camera-move (- px *camera-move-position*) 0 game))
	  ((< (- (first (window-size game)) px) 
	      *camera-move-position*)
	   (camera-move (- *camera-move-position*
			   (- (first (window-size game))px))
			0 game)))
    (cond ((< py *camera-move-position*)
	   (camera-move 0 (- py *camera-move-position*) game))
	  ((< (- (second (window-size game)) py) 
	      *camera-move-position*)
	   (camera-move 0 (- *camera-move-position*
			     (- (second (window-size game))
				py)) game)))))



