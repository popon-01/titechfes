(in-package :titechfes)

(defvar *camera-position* '(0 0))
(defparameter *camera-move-position* 50)

(defun init-camera ()
  (setf *camera-position* (list 0 0)))

;;unko
(defun camera-move (dx dy)
  (setf *camera-position*
	(list 
	 (alexandria:clamp  (+ (first *camera-position*) dx)
			    0
			    (* 32 (1- (array-dimension *map* 1))))
	 (alexandria:clamp (+ (second *camera-position*) dy)
			   (* -32 (1- (array-dimension *map* 0)))
			   0))))

(defun coordinate-in-camera (coord)
  (mapcar #'+ coord *camera-position*))
(defun x-in-camera (x)
  (- x (first *camera-position*)))
(defun y-in-camera (y)
  (- y (second *camera-position*)))
	 

(defun update-camera (player)
  (let ((px (x-in-camera (get-x player)))
	(py (y-in-camera (get-y player))))
    (cond ((< px *camera-move-position*)
	   (camera-move (- px *camera-move-position*) 0))
	  ((< (- *width* px) *camera-move-position*)
	   (camera-move (- *camera-move-position*
			   (- *width* px)) 0)))
    (cond ((< py *camera-move-position*)
	   (camera-move 0 (- py *camera-move-position*)))
	  ((< (- *width* py) *camera-move-position*)
	   (camera-move 0 (- *camera-move-position*
			     (- *width* py)))))))


;unko
(defmethod draw-object :before (obj)
  (decf (slot-value obj 'x) (first *camera-position*))
  (decf (slot-value obj 'y) (second *camera-position*)))
(defmethod draw-object :after (obj)
  (incf (slot-value obj 'x) (first *camera-position*))
  (incf (slot-value obj 'y) (second *camera-position*)))

