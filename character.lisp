(in-package :titechfes)

(define-class gamecharacter (gameobject)
  (hp 100)
  (dx 0)
  (dy 0)
  (ax 0)
  (ay 0)
  (rvx 0)
  (rvy 0)
  (muteki nil)
  (muteki-count 0)
  (muteki-time 10))

(defmethod alive-detect ((char gamecharacter) game)
  (when (<= (hp char) 0)
    (kill char game)))

(defmethod dec-muteki-frame ((chr gamecharacter))
  (if (and (muteki chr) (zerop  (muteki-count chr)))
      (setf (muteki chr) nil)
      (decf (muteki-count chr))))

(defmethod update-object ((chr gamecharacter) game)
  (dec-muteki-frame chr)
  (alive-detect chr game)
  (incf (get-x chr) (dx chr))
  (incf (get-y chr) (dy chr))
  (when (out-of-map-p chr game)
    (kill chr game)))

(defmethod attack ((obj gameobject) (char gamecharacter))
  (when (not (muteki char))
    (decf (hp char) (atk obj))
    (setf (muteki char) t
	  (muteki-count char) (muteki-time char))
    (knock-back obj char)))

(defmethod knock-back ((obj gameobject) (char gamecharacter)))


