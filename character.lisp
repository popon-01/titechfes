(in-package :titechfes)

(define-class gamecharacter (gameobject)
  (hp 100)
  (muteki nil)
  (muteki-count 0)
  (muteki-time 10))

(defmethod alive-detect ((char gamecharacter))
  (when (<= (hp char) 0)
    (kill char)))

(defmethod dec-muteki-frame ((chr gamecharacter))
  (if (and (muteki chr) (zerop  (muteki-count chr)))
      (setf (muteki chr) nil)
      (decf (muteki-count chr))))

(defmethod update-object ((chr gamecharacter) game)
  (dec-muteki-frame chr)
  (alive-detect chr)
  (incf (get-x chr) (dx chr))
  (incf (get-y chr) (dy chr)))

(defmethod attack ((obj gameobject) (char gamecharacter))
  (when (not (muteki char))
    (decf (hp char) (atk obj))
    (setf (muteki char) t
	  (muteki-count char) (muteki-time char))))

(defmethod attack ((num integer) (char gamecharacter))
  (when (not (muteki char))
    (decf (hp char) num)
    (setf (muteki char) t
	  (muteki-count char) (muteki-time char))))
