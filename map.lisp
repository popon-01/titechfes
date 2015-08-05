(in-package titechfes)

(defmethod initialize-instance :after ((w wall) &key)
  (with-accessors ((image image) (w width) (h height)) w
    (setf w (sdl:width image)
	  h (sdl:height image))))

(defun load-map (file-name)
  (iter (for h upfrom 0)
	(for line in-file file-name using #'read-line)
	(appending
	 (iter (for w upfrom 0)
	       (for code in (split-sequence #\space line))
	       (setf (aref *map* h w) (parse-integer code))
	       (cond ((string= "1" code) 
		      (collect
			  (make-instance 'wall
			     :x (+ (* 32 w) 16)
			     :y (+ (* 32 h) 16)))))))))
