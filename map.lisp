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
(defun set-*map* (file-name)
  (let* ((lis (iter (for line in-file file-name using #'read-line)
		    (collect 
			(iter 
			  (for chr in (split-sequence #\space
						      line))
			  (collect (parse-integer chr))))))
	(height (length lis))
	 (width (apply #'max (mapcar #'length lis))))
    (setf *map* (make-array (list height width)
			    :initial-contents lis))))
