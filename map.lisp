(in-package titechfes)

(defun load-map (file-name game)
  (setf (map-size game) (list 0 0))
  (iter (for h upfrom 0)
	(for line in-file file-name using #'read-line)
	(setf (second (map-size game)) h)
	(appending
	 (iter (for w upfrom 0)
	       (for code in (split-sequence #\space line))
	       (setf (first (map-size game)) w)
	       (cond ((string= "1" code)
		      (collect
			  (make-instance 'wall
			     :x (+ (* 32 w) 16)
			     :y (+ (* 32 h) 16)))))))))

