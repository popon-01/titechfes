(in-package titechfes)

(defun load-png-image (src)
  (sdl:convert-to-display-format :surface (sdl:load-image src)
				 :enable-alpha t
				 :pixel-alpha T))

(defun lib-path (name)
  (if (position #\. name)
      (destructuring-bind (name type) (split-sequence #\. name)
	(make-pathname :defaults *lib-path* :name name :type type))
      (make-pathname :defaults *lib-path* :name name :type "txt")))
