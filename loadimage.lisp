(in-package titechfes)

(defvar *image-table* nil)

(defun load-image (indicator name)
  (setf (getf *image-table* indicator)
	(load-png-image (lib-path name))))

(defun load-animation (indicator name w h source-w source-h)
  (let ((image (load-png-image (lib-path name))))
    (setf (sdl:cells image) (loop for y from 0 to (1- source-h) by h
			       append (loop for x from 0 
					 to (1- source-w) by w
					 collect (list x y w h)))
	  (getf *image-table* indicator) image)))

(defun load-images (&rest i-and-names)
  (mapc #'(lambda (ip) (apply #'load-image ip)) i-and-names))

(defun load-animations (&rest args)
  (mapc #'(lambda (arg) (apply #'load-animation arg)) args))

(defun get-image (indicator)
  (getf *image-table* indicator))

(defun get-image-list (&rest keys)
  (mapcar #'get-image keys))










