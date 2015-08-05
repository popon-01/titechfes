(in-package titechfes)

(defvar *image-table* nil)

(defun load-image (indicator name)
  (setf (getf *image-table* indicator)
	(load-png-image (lib-path name))))

(defun load-images (&rest i-and-names)
  (mapc #'(lambda (ip) (apply #'load-image ip)) i-and-names))

(defun get-image (indicator)
  (getf *image-table* indicator))

(defun get-image-list (&rest keys)
  (mapcar #'get-image keys))
