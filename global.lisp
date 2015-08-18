(in-package titechfes)

(let ((current-path (load-time-value
		     (or #.*compile-file-pathname* 
			 *load-pathname*))))
  (defvar *dir-path* (directory-namestring current-path)))
(defvar *lib-path* (merge-pathnames "lib/" *dir-path*))
(defvar *width* 320)
(defvar *height* 320)
(defparameter *gravity* 1)
(defparameter *enemy-mutekitime* 10)
