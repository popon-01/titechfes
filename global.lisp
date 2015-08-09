(in-package titechfes)

(defvar *playerimage-r* nil)
(defvar *playerimage-l* nil)
(defvar *wallimage* nil)
(defvar *map* (make-array '(100 100)))
(defvar *keystate* nil)
(let ((current-path (load-time-value
		     (or #.*compile-file-pathname* 
			 *load-pathname*))))
  (defvar *dir-path* (directory-namestring current-path)))
(defvar *lib-path* (merge-pathnames "lib/" *dir-path*))
(defvar *width* 320)
(defvar *height* 320)










