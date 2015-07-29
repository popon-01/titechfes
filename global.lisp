(in-package titechfes)

(defvar *playerimage* nil)
(defvar *wallimage* nil)
(defvar *map* (make-array '(10 10)))
(defvar *keystate* nil)
(let ((current-path (load-time-value
		     (or #.*compile-file-pathname* 
			 *load-pathname*))))
  (defvar *dir-path* (directory-namestring current-path)))
(defvar *lib-path* (merge-pathnames "lib/" *dir-path*))



