(in-package titechfes)

;------------------key------------------
(defgeneric update-key-state (key keypress keystate))
#|
(defmacro defkeystate (name &rest keys)
  `(progn
     (defclass ,name ()
       ,(loop for k in keys 
	   collect `(,(car k) :initform nil :accessor ,(car k))))
     ,(with-gensyms 
       (key keypress keystate)
       `(defmethod update-key-state (,key ,keypress (,keystate ,name))
	  (with-slots ,(mapcar #'car keys) ,keystate
	    ,@(loop for k in keys
			 collect `(when (sdl:key= ,key ,(cadr k))
					    (setf ,(car k) ,keypress))))))))
|#

(defmacro defkeystate (name &rest keymaps)
  (with-gensyms (key key-press key-state)
    `(progn
       (defclass ,name ()
	  ,(mapcar (lambda (x) `(,(car x) :initform 0)) keymaps))
       (defmethod update-key-state (,key ,key-press (,key-state ,name))
	  (with-slots ,(mapcar #'car keymaps) ,key-state
	    (cond ,@(mapcar (lambda (keys)
			     `((sdl:key= ,key ,(cadr keys))
			       (setf ,(car keys) ,key-press)))
			   keymaps))))
       (defmethod next-key-state ((key-state ,name))
	 (nmapslot (lambda (x) (mod x 2)) key-state)))))



(defkeystate titechfes-key
    (right :sdl-key-right)
  (left :sdl-key-left)
  (jump :sdl-key-c)
  (down :sdl-key-down)
  (up :sdl-key-up)
  (dash :sdl-key-lshift)
  (shot :sdl-key-x)
  (weapon :sdl-key-z))


(defun key-pressed-p (key)
  (oddp key))
(defun key-down-p (key)
  (= key 3))
(defun key-up-p (key)
  (= key 2))
