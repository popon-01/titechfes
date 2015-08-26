(in-package titechfes)

;------------------key------------------
(defgeneric update-key-state (key keypress keystate))

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

(defkeystate titechfes-key
    (right :sdl-key-right)
  (left :sdl-key-left)
  (jump :sdl-key-Lshift)
  (down :sdl-key-down)
  (dash :sdl-key-z)
  (shot :sdl-key-x))

#|
(defmacro defkeystate (name &rest keymaps)
  (with-gensym (key key-press key-state)
    `(progn
       (defclass ,name ()
	  ,(mapcar (lambda (x) `(,(car x) :initform 0)) keymaps))
       (defmethod update-key-state (,key ,key-press (,key-state ,name))
	  (with-slots ,(mapcar #'car keymaps) ,key-state
	    (cond ,@(mapcar (lambda (keys)
			     `((sdl:key= ,key ,(cadr keys))
			       (setf ,(car keys) ,key-press)))
			   keymaps))))
       (defmethod next-key-state ((key-state key-state))
	 (nmapslot (lambda (x) (mod x 2)) key-state)))))


(defgeneric update-key-state (key key-press key-state))


(defun key-pressed-p (key)
  (oddp key))
(defun key-down-p (key)
  (= key 3))
(defun key-up-p (key)
  (= key 2))
|#
