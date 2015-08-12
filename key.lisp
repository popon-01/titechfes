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

