(in-package titechfes)

;------------------macro------------------
(defmacro with-gensyms (lis &body body)
  `(let (,@(mapcar (lambda (x) `(,x (gensym))) lis))
     ,@body))

(defmacro define-class (class-name parent  &rest res)
  `(defclass ,class-name ,parent
     ,(mapcar (lambda (lis)
		(if (listp lis)
		    (apply(lambda (x &optional (y nil) (z x))
			    `(,x :initarg 
				 ,(intern (symbol-name x) "KEYWORD") 
				 :initform ,y :accessor ,z))
			  lis)
		    ((lambda (x) 
		       `(,x :initarg 
			    ,(intern (symbol-name x) "KEYWORD") 
			    :initform nil :accessor ,x))
		     lis)))
	      res)))

(defmacro whens (&body body)
  `(progn
     ,@(loop for b in body collect `(when ,@b))))

(defmacro definteract-method (method-name arg1 arg2 &body body)
  `(progn
     (defmethod ,method-name (,arg1 ,arg2)
       ,@body)
     (defmethod ,method-name (,arg2 ,arg1)
       (,method-name ,(car arg1) ,(car arg2)))))

(defun round-robin (fn lis)
  (mapl (lambda (xs) (mapcar (lambda (x) (funcall fn (car xs) x))
			     (cdr xs)))
	lis))
