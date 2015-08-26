(in-package titechfes)

;------------------macro------------------


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

(defmacro defcollide (arg1 arg2 &body body)
  `(progn
     (defmethod collide (,arg1 ,arg2 game)
       ,@body)
     (defmethod collide (,arg2 ,arg1 game)
       ,@body)))

(defun round-robin (fn lis)
  (mapl (lambda (xs) (mapcar (lambda (x) (funcall fn (car xs) x))
			     (cdr xs)))
	lis))

(defun rad (degree) (* pi (/ degree 180.0)))

(defun vec-abs (x y)
  (sqrt (+ (* x x) (* y y))))

(defun euc-dist (x1 y1 x2 y2)
  (vec-abs (- x1 x2) (- y1 y2)))

(defun univec (x y)
  (let ((dist (vec-abs x y)))
    (if (zerop dist)
	(values 0.0 0.0)
	(values (/ x dist) (/ y dist)))))

(defun dir-univec (sx sy tx ty)
  (multiple-value-bind (res-x res-y) 
      (univec (- tx sx) (- ty sy))
    (values res-x res-y)))
