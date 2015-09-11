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
	(list 0.0 0.0)
	(list (/ x dist) (/ y dist)))))

(defun dir-univec (sx sy tx ty)
  (let ((vec (univec (- tx sx) (- ty sy))))
    (list (first vec) (second vec))))

(defun slot-list (instance)
  (mapcar #'c2mop:slot-definition-name
	  (c2mop:class-slots (class-of instance))))

(defun nmapslot (fn instance)
  (dolist (slot (slot-list instance))
    (setf (slot-value instance slot)
	  (funcall fn (slot-value instance slot)))))

(defmacro pmif (test num)
  (with-gensyms (gnum)
    `(let ((,gnum ,num))
       (if ,test ,gnum (- ,gnum)))))

(defmacro alambda (vars &body body)
  `(labels ((self ,vars ,@body))
     #'self))

(defmacro letrec (vars &body body)
  `(labels ((rec ,(mapcar #'first vars) ,@body))
     (rec ,@(mapcar #'second vars))))
(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defun to-s (obj)
  (format nil "~a" obj))

(defun a-to-b-vector (a b &optional (xfun #'get-x) (yfun #'get-y))
  (list (- (funcall xfun b) (funcall xfun a))
	(- (funcall yfun b) (funcall yfun a))))

(defun distance (a b &optional (xfun #'get-x) (yfun #'get-y))
  (sqrt (+ (expt (- (funcall xfun a) (funcall xfun b)) 2)
	   (expt (- (funcall yfun a) (funcall yfun b)) 2))))

(defun uvec (a b &optional (xfun #'get-x) (yfun #'get-y))
  (let ((dis (distance a b xfun yfun)))
    (mapcar (lambda (x) (float (/ x dis)))
	    (a-to-b-vector a b xfun yfun))))

(defun make-timer (num)
  (let ((i 0))
    (lambda ()
      (if (<= num i)
	  (progn (setf i 1) t)
	  (progn (incf i) nil)))))

(defun charge-timer (num)
  (let ((charge num))
    (lambda (message)
      (case message
	(:charge (progn (setf charge (clamp (1+ charge) 0 num))
			(<= num charge)))
	(:shot (and (<= num charge)
		    (setf charge 0)
		    t))))))
