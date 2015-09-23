(in-package titechfes)

(defvar *image-table* nil)

(defun load-image (indicator name)
  (setf (getf *image-table* indicator)
	(load-png-image (lib-path name))))

(defun load-animation (indicator name w h source-w source-h)
  (let ((image (load-png-image (lib-path name))))
    (setf (sdl:cells image) (loop for y from 0 to (1- source-h) by h
			       append (loop for x from 0 
					 to (1- source-w) by w
					 collect (list x y w h)))
	  (getf *image-table* indicator) image)))

(defun load-images (&rest i-and-names)
  (mapc #'(lambda (ip) (apply #'load-image ip)) i-and-names))

(defun load-animations (&rest args)
  (mapc #'(lambda (arg) (apply #'load-animation arg)) args))

(defun get-image (indicator)
  (getf *image-table* indicator))

(defun get-image-list (&rest keys)
  (mapcar #'get-image keys))

(defun mapchip-image-load ()
  (load-images '(:wall "wall_g.png")
	       '(:dameged-wall "damaged_wall_g.png")
	       '(:dameged2-wall "damaged2_wall_g.png")
	       '(:switch-red-on "switch_r_on.png")
	       '(:switch-red-off "switch_r_off.png")
	       '(:switch-blue-on "switch_b_on.png")
	       '(:switch-blue-off "switch_b_off.png")
	       '(:switch-yellow-on "switch_y_on.png")
	       '(:switch-yellow-off "switch_y_off.png")
	       '(:switch-green-on "switch_gr_on.png")
	       '(:switch-green-off "switch_gr_off.png")
	       '(:wall-red-on "wall_r_on.png")
	       '(:wall-red-off "wall_r_off.png")
	       '(:wall-blue-on "wall_b_on.png")
	       '(:wall-blue-off "wall_b_off.png")
	       '(:wall-yellow-on "wall_y_on.png")
	       '(:wall-yellow-off "wall_y_off.png")
	       '(:wall-green-on "wall_gr_on.png")
	       '(:wall-green-off "wall_gr_off.png")))

(defun chara-image-load ()
  (load-images '(:player-l "me2.png")
	       '(:player-r "me.png")
	       '(:enemy-l "enemy2.png")
	       '(:enemy-r "enemy.png")
	       '(:demon-gate "demon-gate.png")
	       '(:enemy2-l "minienemy2.png")
	       '(:enemy2-r "minienemy1.png")
	       '(:big "big.png")
	       '(:big-l "big-l.png")))

(defun bullet-image-load ()
  (load-images '(:knife-l "knife2-l.png")
	       '(:knife-r "knife2-r.png")
	       '(:javelin-l "javelin-l.png")
	       '(:javelin-r "javelin-r.png")
	       '(:explosion "explosion.png")
	       '(:ebul "ebul.png"))
  (load-animations '(:boomerang-l "boomerang-l_ani.png"
		     24 24 96 24)
		   '(:boomerang-r "boomerang-r_ani.png"
		     24 24 96 24)
		   '(:axe-l "axe-l_ani.png"
		     24 24 96 24)
		   '(:axe-r "axe-r_ani.png"
		     24 24 96 24)
		   '(:bomb-l "bomb-l_ani.png"
		     24 24 96 24)
		   '(:bomb-r "bomb-r_ani.png"
		     24 24 96 24)))

(defun item-image-load ()
  (load-images '(:coin "coin.png")
	       '(:dash-up "dash_up.png")
	       '(:jump-up "jump_up.png")
	       '(:goal "goal.png")
	       '(:knife-item "knife_item.png")
	       '(:axe-item "axe_item.png")
	       '(:boomerang-item "boomerang_item.png")
	       '(:javelin-item "javelin_item.png")
	       '(:bomb-item "bomb_item.png")))

(defun icon-image-load ()
  (load-images       '(:knife-icon "knife_icon.png")
		     '(:axe-icon "axe_icon.png")
		     '(:boomerang-icon "boomerang_icon.png")
		     '(:javelin-icon "javelin_icon.png")
		     '(:bomb-icon "bomb_icon.png")))

(defun gameimage-load ()
  (mapchip-image-load)
  (chara-image-load)
  (bullet-image-load)
  (item-image-load)
  (icon-image-load)
  (load-images '(:title "title_touka.png")
	       '(:back-pillar "matsumura_hashira06.png")))
