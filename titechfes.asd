;;;; titechfes.asd

(asdf:defsystem #:titechfes
  :description "Describe titechfes here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :components ((:file "package")
	       (:file "macro")
	       (:file "global")
	       (:file "loadimage")
	       (:file "loadfile")
	       (:file "key")
	       (:file "game")
	       (:file "gameobject")
	       (:file "bullet")
	       (:file "enemy")
	       (:file "player")
	       (:file "item")
	       (:file "map")
	       (:file "camera")
               (:file "titechfes"))
  :depends-on (:lispbuilder-sdl :split-sequence :iterate :alexandria))

