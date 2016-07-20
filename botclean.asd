(asdf:defsystem :botclean
  :depends-on (:fset :yason)
  :components ((:file "src/packages")
	       (:file "src/field")
	       (:file "src/game")
               ))

