;;;; game-of-life.asd

(asdf:defsystem #:game-of-life
  :description "SImple lisp implementation of Conway's game of life."
  :author "Mark Gerarts <mark.gerarts@gmail.com>"
  :license "GPLv3"
  :serial t
  :depends-on (#:croatoan)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "game-of-life")
                             (:file "ui")))))
