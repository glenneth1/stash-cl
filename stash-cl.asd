;;;; stash-cl.asd --- ASDF system definition for stash-cl

(asdf:defsystem #:stash-cl
  :description "GNU Stow replacement written in Common Lisp - manage dotfiles and symlinks"
  :author "Glenn <glenn@example.com>"
  :license "GPL-3.0"
  :version "0.2.0"
  :serial t
  :depends-on (#:uiop
               #:unix-opts
               #:cl-ppcre
               #:alexandria
               #:local-time)
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "colors")
                 (:file "log")
                 (:file "paths")
                 (:file "conflict")
                 (:file "file-ops")
                 (:file "package-mgmt")
                 (:file "tree")
                 (:file "task-planner")
                 (:file "folding")
                 (:file "help")
                 (:file "main"))))
  :build-operation "program-op"
  :build-pathname "stash"
  :entry-point "stash-cl:main")
