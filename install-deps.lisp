;;;; install-deps.lisp --- Install dependencies for stash-cl

(format t "~%Installing stash-cl dependencies...~%~%")

(ql:quickload :osicat)
(ql:quickload :unix-opts)
(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)
(ql:quickload :local-time)

(format t "~%All dependencies installed!~%~%")
(uiop:quit 0)
