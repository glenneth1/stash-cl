;;;; build.lisp --- Build script for stash-cl

(require :asdf)

(format t "~%Building stash-cl executable...~%~%")

;; Load quicklisp if available
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Load the system
(format t "Loading stash-cl system...~%")
(asdf:load-system :stash-cl)

;; Build the executable
(format t "~%Creating executable...~%")
(asdf:make :stash-cl)

(format t "~%Build complete! Executable: ./stash~%~%")
(uiop:quit 0)
