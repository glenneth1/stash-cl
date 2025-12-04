;;;; build.lisp --- Build script for stash-cl

(require :asdf)

(format t "~%Building stash-cl executable with maximum compression...~%~%")

;; Load quicklisp if available
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Set SBCL optimization settings for maximum compression
(format t "Configuring SBCL for maximum compression...~%")
(declaim (optimize (speed 2) (space 3) (debug 0) (safety 1) (compilation-speed 0)))

;; Muffle warnings
#+sbcl
(setf sb-ext:*muffled-warnings* 'warning)

;; Load the system
(format t "Loading stash-cl system...~%")
(asdf:load-system :stash-cl)

;; Build the executable with compression
(format t "~%Creating compressed executable...~%")

#+sbcl
(sb-ext:save-lisp-and-die "stash.bin"
                          :toplevel #'stash-cl:toplevel-entry
                          :executable t
                          :compression 9
                          :save-runtime-options nil
                          :purify t)

#-sbcl
(asdf:make :stash-cl)
