;;;; package-mgmt.lisp --- Package management for stash-cl

(in-package #:stash-cl/package-mgmt)

(defstruct package-info
  "Information about a package."
  name
  path
  target
  ignore-patterns)

(defun read-ignore-patterns (package-path)
  "Read ignore patterns from .stash-local-ignore and .stash-global-ignore files."
  (let ((patterns nil)
        (local-ignore (concatenate 'string package-path "/.stash-local-ignore"))
        (global-ignore (concatenate 'string package-path "/../.stash-global-ignore")))
    
    ;; Read local ignore file
    (when (probe-file local-ignore)
      (with-open-file (stream local-ignore)
        (loop for line = (read-line stream nil)
              while line
              unless (or (string= line "")
                        (char= (char line 0) #\#))
              do (push line patterns))))
    
    ;; Read global ignore file
    (when (probe-file global-ignore)
      (with-open-file (stream global-ignore)
        (loop for line = (read-line stream nil)
              while line
              unless (or (string= line "")
                        (char= (char line 0) #\#))
              do (push line patterns))))
    
    ;; Add default patterns
    (push "^\\.git$" patterns)
    (push "^\\.gitignore$" patterns)
    (push "^README" patterns)
    (push "^LICENSE" patterns)
    (push "^COPYING" patterns)
    (push "^\\.stash-local-ignore$" patterns)
    
    (nreverse patterns)))

(defun should-ignore-p (filename patterns)
  "Check if FILENAME matches any of the ignore PATTERNS."
  (some (lambda (pattern)
          (cl-ppcre:scan pattern filename))
        patterns))
