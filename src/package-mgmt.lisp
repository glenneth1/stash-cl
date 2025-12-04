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
        (global-ignore (merge-pathnames ".stash-global-ignore" (user-homedir-pathname))))
    
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
    
    ;; Add default patterns (don't ignore .stash-local-ignore - we want to stash it)
    (push "^\\.git$" patterns)
    (push "^\\.gitignore$" patterns)
    (push "^README" patterns)
    (push "^LICENSE" patterns)
    (push "^COPYING" patterns)
    
    (nreverse patterns)))

(defun glob-to-regex (glob)
  "Convert a glob pattern to a regex pattern.
Supports: * (any chars), ? (single char), simple patterns.
Patterns ending with /* match the directory name before the slash."
  (let* ((ends-with-slash-star (and (>= (length glob) 2)
                                     (string= (subseq glob (- (length glob) 2)) "/*")))
         (pattern-to-convert (if ends-with-slash-star
                                  (subseq glob 0 (- (length glob) 2))  ; Remove /*
                                  glob))
         (regex (with-output-to-string (s)
                  (loop for char across pattern-to-convert
                        do (case char
                             (#\* (write-string ".*" s))
                             (#\? (write-string "." s))
                             (#\. (write-string "\\." s))
                             (#\+ (write-string "\\+" s))
                             (#\[ (write-string "\\[" s))
                             (#\] (write-string "\\]" s))
                             (#\( (write-string "\\(" s))
                             (#\) (write-string "\\)" s))
                             (#\{ (write-string "\\{" s))
                             (#\} (write-string "\\}" s))
                             (#\^ (write-string "\\^" s))
                             (#\$ (write-string "\\$" s))
                             (t (write-char char s)))))))
    ;; Anchor the pattern to match the whole filename
    (concatenate 'string "^" regex "$")))

(defun should-ignore-p (filename patterns)
  "Check if FILENAME matches any of the ignore PATTERNS (glob or regex format)."
  (some (lambda (pattern)
          ;; If pattern starts with ^ or contains regex chars, treat as regex
          ;; Otherwise treat as glob
          (let ((regex-pattern (if (or (char= (char pattern 0) #\^)
                                       (find #\\ pattern))
                                   pattern  ; Already regex
                                   (glob-to-regex pattern))))  ; Convert glob to regex
            (cl-ppcre:scan regex-pattern filename)))
        patterns))
