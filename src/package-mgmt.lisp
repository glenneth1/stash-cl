;;;; package-mgmt.lisp --- Package management for stash-cl

(in-package #:stash-cl/package-mgmt)

;;; CLOS Stash Package Class

(defclass stash-package ()
  ((name :initarg :name
         :accessor stash-package-name
         :type string
         :documentation "Name of the package")
   (path :initarg :path
         :accessor stash-package-path
         :type string
         :documentation "Path to the package directory")
   (target :initarg :target
           :accessor stash-package-target
           :type string
           :documentation "Target directory for stashing")
   (ignore-patterns :initarg :ignore-patterns
                    :initform nil
                    :accessor stash-package-ignore-patterns
                    :type list
                    :documentation "List of ignore patterns for this package"))
  (:documentation "Represents a stash package with its configuration."))

;;; Generic functions for stash-package

(defgeneric load-ignore-patterns (package)
  (:documentation "Load ignore patterns for PACKAGE from config files."))

(defmethod load-ignore-patterns ((pkg stash-package))
  "Load ignore patterns from .stash-local-ignore and .stash-global-ignore."
  (setf (stash-package-ignore-patterns pkg)
        (read-ignore-patterns (stash-package-path pkg))))

;;; Backward compatibility - package-info struct accessors

(defun make-package-info (&key name path target ignore-patterns)
  "Create a stash-package instance (backward compatible constructor)."
  (make-instance 'stash-package
                 :name name
                 :path path
                 :target target
                 :ignore-patterns ignore-patterns))

(defun package-info-p (obj)
  "Check if OBJ is a stash-package (backward compatible predicate)."
  (typep obj 'stash-package))

(defun package-info-name (pkg)
  "Backward compatible accessor for name."
  (stash-package-name pkg))

(defun package-info-path (pkg)
  "Backward compatible accessor for path."
  (stash-package-path pkg))

(defun package-info-target (pkg)
  "Backward compatible accessor for target."
  (stash-package-target pkg))

(defun package-info-ignore-patterns (pkg)
  "Backward compatible accessor for ignore-patterns."
  (stash-package-ignore-patterns pkg))

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

(defun should-ignore-with-overrides-p (filename patterns override-patterns)
  "Check if FILENAME should be ignored, considering override patterns.
Returns T if the file matches an ignore pattern AND does not match any override pattern.
Override patterns force-include files that would otherwise be ignored."
  (and (should-ignore-p filename patterns)
       (not (some (lambda (override)
                    (let ((regex-pattern (if (or (char= (char override 0) #\^)
                                                 (find #\\ override))
                                             override
                                             (glob-to-regex override))))
                      (cl-ppcre:scan regex-pattern filename)))
                  override-patterns))))
