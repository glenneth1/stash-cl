;;;; paths.lisp --- Path manipulation utilities for stash-cl

(in-package #:stash-cl/paths)

(defun expand-home (path)
  "Expand ~ in PATH to the user's home directory."
  (if (and (> (length path) 0)
           (char= (char path 0) #\~))
      (concatenate 'string
                   (namestring (user-homedir-pathname))
                   (subseq path 1))
      path))

(defun concat-path (&rest components)
  "Concatenate path COMPONENTS with proper separators."
  (format nil "~{~A~^/~}" 
          (remove-if (lambda (s) (or (null s) (string= s "")))
                     components)))

(defun ensure-config-path (path)
  "Ensure the directory for PATH exists."
  (let ((dir (directory-namestring path)))
    (ensure-directories-exist dir)))

(defun canonicalize-path (path)
  "Canonicalize PATH by expanding ~ and resolving to absolute path."
  (namestring (truename (expand-home path))))
