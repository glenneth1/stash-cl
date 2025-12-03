;;;; file-ops.lisp --- File operations for stash-cl

(in-package #:stash-cl/file-ops)

(defun file-is-symlink-p (path)
  "Check if PATH is a symbolic link."
  #+osicat
  (eq (osicat:file-kind path :follow-symlinks nil) :symbolic-link)
  #-osicat
  (handler-case
      (zerop (nth-value 2 (uiop:run-program (list "test" "-L" path)
                                            :ignore-error-status t)))
    (error () nil)))

(defun file-is-directory-p (path)
  "Check if PATH is a directory."
  (uiop:directory-exists-p path))

(defun file-is-regular-p (path)
  "Check if PATH is a regular file."
  (and (probe-file path)
       (not (file-is-symlink-p path))
       (not (file-is-directory-p path))))

(defun mkdir-p (path)
  "Create directory at PATH, including parent directories."
  (ensure-directories-exist 
   (if (uiop:directory-pathname-p path)
       path
       (concatenate 'string path "/"))))

(defun create-symlink (target source)
  "Create a symbolic link from TARGET to SOURCE."
  (log-action "CREATE-SYMLINK" target source)
  #+osicat
  (osicat:make-link target :target source)
  #-osicat
  (uiop:run-program (list "ln" "-s" source target)))

(defun delete-directory (path)
  "Delete directory at PATH."
  (log-action "DELETE-DIR" path)
  (uiop:delete-directory-tree (pathname path) :validate t))

(defun move-source-to-target (source target)
  "Move SOURCE to TARGET."
  (log-action "MOVE" source target)
  (ensure-directories-exist target)
  (rename-file source target))

(defun execute-operations (operations)
  "Execute a list of OPERATIONS."
  (dolist (op operations)
    (case (car op)
      (:create-symlink
       (create-symlink (second op) (third op)))
      (:mkdir
       (mkdir-p (second op)))
      (:move
       (move-source-to-target (second op) (third op)))
      (:delete
       (if (file-is-directory-p (second op))
           (delete-directory (second op))
           (delete-file (second op)))))))
