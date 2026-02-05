;;;; file-ops.lisp --- File operations for stash-cl

(in-package #:stash-cl/file-ops)

;;; CLOS File Operation Protocol

(defclass file-operation ()
  ((path :initarg :path
         :accessor operation-path
         :type string
         :documentation "Primary path for the operation"))
  (:documentation "Base class for file system operations."))

(defgeneric execute-operation (operation)
  (:documentation "Execute a file system OPERATION."))

(defgeneric operation-description (operation)
  (:documentation "Return a human-readable description of OPERATION."))

;;; Symlink Operation

(defclass symlink-operation (file-operation)
  ((source :initarg :source
           :accessor operation-source
           :type string
           :documentation "Source path the symlink points to"))
  (:documentation "Operation to create a symbolic link."))

(defmethod execute-operation ((op symlink-operation))
  "Create a symbolic link."
  (create-symlink (operation-path op) (operation-source op)))

(defmethod operation-description ((op symlink-operation))
  (format nil "CREATE-SYMLINK ~A -> ~A" (operation-path op) (operation-source op)))

;;; Mkdir Operation

(defclass mkdir-operation (file-operation)
  ()
  (:documentation "Operation to create a directory."))

(defmethod execute-operation ((op mkdir-operation))
  "Create a directory."
  (mkdir-p (operation-path op)))

(defmethod operation-description ((op mkdir-operation))
  (format nil "MKDIR ~A" (operation-path op)))

;;; Move Operation

(defclass move-operation (file-operation)
  ((destination :initarg :destination
                :accessor operation-destination
                :type string
                :documentation "Destination path"))
  (:documentation "Operation to move a file."))

(defmethod execute-operation ((op move-operation))
  "Move a file."
  (move-source-to-target (operation-path op) (operation-destination op)))

(defmethod operation-description ((op move-operation))
  (format nil "MOVE ~A -> ~A" (operation-path op) (operation-destination op)))

;;; Delete Operation

(defclass delete-operation (file-operation)
  ()
  (:documentation "Operation to delete a file or directory."))

(defmethod execute-operation ((op delete-operation))
  "Delete a file or directory."
  (let ((path (operation-path op)))
    (if (file-is-directory-p path)
        (delete-directory path)
        (when (probe-file path)
          (delete-file path)))))

(defmethod operation-description ((op delete-operation))
  (format nil "DELETE ~A" (operation-path op)))

;;; File type predicates

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

;;; Low-level operations (used by CLOS methods and backward compat)

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
  ;; Use rm -rf to handle directories with symlinks
  (uiop:run-program (list "rm" "-rf" path)))

(defun move-source-to-target (source target)
  "Move SOURCE to TARGET."
  (log-action "MOVE" source target)
  (ensure-directories-exist target)
  (rename-file source target))

;;; Backward compatible function (accepts plist-style operations)

(defun execute-operations (operations)
  "Execute a list of OPERATIONS.
Accepts both CLOS operation objects and legacy plist-style operations."
  (dolist (op operations)
    (if (typep op 'file-operation)
        ;; CLOS operation object
        (execute-operation op)
        ;; Legacy plist-style operation
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
               (delete-file (second op))))))))
