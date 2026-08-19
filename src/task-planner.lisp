;;;; task-planner.lisp --- Task planning and validation system for stash-cl

(in-package #:stash-cl/task-planner)

;;; Conflict error condition

(define-condition conflict-error (error)
  ((message :initarg :message :reader conflict-error-message)
   (path :initarg :path :reader conflict-error-path))
  (:report (lambda (condition stream)
             (format stream "Conflict detected: ~A"
                     (conflict-error-message condition)))))

;;; CLOS Task Class Hierarchy

(defclass task ()
  ((path :initarg :path
         :accessor task-path
         :type string
         :documentation "Target path for the operation"))
  (:documentation "Base class for all filesystem operations."))

(defclass create-task (task)
  ()
  (:documentation "Base class for creation operations."))

(defclass remove-task (task)
  ()
  (:documentation "Base class for removal operations."))

(defclass link-task (task)
  ((source :initarg :source
           :accessor task-source
           :type string
           :documentation "Source path the symlink points to"))
  (:documentation "Mixin for tasks involving symlinks."))

(defclass dir-task (task)
  ()
  (:documentation "Mixin for tasks involving directories."))

(defclass create-link-task (create-task link-task)
  ()
  (:documentation "Task to create a symbolic link."))

(defclass remove-link-task (remove-task link-task)
  ()
  (:documentation "Task to remove a symbolic link."))

(defclass create-dir-task (create-task dir-task)
  ()
  (:documentation "Task to create a directory."))

(defclass remove-dir-task (remove-task dir-task)
  ()
  (:documentation "Task to remove a directory."))

(defclass move-task (task)
  ((source :initarg :source
           :accessor task-source
           :type string
           :documentation "Source path to move from")
   (dest :initarg :dest
         :accessor task-dest
         :type string
         :documentation "Destination path to move to"))
  (:documentation "Task to move a file."))

;;; Generic functions for task operations

(defgeneric execute-task (task &key simulate)
  (:documentation "Execute a task. If SIMULATE is T, only print what would be done."))

(defgeneric validate-task (task)
  (:documentation "Validate a task. Returns T if valid, NIL if invalid."))

(defgeneric task-action (task)
  (:documentation "Return the action keyword for a task (for compatibility)."))

(defgeneric task-type (task)
  (:documentation "Return the type keyword for a task (for compatibility)."))

;;; CLOS Task Planner Class

(defclass task-planner ()
  ((tasks :initform nil
          :accessor planner-tasks
          :type list
          :documentation "List of tasks to execute")
   (conflicts :initform nil
              :accessor planner-conflicts
              :type list
              :documentation "List of detected conflicts")
   (dir-task-for :initform (make-hash-table :test 'equal)
                 :accessor planner-dir-task-for
                 :type hash-table
                 :documentation "Path -> dir task mapping")
   (link-task-for :initform (make-hash-table :test 'equal)
                  :accessor planner-link-task-for
                  :type hash-table
                  :documentation "Path -> link task mapping")
   (stash-path :initarg :stash-path
               :initform ""
               :accessor planner-stash-path
               :type string
               :documentation "Path to stash directory")
   (target-dir :initarg :target-dir
               :initform ""
               :accessor planner-target-dir
               :type string
               :documentation "Absolute path to target directory"))
  (:documentation "Task planning system for managing filesystem operations."))

(defparameter *planner-state* nil
  "Current planner instance.")

;;; Backward compatibility aliases for planner-state accessors
(defun planner-state-tasks (planner)
  "Backward compatible accessor for tasks."
  (planner-tasks planner))

(defun (setf planner-state-tasks) (value planner)
  "Backward compatible setter for tasks."
  (setf (planner-tasks planner) value))

(defun planner-state-conflicts (planner)
  "Backward compatible accessor for conflicts."
  (planner-conflicts planner))

(defun (setf planner-state-conflicts) (value planner)
  "Backward compatible setter for conflicts."
  (setf (planner-conflicts planner) value))

(defun planner-state-dir-task-for (planner)
  "Backward compatible accessor for dir-task-for."
  (planner-dir-task-for planner))

(defun planner-state-link-task-for (planner)
  "Backward compatible accessor for link-task-for."
  (planner-link-task-for planner))

;;; Initialization

(defun init-planner (stash-path target-dir)
  "Initialize the task planner with STASH-PATH and TARGET-DIR."
  (setf *planner-state*
        (make-instance 'task-planner
                       :stash-path stash-path
                       :target-dir target-dir)))

(defun reset-planner ()
  "Reset the planner state."
  (when *planner-state*
    (setf (planner-tasks *planner-state*) nil)
    (setf (planner-conflicts *planner-state*) nil)
    (clrhash (planner-dir-task-for *planner-state*))
    (clrhash (planner-link-task-for *planner-state*))))

;;; Task creation

(defun make-task-instance (action type path &key source dest)
  "Create a task instance based on ACTION and TYPE."
  (ecase action
    (:create
     (ecase type
       (:link (make-instance 'create-link-task :path path :source source))
       (:dir (make-instance 'create-dir-task :path path))))
    (:remove
     (ecase type
       (:link (make-instance 'remove-link-task :path path))
       (:dir (make-instance 'remove-dir-task :path path))))
    (:move
     (make-instance 'move-task :path path :source source :dest dest))))

(defun add-task (action type path &key source dest)
  "Add a task to the task queue."
  (unless *planner-state*
    (error "Planner not initialized. Call init-planner first."))
  
  (let ((task (make-task-instance action type path :source source :dest dest)))
    (push task (planner-tasks *planner-state*))
    
    ;; Track directory and link tasks
    (typecase task
      (dir-task
       (setf (gethash path (planner-dir-task-for *planner-state*)) task))
      (link-task
       (setf (gethash path (planner-link-task-for *planner-state*)) task)))
    
    task))

(defun find-dir-task (path)
  "Find directory task for PATH."
  (when *planner-state*
    (gethash path (planner-dir-task-for *planner-state*))))

(defun find-link-task (path)
  "Find link task for PATH."
  (when *planner-state*
    (gethash path (planner-link-task-for *planner-state*))))

;;; Conflict detection

(defun add-conflict (package message &optional path)
  "Add a conflict to the conflict list."
  (unless *planner-state*
    (error "Planner not initialized."))
  
  (let ((conflict (list :package package
                       :message message
                       :path path)))
    (push conflict (planner-conflicts *planner-state*))))

(defun has-conflicts-p ()
  "Check if there are any conflicts."
  (and *planner-state*
       (planner-conflicts *planner-state*)))

(defun get-conflicts ()
  "Get list of all conflicts."
  (when *planner-state*
    (reverse (planner-conflicts *planner-state*))))

(defun get-conflict-count ()
  "Get number of conflicts."
  (if *planner-state*
      (length (planner-conflicts *planner-state*))
      0))

;;; Task validation methods

(defmethod validate-task ((task create-link-task))
  "Validate a create-link task. Requires source."
  (and (slot-boundp task 'source)
       (task-source task)
       (not (string= (task-source task) ""))))

(defmethod validate-task ((task create-dir-task))
  "Validate a create-dir task. Just needs path."
  t)

(defmethod validate-task ((task remove-task))
  "Validate a remove task. Just needs path."
  t)

(defmethod validate-task ((task move-task))
  "Validate a move task. Requires source and dest."
  (and (slot-boundp task 'source)
       (slot-boundp task 'dest)
       (task-source task)
       (task-dest task)))

(defun validate-all-tasks ()
  "Validate all tasks in the queue. Returns T if all valid, NIL otherwise."
  (unless *planner-state*
    (return-from validate-all-tasks nil))
  
  (every #'validate-task (planner-tasks *planner-state*)))

;;; Task action/type methods for compatibility

(defmethod task-action ((task create-task)) :create)
(defmethod task-action ((task remove-task)) :remove)
(defmethod task-action ((task move-task)) :move)

(defmethod task-type ((task link-task)) :link)
(defmethod task-type ((task dir-task)) :dir)
(defmethod task-type ((task move-task)) :file)

;;; Task execution methods

(defmethod execute-task :around ((task task) &key simulate)
  "Around method to handle simulation mode and logging."
  (let ((action-str (format nil "~A ~A ~A"
                           (task-action task)
                           (task-type task)
                           (task-path task))))
    (if simulate
        (format t "SIMULATE: ~A~%" action-str)
        (progn
          (format t "EXECUTE: ~A~%" action-str)
          (call-next-method)))))

(defmethod execute-task ((task create-link-task) &key simulate)
  "Execute a create-link task."
  (declare (ignore simulate))
  (stash-cl/file-ops:create-symlink (task-path task) (task-source task)))

(defmethod execute-task ((task create-dir-task) &key simulate)
  "Execute a create-dir task."
  (declare (ignore simulate))
  (stash-cl/file-ops:mkdir-p (task-path task)))

(defmethod execute-task ((task remove-link-task) &key simulate)
  "Execute a remove-link task."
  (declare (ignore simulate))
  (when (probe-file (task-path task))
    (delete-file (task-path task))))

(defmethod execute-task ((task remove-dir-task) &key simulate)
  "Execute a remove-dir task."
  (declare (ignore simulate))
  (when (uiop:directory-exists-p (task-path task))
    (stash-cl/file-ops:delete-directory (task-path task))))

(defmethod execute-task ((task move-task) &key simulate)
  "Execute a move task."
  (declare (ignore simulate))
  (when (probe-file (task-source task))
    (rename-file (task-source task) (task-dest task))))

(defun execute-all-tasks (&key simulate)
  "Execute all tasks in the queue. If SIMULATE is T, only show what would be done."
  (unless *planner-state*
    (error "Planner not initialized."))
  
  ;; Check for conflicts first
  (when (has-conflicts-p)
    (if simulate
        ;; In simulation mode, just report conflicts but continue
        (progn
          (format t "~%CONFLICTS DETECTED (simulation mode):~%")
          (dolist (conflict (get-conflicts))
            (format t "  - Package ~A: ~A~@[ (~A)~]~%"
                    (getf conflict :package)
                    (getf conflict :message)
                    (getf conflict :path))))
        ;; In normal mode, error out
        (progn
          (format t "~%ERROR: Cannot execute tasks due to conflicts:~%")
          (dolist (conflict (get-conflicts))
            (format t "  - Package ~A: ~A~@[ (~A)~]~%"
                    (getf conflict :package)
                    (getf conflict :message)
                    (getf conflict :path)))
          (return-from execute-all-tasks nil))))
  
  ;; Validate all tasks
  (unless (validate-all-tasks)
    (format t "~%ERROR: Task validation failed~%")
    (return-from execute-all-tasks nil))
  
  ;; Execute tasks in reverse order (LIFO - last added, first executed)
  (let ((tasks (reverse (planner-tasks *planner-state*))))
    (if simulate
        (format t "~%SIMULATION MODE - No changes will be made~%~%")
        (format t "~%Executing ~D task(s)...~%~%" (length tasks)))
    
    (dolist (task tasks)
      (execute-task task :simulate simulate))
    
    (if simulate
        (format t "~%Simulation complete. ~D operation(s) would be performed.~%"
                (length tasks))
        (format t "~%Successfully executed ~D task(s).~%" (length tasks)))
    
    t))

;;; Task inspection

(defun get-tasks ()
  "Get list of all tasks."
  (when *planner-state*
    (reverse (planner-tasks *planner-state*))))

(defun get-task-count ()
  "Get number of tasks."
  (if *planner-state*
      (length (planner-tasks *planner-state*))
      0))

(defun print-task-summary ()
  "Print a summary of planned tasks."
  (unless *planner-state*
    (format t "No planner state.~%")
    (return-from print-task-summary))
  
  (let ((tasks (get-tasks))
        (conflicts (get-conflicts)))
    (format t "~%Task Summary:~%")
    (format t "  Total tasks: ~D~%" (length tasks))
    (format t "  Conflicts: ~D~%" (length conflicts))
    
    (when tasks
      (format t "~%Planned operations:~%")
      (dolist (task tasks)
        (format t "  ~A ~A: ~A~@[ -> ~A~]~%"
                (task-action task)
                (task-type task)
                (task-path task)
                (task-source task))))
    
    (when conflicts
      (format t "~%Conflicts detected:~%")
      (dolist (conflict conflicts)
        (format t "  Package ~A: ~A~@[ (~A)~]~%"
                (getf conflict :package)
                (getf conflict :message)
                (getf conflict :path))))))

;;; Helper functions for common task patterns

(defun check-path-conflict (path source)
  "Check if PATH has a conflict. Returns NIL if no conflict, error message if conflict."
  (cond
    ;; Path doesn't exist - no conflict
    ((not (probe-file path))
     nil)
    
    ;; Path is a symlink
    ((stash-cl/file-ops:file-is-symlink-p path)
     (let ((link-target (stash-cl/file-ops:read-symlink path)))
       (cond
         ;; Symlink points to same source - no conflict (idempotent)
         ((string= (namestring (truename link-target))
                  (namestring (truename source)))
          nil)
         
         ;; Symlink points to different location - conflict
         (t
          (format nil "Symlink already exists pointing to different location: ~A -> ~A (want: ~A)"
                  path link-target source)))))
    
    ;; Path is a regular file - conflict
    ((stash-cl/file-ops:file-is-regular-p path)
     (format nil "Regular file already exists at: ~A" path))
    
    ;; Path is a directory - conflict
    ((stash-cl/file-ops:file-is-directory-p path)
     (format nil "Directory already exists at: ~A" path))
    
    ;; Unknown type - conflict
    (t
     (format nil "Path already exists (unknown type): ~A" path))))

(defun plan-create-link (path source &key (check-conflicts t))
  "Plan creation of a symlink from PATH to SOURCE. 
If CHECK-CONFLICTS is T (default), checks for conflicts first.
Set CHECK-CONFLICTS to NIL when unfolding to preserve original content."
  (if check-conflicts
      (let ((conflict (check-path-conflict path source)))
        (cond
          ;; Conflict detected - add to conflict list and signal error
          (conflict
           (add-conflict "stash" conflict path)
           (error 'conflict-error :message conflict :path path))
          
          ;; Path already exists and points to same source - skip (idempotent)
          ((and (probe-file path)
                (stash-cl/file-ops:file-is-symlink-p path))
           ;; Symlink already correct, nothing to do
           nil)
          
          ;; Path doesn't exist - create it
          (t
           (add-task :create :link path :source source))))
      ;; No conflict checking - just create the link
      (add-task :create :link path :source source)))

(defun plan-remove-link (path)
  "Plan removal of a symlink at PATH."
  (add-task :remove :link path))

(defun plan-create-dir (path)
  "Plan creation of a directory at PATH."
  ;; If a symlink exists at this path, remove it first
  (when (and (probe-file path)
             (stash-cl/file-ops:file-is-symlink-p path))
    (plan-remove-link path))
  (add-task :create :dir path))

(defun plan-remove-dir (path)
  "Plan removal of a directory at PATH."
  (add-task :remove :dir path))

(defun plan-move-file (source dest)
  "Plan moving a file from SOURCE to DEST."
  (add-task :move :file source :source source :dest dest))
