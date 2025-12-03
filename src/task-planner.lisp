;;;; task-planner.lisp --- Task planning and validation system for stash-cl

(in-package #:stash-cl/task-planner)

;;; Task structure

(defstruct task
  "Represents a single filesystem operation to be performed."
  (action nil :type keyword)     ; :create, :remove, :move
  (type nil :type keyword)       ; :link, :dir, :file
  (path "" :type string)         ; Target path
  (source nil :type (or null string))  ; Source path (for links and moves)
  (dest nil :type (or null string)))   ; Destination (for moves)

;;; Task planner state

(defstruct planner-state
  "State for the task planning system."
  (tasks nil :type list)                    ; List of tasks to execute
  (conflicts nil :type list)                ; List of detected conflicts
  (dir-task-for (make-hash-table :test 'equal) :type hash-table)   ; Path -> dir task
  (link-task-for (make-hash-table :test 'equal) :type hash-table)  ; Path -> link task
  (stash-path "" :type string)              ; Relative path to stash directory
  (target-dir "" :type string))             ; Absolute path to target directory

(defparameter *planner-state* nil
  "Current planner state.")

;;; Initialization

(defun init-planner (stash-path target-dir)
  "Initialize the task planner with STASH-PATH and TARGET-DIR."
  (setf *planner-state*
        (make-planner-state
         :stash-path stash-path
         :target-dir target-dir)))

(defun reset-planner ()
  "Reset the planner state."
  (when *planner-state*
    (setf (planner-state-tasks *planner-state*) nil)
    (setf (planner-state-conflicts *planner-state*) nil)
    (clrhash (planner-state-dir-task-for *planner-state*))
    (clrhash (planner-state-link-task-for *planner-state*))))

;;; Task creation

(defun add-task (action type path &key source dest)
  "Add a task to the task queue."
  (unless *planner-state*
    (error "Planner not initialized. Call init-planner first."))
  
  (let ((task (make-task :action action
                        :type type
                        :path path
                        :source source
                        :dest dest)))
    (push task (planner-state-tasks *planner-state*))
    
    ;; Track directory and link tasks
    (case type
      (:dir
       (setf (gethash path (planner-state-dir-task-for *planner-state*)) task))
      (:link
       (setf (gethash path (planner-state-link-task-for *planner-state*)) task)))
    
    task))

(defun find-dir-task (path)
  "Find directory task for PATH."
  (when *planner-state*
    (gethash path (planner-state-dir-task-for *planner-state*))))

(defun find-link-task (path)
  "Find link task for PATH."
  (when *planner-state*
    (gethash path (planner-state-link-task-for *planner-state*))))

;;; Conflict detection

(defun add-conflict (package message &optional path)
  "Add a conflict to the conflict list."
  (unless *planner-state*
    (error "Planner not initialized."))
  
  (let ((conflict (list :package package
                       :message message
                       :path path)))
    (push conflict (planner-state-conflicts *planner-state*))))

(defun has-conflicts-p ()
  "Check if there are any conflicts."
  (and *planner-state*
       (planner-state-conflicts *planner-state*)))

(defun get-conflicts ()
  "Get list of all conflicts."
  (when *planner-state*
    (reverse (planner-state-conflicts *planner-state*))))

(defun get-conflict-count ()
  "Get number of conflicts."
  (if *planner-state*
      (length (planner-state-conflicts *planner-state*))
      0))

;;; Task validation

(defun validate-task (task)
  "Validate a single TASK. Returns T if valid, NIL if invalid."
  (case (task-action task)
    (:create
     (case (task-type task)
       (:link
        ;; Link creation requires source
        (and (task-source task)
             (not (string= (task-source task) ""))))
       (:dir
        ;; Directory creation just needs path
        t)
       (otherwise nil)))
    (:remove
     ;; Removal just needs path
     t)
    (:move
     ;; Move requires both source and dest
     (and (task-source task)
          (task-dest task)))
    (otherwise nil)))

(defun validate-all-tasks ()
  "Validate all tasks in the queue. Returns T if all valid, NIL otherwise."
  (unless *planner-state*
    (return-from validate-all-tasks nil))
  
  (every #'validate-task (planner-state-tasks *planner-state*)))

;;; Task execution

(defun execute-task (task &key simulate)
  "Execute a single TASK. If SIMULATE is T, only print what would be done."
  (let ((action-str (format nil "~A ~A ~A"
                           (task-action task)
                           (task-type task)
                           (task-path task))))
    
    (if simulate
        (format t "SIMULATE: ~A~%" action-str)
        (progn
          (format t "EXECUTE: ~A~%" action-str)
          (case (task-action task)
            (:create
             (case (task-type task)
               (:link
                (stash-cl/file-ops:create-symlink (task-path task) (task-source task)))
               (:dir
                (stash-cl/file-ops:mkdir-p (task-path task)))))
            (:remove
             (case (task-type task)
               (:link
                (when (probe-file (task-path task))
                  (delete-file (task-path task))))
               (:dir
                (when (uiop:directory-exists-p (task-path task))
                  (stash-cl/file-ops:delete-directory (task-path task))))))
            (:move
             (when (probe-file (task-source task))
               (rename-file (task-source task) (task-dest task)))))))))

(defun execute-all-tasks (&key simulate)
  "Execute all tasks in the queue. If SIMULATE is T, only show what would be done."
  (unless *planner-state*
    (error "Planner not initialized."))
  
  ;; Check for conflicts first
  (when (has-conflicts-p)
    (format t "~%ERROR: Cannot execute tasks due to conflicts:~%")
    (dolist (conflict (get-conflicts))
      (format t "  - Package ~A: ~A~@[ (~A)~]~%"
              (getf conflict :package)
              (getf conflict :message)
              (getf conflict :path)))
    (return-from execute-all-tasks nil))
  
  ;; Validate all tasks
  (unless (validate-all-tasks)
    (format t "~%ERROR: Task validation failed~%")
    (return-from execute-all-tasks nil))
  
  ;; Execute tasks in reverse order (LIFO - last added, first executed)
  (let ((tasks (reverse (planner-state-tasks *planner-state*))))
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
    (reverse (planner-state-tasks *planner-state*))))

(defun get-task-count ()
  "Get number of tasks."
  (if *planner-state*
      (length (planner-state-tasks *planner-state*))
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

(defun plan-create-link (path source)
  "Plan creation of a symlink from PATH to SOURCE."
  (add-task :create :link path :source source))

(defun plan-remove-link (path)
  "Plan removal of a symlink at PATH."
  (add-task :remove :link path))

(defun plan-create-dir (path)
  "Plan creation of a directory at PATH."
  (add-task :create :dir path))

(defun plan-remove-dir (path)
  "Plan removal of a directory at PATH."
  (add-task :remove :dir path))

(defun plan-move-file (source dest)
  "Plan moving a file from SOURCE to DEST."
  (add-task :move :file source :source source :dest dest))
