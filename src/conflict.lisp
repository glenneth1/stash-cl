;;;; conflict.lisp --- Conflict handling for stash-cl

(in-package #:stash-cl/conflict)

;;; CLOS Conflict Resolver Protocol

(defclass conflict-resolver ()
  ()
  (:documentation "Base class for conflict resolution strategies."))

(defgeneric resolve-conflict (resolver source target)
  (:documentation "Resolve a conflict between SOURCE and TARGET.
Returns action to take: :skip, :overwrite, :backup, or :abort."))

;;; Interactive Resolver (prompts user)

(defclass interactive-resolver (conflict-resolver)
  ()
  (:documentation "Resolver that prompts the user for action."))

(defmethod resolve-conflict ((resolver interactive-resolver) source target)
  "Prompt user to resolve conflict."
  (log-action "CONFLICT" source target)
  (format t "~%~A~%" (stash-cl/colors:color-yellow "CONFLICT DETECTED"))
  (format t "Target already exists: ~A~%" target)
  (format t "Source: ~A~%" source)
  (prompt-user-for-action
   "What would you like to do?"
   :skip
   :overwrite
   :backup
   :abort))

;;; Auto-Skip Resolver (non-interactive)

(defclass auto-skip-resolver (conflict-resolver)
  ()
  (:documentation "Resolver that automatically skips conflicts."))

(defmethod resolve-conflict ((resolver auto-skip-resolver) source target)
  "Automatically skip conflicts."
  (log-action "CONFLICT-SKIP" source target)
  :skip)

;;; Auto-Overwrite Resolver (non-interactive)

(defclass auto-overwrite-resolver (conflict-resolver)
  ()
  (:documentation "Resolver that automatically overwrites on conflicts."))

(defmethod resolve-conflict ((resolver auto-overwrite-resolver) source target)
  "Automatically overwrite on conflicts."
  (log-action "CONFLICT-OVERWRITE" source target)
  :overwrite)

;;; Auto-Abort Resolver (non-interactive)

(defclass auto-abort-resolver (conflict-resolver)
  ()
  (:documentation "Resolver that automatically aborts on conflicts."))

(defmethod resolve-conflict ((resolver auto-abort-resolver) source target)
  "Automatically abort on conflicts."
  (log-action "CONFLICT-ABORT" source target)
  :abort)

;;; Global resolver instance

(defparameter *conflict-resolver* (make-instance 'interactive-resolver)
  "Current conflict resolver. Default is interactive.")

;;; Helper function for prompting

(defun prompt-user-for-action (message &rest options)
  "Prompt user with MESSAGE and OPTIONS, return selected option."
  (format t "~A~%" message)
  (loop for i from 1
        for option in options
        do (format t "  ~D. ~A~%" i option))
  (format t "Choice: ")
  (force-output)
  (let ((choice (read)))
    (if (and (integerp choice)
             (>= choice 1)
             (<= choice (length options)))
        (nth (1- choice) options)
        (progn
          (format t "Invalid choice. Please try again.~%")
          (apply #'prompt-user-for-action message options)))))

;;; Backward compatible function

(defun handle-conflict (source target)
  "Handle conflict between SOURCE and TARGET.
Returns action to take: :skip, :overwrite, :backup, or :abort."
  (resolve-conflict *conflict-resolver* source target))
