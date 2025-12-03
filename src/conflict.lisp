;;;; conflict.lisp --- Conflict handling for stash-cl

(in-package #:stash-cl/conflict)

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

(defun handle-conflict (source target)
  "Handle conflict between SOURCE and TARGET.
Returns action to take: :skip, :overwrite, :backup, or :abort."
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
