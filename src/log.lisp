;;;; log.lisp --- Logging utilities for stash-cl

(in-package #:stash-cl/log)

(defparameter *log-file* "~/.local/state/stash/stash.log"
  "Path to the log file.")

(defun current-timestamp ()
  "Return current timestamp as a string."
  (multiple-value-bind (sec min hour day month year)
      (get-decoded-time)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour min sec)))

(defun log-action (action &rest args)
  "Log an ACTION with ARGS to the log file."
  (let ((log-path (stash-cl/paths:expand-home *log-file*)))
    (ensure-directories-exist log-path)
    (with-open-file (stream log-path
                            :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)
      (format stream "[~A] ~A~{ ~A~}~%"
              (current-timestamp)
              action
              args))))
