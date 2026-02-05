;;;; log.lisp --- Logging utilities for stash-cl

(in-package #:stash-cl/log)

;;; CLOS Logger Class

(defclass logger ()
  ((log-file :initarg :log-file
             :accessor logger-log-file
             :type string
             :documentation "Path to the log file")
   (enabled :initarg :enabled
            :initform t
            :accessor logger-enabled
            :type boolean
            :documentation "Whether logging is enabled"))
  (:documentation "Logger for recording stash operations."))

;;; Generic functions

(defgeneric log-message (logger action &rest args)
  (:documentation "Log a message with ACTION and ARGS to LOGGER."))

(defgeneric current-timestamp (logger)
  (:documentation "Return current timestamp string for LOGGER."))

;;; Method implementations

(defmethod current-timestamp ((logger logger))
  "Return current timestamp as a string."
  (declare (ignore logger))
  (multiple-value-bind (sec min hour day month year)
      (get-decoded-time)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour min sec)))

(defmethod log-message ((logger logger) action &rest args)
  "Log an ACTION with ARGS to the logger's file."
  (when (logger-enabled logger)
    (let ((log-path (stash-cl/paths:expand-home (logger-log-file logger))))
      (ensure-directories-exist log-path)
      (with-open-file (stream log-path
                              :direction :output
                              :if-exists :append
                              :if-does-not-exist :create)
        (format stream "[~A] ~A~{ ~A~}~%"
                (current-timestamp logger)
                action
                args)))))

;;; Global logger instance

(defparameter *logger* 
  (make-instance 'logger :log-file "~/.local/state/stash/stash.log")
  "Default logger instance.")

;;; Backward compatible interface

(defparameter *log-file* "~/.local/state/stash/stash.log"
  "Path to the log file. Deprecated: use (logger-log-file *logger*) instead.")

(defun log-action (action &rest args)
  "Log an ACTION with ARGS to the log file."
  (apply #'log-message *logger* action args))
