;;;; colors.lisp --- ANSI color codes for terminal output

(in-package #:stash-cl/colors)

;;; CLOS Terminal Style System

(defclass terminal-style ()
  ((code :initarg :code
         :accessor style-code
         :type string
         :documentation "ANSI escape code for this style")
   (name :initarg :name
         :accessor style-name
         :type keyword
         :documentation "Name of this style"))
  (:documentation "Represents an ANSI terminal style (color or attribute)."))

(defgeneric apply-style (style text)
  (:documentation "Apply STYLE to TEXT, returning styled string."))

(defmethod apply-style ((style terminal-style) text)
  "Apply terminal style to text."
  (if *use-colors*
      (format nil "~C[~Am~A~C[0m" #\Escape (style-code style) text #\Escape)
      text))

;;; Global configuration

(defparameter *use-colors* t
  "Whether to use ANSI colors in output.")

;;; Pre-defined style instances

(defparameter *style-red* (make-instance 'terminal-style :code "31" :name :red))
(defparameter *style-green* (make-instance 'terminal-style :code "32" :name :green))
(defparameter *style-yellow* (make-instance 'terminal-style :code "33" :name :yellow))
(defparameter *style-blue* (make-instance 'terminal-style :code "34" :name :blue))
(defparameter *style-bold* (make-instance 'terminal-style :code "1" :name :bold))

;;; Backward compatible functions

(defun color-red (text)
  "Wrap TEXT in red ANSI color codes."
  (apply-style *style-red* text))

(defun color-green (text)
  "Wrap TEXT in green ANSI color codes."
  (apply-style *style-green* text))

(defun color-yellow (text)
  "Wrap TEXT in yellow ANSI color codes."
  (apply-style *style-yellow* text))

(defun color-blue (text)
  "Wrap TEXT in blue ANSI color codes."
  (apply-style *style-blue* text))

(defun color-bold (text)
  "Wrap TEXT in bold ANSI codes."
  (apply-style *style-bold* text))

(defun color-reset ()
  "Return ANSI reset code."
  (if *use-colors*
      (format nil "~C[0m" #\Escape)
      ""))

(defun format-error (message &optional suggestion)
  "Format an error message with color and optional suggestion."
  (format nil "~A ~A~A"
          (color-red "Error:")
          message
          (if suggestion
              (format nil "~%  ~A ~A" 
                      (color-yellow "→")
                      (color-bold suggestion))
              "")))

(defun format-warning (message)
  "Format a warning message with color."
  (format nil "~A ~A"
          (color-yellow "Warning:")
          message))

(defun format-success (message)
  "Format a success message with color."
  (format nil "~A ~A"
          (color-green "✓")
          message))
