;;;; colors.lisp --- ANSI color codes for terminal output

(in-package #:stash-cl/colors)

(defparameter *use-colors* t
  "Whether to use ANSI colors in output.")

(defun color-red (text)
  "Wrap TEXT in red ANSI color codes."
  (if *use-colors*
      (format nil "~C[31m~A~C[0m" #\Escape text #\Escape)
      text))

(defun color-green (text)
  "Wrap TEXT in green ANSI color codes."
  (if *use-colors*
      (format nil "~C[32m~A~C[0m" #\Escape text #\Escape)
      text))

(defun color-yellow (text)
  "Wrap TEXT in yellow ANSI color codes."
  (if *use-colors*
      (format nil "~C[33m~A~C[0m" #\Escape text #\Escape)
      text))

(defun color-blue (text)
  "Wrap TEXT in blue ANSI color codes."
  (if *use-colors*
      (format nil "~C[34m~A~C[0m" #\Escape text #\Escape)
      text))

(defun color-reset ()
  "Return ANSI reset code."
  (if *use-colors*
      (format nil "~C[0m" #\Escape)
      ""))
