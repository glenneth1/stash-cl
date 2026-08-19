;;;; config.lisp --- Configuration file support for stash-cl

(in-package #:stash-cl/config)

(defstruct stash-config
  "Configuration loaded from file.
Each field is NIL if not set, meaning 'use the default'."
  (dir nil)
  (target nil)
  (source nil)
  (verbose nil)
  (no-folding nil)
  (ignore-patterns nil)
  (defer-patterns nil)
  (override-patterns nil))

(defun config-file-paths ()
  "Return list of candidate config file paths in priority order.
XDG config dir first, then ~/.stashrc."
  (let ((xdg-config (or (uiop:getenv "XDG_CONFIG_HOME")
                        (concatenate 'string
                                     (namestring (user-homedir-pathname))
                                     "/.config"))))
    (list (concatenate 'string xdg-config "/stash/config")
          (concatenate 'string
                       (namestring (user-homedir-pathname))
                       "/.stashrc"))))

(defun find-config-file ()
  "Find the first existing config file, or NIL if none found."
  (find-if #'probe-file (config-file-paths)))

(defun parse-config-line (line)
  "Parse a single config line.
Returns (KEY . VALUE) or NIL for comments/blank lines.
KEY is a keyword, VALUE is a string."
  (let ((trimmed (string-trim '(#\Space #\Tab) line)))
    ;; Skip empty lines and comments
    (when (or (string= trimmed "")
              (char= (char trimmed 0) #\#))
      (return-from parse-config-line nil))
    ;; Parse key = value
    (let ((pos (position #\= trimmed)))
      (unless pos
        (return-from parse-config-line nil))
      (let ((key-str (string-trim '(#\Space #\Tab) (subseq trimmed 0 pos)))
            (val-str (string-trim '(#\Space #\Tab) (subseq trimmed (1+ pos)))))
        (when (string= key-str "")
          (return-from parse-config-line nil))
        (cons (intern (string-upcase key-str) :keyword)
              val-str)))))

(defun load-config-file (&optional path)
  "Load configuration from PATH (or auto-detect if NIL).
Returns a STASH-CONFIG struct."
  (let ((config (make-stash-config))
        (file-path (or path (find-config-file))))
    (unless (and file-path (probe-file file-path))
      (return-from load-config-file config))
    
    (with-open-file (stream file-path :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            do (let ((parsed (parse-config-line line)))
                 (when parsed
                   (let ((key (car parsed))
                         (val (cdr parsed)))
                     (case key
                       (:dir
                        (setf (stash-config-dir config) val))
                       (:target
                        (setf (stash-config-target config) val))
                       (:source
                        (setf (stash-config-source config) val))
                       (:verbose
                        (setf (stash-config-verbose config)
                              (parse-integer val :junk-allowed t)))
                       (:no-folding
                        (setf (stash-config-no-folding config)
                              (member val '("true" "yes" "1") :test #'string-equal)))
                       (:ignore
                        (push val (stash-config-ignore-patterns config)))
                       (:defer
                        (push val (stash-config-defer-patterns config)))
                       (:override
                        (push val (stash-config-override-patterns config)))
                       (otherwise
                        ;; Unknown key, silently ignore
                        )))))))
    
    ;; Reverse lists since we pushed (LIFO)
    (setf (stash-config-ignore-patterns config)
          (nreverse (stash-config-ignore-patterns config)))
    (setf (stash-config-defer-patterns config)
          (nreverse (stash-config-defer-patterns config)))
    (setf (stash-config-override-patterns config)
          (nreverse (stash-config-override-patterns config)))
    
    config))

(defun merge-config-with-options (config options)
  "Merge CONFIG defaults with OPTIONS plist from CLI.
CLI values take precedence over config file values.
Returns a new OPTIONS plist with config defaults filled in where CLI didn't specify."
  (let ((result (copy-list options)))
    ;; Dir: only use config if not specified on CLI
    (unless (getf result :dir)
      (when (stash-config-dir config)
        (setf (getf result :dir) (stash-config-dir config))))
    ;; Target
    (unless (getf result :target)
      (when (stash-config-target config)
        (setf (getf result :target) (stash-config-target config))))
    ;; Source
    (unless (getf result :source)
      (when (stash-config-source config)
        (setf (getf result :source) (stash-config-source config))))
    ;; Verbose: use config if CLI didn't specify any
    (unless (getf result :verbose)
      (when (stash-config-verbose config)
        (setf (getf result :verbose) (stash-config-verbose config))))
    ;; No-folding
    (unless (getf result :no-folding)
      (when (stash-config-no-folding config)
        (setf (getf result :no-folding) t)))
    ;; Ignore patterns: append config patterns to CLI patterns
    (when (stash-config-ignore-patterns config)
      (let ((cli-ignores (loop for (key val) on result by #'cddr
                               when (eq key :ignore) collect val)))
        (setf result
              (append result
                      (loop for pattern in (stash-config-ignore-patterns config)
                            unless (member pattern cli-ignores :test #'string=)
                            collect :ignore and collect pattern)))))
    ;; Defer patterns
    (when (stash-config-defer-patterns config)
      (let ((cli-defers (loop for (key val) on result by #'cddr
                              when (eq key :defer) collect val)))
        (setf result
              (append result
                      (loop for pattern in (stash-config-defer-patterns config)
                            unless (member pattern cli-defers :test #'string=)
                            collect :defer and collect pattern)))))
    ;; Override patterns
    (when (stash-config-override-patterns config)
      (let ((cli-overrides (loop for (key val) on result by #'cddr
                                 when (eq key :override) collect val)))
        (setf result
              (append result
                      (loop for pattern in (stash-config-override-patterns config)
                            unless (member pattern cli-overrides :test #'string=)
                            collect :override and collect pattern)))))
    
    result))
