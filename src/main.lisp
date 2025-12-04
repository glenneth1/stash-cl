;;;; main.lisp --- Main entry point and CLI for stash-cl

(in-package #:stash-cl)

;;; CLI Options Definition

(opts:define-opts
  (:name :help
   :description "Display this help message"
   :short #\h
   :long "help")
  
  (:name :version
   :description "Display version information"
   :long "version")
  
  (:name :source
   :description "Source directory to stash"
   :short #\s
   :long "source"
   :arg-parser #'identity
   :meta-var "DIR")
  
  (:name :target
   :description "Target directory"
   :short #\t
   :long "target"
   :arg-parser #'identity
   :meta-var "DIR")
  
  (:name :dir
   :description "Stash directory (default: current directory)"
   :long "dir"
   :arg-parser #'identity
   :meta-var "DIR")
  
  (:name :deploy
   :description "Deploy mode - stash all packages"
   :short #\d
   :long "deploy")
  
  (:name :delete
   :description "Unstash packages"
   :short #\D
   :long "delete")
  
  (:name :restash
   :description "Restash packages (unstash then stash)"
   :short #\R
   :long "restash")
  
  (:name :simulate
   :description "Simulation mode (dry-run)"
   :short #\n
   :long "simulate")
  
  (:name :no-folding
   :description "Disable tree folding"
   :long "no-folding")
  
  (:name :verbose
   :description "Increase verbosity (use multiple times: -v, -v -v, -v -v -v)"
   :short #\v
   :long "verbose"
   :reduce (lambda (acc val) 
             (declare (ignore val)) 
             (if (numberp acc) (1+ acc) 1))
   :initial-value 0)
  
  (:name :adopt
   :description "Adopt existing files into package (move to package, then symlink)"
   :long "adopt")
  
  (:name :ignore
   :description "Ignore pattern (regex, can be specified multiple times)"
   :long "ignore"
   :arg-parser #'identity
   :meta-var "REGEX")
  
  (:name :recursive
   :description "Recursively process directories"
   :short #\r
   :long "recursive"))

;;; Helper Functions

(defun get-all-packages (stash-dir)
  "Get list of all packages in STASH-DIR."
  (let ((packages nil))
    (dolist (entry (uiop:subdirectories stash-dir))
      (let ((package-name (car (last (pathname-directory entry)))))
        (unless (or (string= package-name ".")
                   (string= package-name ".."))
          (push package-name packages))))
    (nreverse packages)))

(defun resolve-package-path (package stash-dir)
  "Resolve the full path to PACKAGE in STASH-DIR."
  (let* ((package-path (if (string= package ".")
                           stash-dir
                           (concatenate 'string stash-dir "/" package)))
         ;; Ensure trailing slash for directory operations
         (package-path-dir (if (char= (char package-path (1- (length package-path))) #\/)
                               package-path
                               (concatenate 'string package-path "/"))))
    (unless (uiop:directory-exists-p package-path-dir)
      (error "Package directory does not exist: ~A" package-path-dir))
    package-path-dir))

(defun resolve-target-path (target stash-dir)
  "Resolve the target directory path."
  (if target
      (expand-home target)
      ;; Default: parent directory of stash-dir
      (namestring (uiop:pathname-parent-directory-pathname 
                   (uiop:ensure-directory-pathname stash-dir)))))

;;; Core Handler Functions

(defun adopt-existing-files (package-path target-dir &key simulate)
  "Adopt existing files from TARGET-DIR into PACKAGE-PATH.
Moves files that exist in target but not in package into the package directory.
NOTE: This is a basic implementation that needs refinement."
  (format t "~%Note: --adopt flag is experimental and may need refinement~%")
  (format t "Adopting existing files from ~A...~%"  target-dir)
  (format t "Into package: ~A~%~%" package-path)
  
  ;; For now, just report what would be done
  ;; Full implementation requires careful path handling
  (if simulate
      (format t "  Simulation: Would scan target for files to adopt~%")
      (format t "  Warning: Adopt functionality needs testing - use -n first~%"))
  
  0)  ; Return 0 adopted files for now

(defun handle-stash-with-folding (package stash-dir target-dir &key simulate adopt cli-patterns)
  "Stash PACKAGE using task planner and folding."
  
  (format t "~%Stashing package: ~A~%" package)
  (format t "  From: ~A~%" stash-dir)
  (format t "  To: ~A~%~%" target-dir)
  
  ;; Initialize planner
  (init-planner stash-dir target-dir)
  
  ;; Resolve package path
  (let ((package-path (resolve-package-path package stash-dir)))
    
    ;; Adopt existing files if requested
    (when adopt
      (adopt-existing-files package-path target-dir :simulate simulate))
    
    ;; Use enhanced folding - catch conflicts
    (handler-case
        (progn
          (stash-package-with-folding package-path target-dir :cli-patterns cli-patterns)
          
          ;; Execute (or simulate)
          (execute-all-tasks :simulate simulate))
      
      (stash-cl/task-planner:conflict-error (c)
        ;; In simulation mode, conflicts are reported but don't stop execution
        ;; In normal mode, re-signal the error
        (unless simulate
          (error c))))
    
    ;; Show stats
    (print-folding-stats)))

(defun handle-unstash-with-refolding (package stash-dir target-dir &key simulate)
  "Unstash PACKAGE using task planner and refolding."
  
  (format t "~%Unstashing package: ~A~%" package)
  (format t "  From: ~A~%" target-dir)
  (format t "  Package: ~A~%~%" stash-dir)
  
  ;; Initialize planner
  (init-planner stash-dir target-dir)
  
  ;; Resolve package path
  (let ((package-path (resolve-package-path package stash-dir)))
    
    ;; Use enhanced unstashing with refolding
    (unstash-package-with-refolding package-path target-dir)
    
    ;; Execute (or simulate)
    (execute-all-tasks :simulate simulate)
    
    ;; Show stats
    (print-folding-stats)))

(defun handle-restash (package stash-dir target-dir &key simulate)
  "Restash PACKAGE by unstashing then stashing."
  
  (format t "~%Restashing package: ~A~%" package)
  
  ;; Initialize planner
  (init-planner stash-dir target-dir)
  
  ;; Resolve package path
  (let ((package-path (resolve-package-path package stash-dir)))
    
    ;; Phase 1: Unstash
    (format t "~%Phase 1: Unstashing...~%")
    (unstash-package-with-refolding package-path target-dir)
    
    ;; Phase 2: Stash
    (format t "~%Phase 2: Stashing...~%")
    (stash-package-with-folding package-path target-dir)
    
    ;; Execute all tasks
    (execute-all-tasks :simulate simulate)
    
    ;; Show stats
    (print-folding-stats)
    
    (format t "~%Restash complete!~%")))

(defun handle-deploy (stash-dir target-dir &key simulate)
  "Deploy all packages from STASH-DIR to TARGET-DIR."
  (let ((packages (get-all-packages stash-dir)))
    (if packages
        (progn
          (format t "~%Deploying ~D package(s)...~%" (length packages))
          (dolist (package packages)
            (handle-stash-with-folding package stash-dir target-dir :simulate simulate)))
        (format t "No packages found in ~A~%" stash-dir))))

;;; Main Entry Point

(defun main (&optional (argv nil))
  "Main entry point for stash-cl."
  (let ((args (or argv (uiop:command-line-arguments))))
    
    (handler-case
        (multiple-value-bind (options free-args)
            (opts:get-opts args)
          
          ;; Handle help and version first
          (when (getf options :help)
            (display-help)
            (uiop:quit 0))
          
          (when (getf options :version)
            (display-version)
            (uiop:quit 0))
          
          ;; Get options
          (let* ((simulate (getf options :simulate))
                 (no-folding (getf options :no-folding))
                 (delete (getf options :delete))
                 (restash (getf options :restash))
                 (deploy (getf options :deploy))
                 (adopt (getf options :adopt))
                 ;; Count verbose flags manually since unix-opts doesn't handle -vv properly
                 (verbosity (count :verbose options))
                 (stash-dir (getf options :dir (namestring (uiop:getcwd))))
                 (recursive-p (getf options :recursive))
                 (source (getf options :source))
                 (target (getf options :target))
                 ;; Collect all --ignore patterns
                 (cli-ignore-patterns (loop for (key val) on options by #'cddr
                                           when (eq key :ignore)
                                           collect val))
                 (packages free-args))
            
            ;; Set folding options
            (setf *folding-enabled* (not no-folding))
            (setf *folding-verbosity* verbosity)
            
            ;; Resolve directories
            (setf stash-dir (expand-home stash-dir))
            (let ((target-dir (resolve-target-path target stash-dir)))
              
              (cond
                ;; Deploy mode
                (deploy
                 (handle-deploy stash-dir target-dir :simulate simulate))
                
                ;; Restash mode
                (restash
                 (if packages
                     (dolist (pkg packages)
                       (handle-restash pkg stash-dir target-dir :simulate simulate))
                     (format t "Error: No packages specified for restash~%")))
                
                ;; Unstash mode
                (delete
                 (if packages
                     (dolist (pkg packages)
                       (handle-unstash-with-refolding pkg stash-dir target-dir :simulate simulate))
                     (format t "Error: No packages specified for unstash~%")))
                
                ;; Stash mode (default)
                (packages
                 (dolist (pkg packages)
                   (handle-stash-with-folding pkg stash-dir target-dir 
                                             :simulate simulate 
                                             :adopt adopt
                                             :cli-patterns cli-ignore-patterns)))
                
                ;; No action specified
                (t
                 (format t "Error: No packages specified~%")
                 (format t "Use --help for usage information~%")
                 (uiop:quit 1))))))
      
      (opts:unknown-option (condition)
        (format t "Error: Unknown option: ~A~%" (opts:option condition))
        (format t "Use --help for usage information~%")
        (uiop:quit 1))
      
      (opts:missing-arg (condition)
        (format t "Error: Missing argument for option: ~A~%" (opts:option condition))
        (format t "Use --help for usage information~%")
        (uiop:quit 1))
      
      (opts:arg-parser-failed (condition)
        (format t "Error: Failed to parse argument for option: ~A~%" (opts:option condition))
        (format t "Use --help for usage information~%")
        (uiop:quit 1))
      
      (error (condition)
        (format t "Error: ~A~%" condition)
        (uiop:quit 1)))))

(defun toplevel-entry ()
  "Entry point for the standalone executable."
  (main)
  (uiop:quit 0))
