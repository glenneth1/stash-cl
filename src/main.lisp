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
   :short #\V
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
  
  (:name :defer
   :description "Defer pattern - skip files matching REGEX during stashing"
   :long "defer"
   :arg-parser #'identity
   :meta-var "REGEX")
  
  (:name :override
   :description "Override pattern - force stow files matching REGEX even if ignored"
   :long "override"
   :arg-parser #'identity
   :meta-var "REGEX")
  
  (:name :recursive
   :description "Recursively process directories"
   :short #\r
   :long "recursive")
  
  (:name :import
   :description "Import existing file/directory into a new package"
   :short #\i
   :long "import"
   :arg-parser #'identity
   :meta-var "PATH")
  
  (:name :package
   :description "Package name (used with --import)"
   :short #\p
   :long "package"
   :arg-parser #'identity
   :meta-var "NAME")
  
  (:name :list
   :description "List all packages and their stashed status"
   :short #\l
   :long "list"))

;;; Helper Functions

(defun get-all-packages (stash-dir)
  "Get list of all packages in STASH-DIR.
Excludes dot-directories (e.g. .git) which are not stash packages."
  (let ((packages nil))
    (dolist (entry (uiop:subdirectories stash-dir))
      (let ((package-name (car (last (pathname-directory entry)))))
        (unless (or (string= package-name ".")
                   (string= package-name "..")
                   (char= (char package-name 0) #\.))
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
      (error (stash-cl/colors:format-error 
              (format nil "Package directory does not exist: ~A" package-path-dir)
              (format nil "Check that the package exists in ~A" stash-dir))))
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
Scans target directory and moves non-symlink files that would conflict into the package."
  (let ((adopted-count 0)
        (package-path-normalized (uiop:ensure-directory-pathname package-path))
        (target-dir-normalized (uiop:ensure-directory-pathname target-dir)))
    
    (format t "~%~A~%" (stash-cl/colors:format-warning "Scanning for files to adopt..."))
    
    (labels ((adopt-file (target-file rel-path-str)
               "Adopt TARGET-FILE into package at REL-PATH-STR."
               (let ((package-file (concatenate 'string 
                                               (namestring package-path-normalized)
                                               rel-path-str)))
                 (if simulate
                     (format t "  Would adopt: ~A~%" rel-path-str)
                     (progn
                       (format t "  ~A ~A~%" 
                               (stash-cl/colors:color-green "Adopting:")
                               rel-path-str)
                       ;; Ensure parent directory exists in package
                       (ensure-directories-exist package-file)
                       ;; Move the file - use absolute paths
                       (let* ((abs-target (truename target-file))
                              (abs-package (merge-pathnames package-file)))
                         (uiop:rename-file-overwriting-target abs-target abs-package))))
                 (incf adopted-count)))
             
             (scan-target-directory (tgt-dir rel-prefix-str)
               "Recursively scan target directory for files to adopt."
               (when (probe-file tgt-dir)
                 ;; Check each file in target
                 (dolist (tgt-file (uiop:directory-files tgt-dir))
                   (let* ((filename (file-namestring tgt-file))
                          (rel-path-str (concatenate 'string rel-prefix-str filename)))
                     ;; Adopt if it's a regular file (not a symlink)
                     (when (not (file-is-symlink-p (namestring tgt-file)))
                       (adopt-file tgt-file rel-path-str))))
                 
                 ;; Recursively process subdirectories
                 (dolist (tgt-subdir (uiop:subdirectories tgt-dir))
                   (let* ((dirname (car (last (pathname-directory tgt-subdir))))
                          (new-rel-prefix-str (concatenate 'string rel-prefix-str dirname "/")))
                     (scan-target-directory tgt-subdir new-rel-prefix-str))))))
      
      ;; Start scanning from target root
      (scan-target-directory target-dir-normalized "")
      
      (if (> adopted-count 0)
          (format t "~%~A~%~%" (stash-cl/colors:format-success 
                                (format nil "Adopted ~A file(s)" adopted-count)))
          (format t "~%No files to adopt~%~%"))
      
      adopted-count)))

(defun handle-stash-with-folding (package stash-dir target-dir &key simulate adopt cli-patterns defer-patterns override-patterns)
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
          (stash-package-with-folding package-path target-dir 
                                       :cli-patterns cli-patterns
                                       :defer-patterns defer-patterns
                                       :override-patterns override-patterns)
          
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
  
  ;; Resolve package path
  (let ((package-path (resolve-package-path package stash-dir)))
    
    ;; Phase 1: Unstash - plan and execute immediately
    ;; This ensures the filesystem state is correct for the stash phase
    (format t "~%Phase 1: Unstashing...~%")
    (init-planner stash-dir target-dir)
    (unstash-package-with-refolding package-path target-dir)
    (execute-all-tasks :simulate simulate)
    
    ;; Phase 2: Stash - plan and execute with fresh planner
    ;; Now the filesystem reflects the unstashed state
    (format t "~%Phase 2: Stashing...~%")
    (init-planner stash-dir target-dir)
    (stash-package-with-folding package-path target-dir)
    (execute-all-tasks :simulate simulate)
    
    ;; Show stats
    (print-folding-stats)
    
    (format t "~%Restash complete!~%")))

(defun package-symlink-count (package-path target-dir)
  "Count how many symlinks in TARGET-DIR correspond to files in PACKAGE-PATH.
Walks the package tree (not the target) and checks each corresponding target path.
Returns two values: count of active symlinks, and total files in package."
  (let ((package-real (namestring (truename package-path)))
        (symlink-count 0)
        (total-count 0))
    (labels ((check-package (pkg-dir rel-path)
               "Walk package directory, checking corresponding target paths."
               ;; Check files in this directory
               (dolist (f (uiop:directory-files pkg-dir))
                 (incf total-count)
                 (let* ((fname (file-namestring f))
                        (target-path (concatenate 'string target-dir "/" rel-path fname)))
                   (when (and (probe-file target-path)
                              (file-is-symlink-p target-path))
                     (let ((link-target (read-symlink target-path)))
                       (handler-case
                           (let ((resolved (namestring (truename link-target))))
                             (when (alexandria:starts-with-subseq package-real resolved)
                               (incf symlink-count)))
                         (error () nil))))))
               ;; Check subdirectories
               (dolist (d (uiop:subdirectories pkg-dir))
                 (let* ((dname (car (last (pathname-directory d))))
                        (target-path (concatenate 'string target-dir "/" rel-path dname)))
                   (cond
                     ;; Target is a symlink - check if it points to this package dir
                     ((and (probe-file target-path)
                           (file-is-symlink-p target-path))
                      (incf total-count)
                      (let ((link-target (read-symlink target-path)))
                        (handler-case
                            (let ((resolved (namestring (truename link-target))))
                              (when (alexandria:starts-with-subseq package-real resolved)
                                (incf symlink-count)))
                          (error () nil))))
                     ;; Target is a real directory - recurse into it
                     ((and (probe-file target-path)
                           (not (file-is-symlink-p target-path)))
                      (check-package (namestring d)
                                     (concatenate 'string rel-path dname "/")))
                     ;; Target doesn't exist - count the file but no symlink
                     (t
                      (incf total-count)))))))
      (check-package package-path ""))
    (values symlink-count total-count)))

(defun handle-list (stash-dir target-dir)
  "List all packages in STASH-DIR and show whether they are stashed into TARGET-DIR."
  (let ((packages (get-all-packages stash-dir)))
    (if (null packages)
        (format t "No packages found in ~A~%" stash-dir)
        (progn
          (format t "~%Packages in ~A~%" stash-dir)
          (format t "Target: ~A~%" target-dir)
          (format t "~A~%" (make-string 60 :initial-element #\-))
          (dolist (pkg packages)
            (let* ((pkg-path (concatenate 'string stash-dir "/" pkg "/"))
                   (pkg-exists (uiop:directory-exists-p pkg-path)))
              (if pkg-exists
                  (multiple-value-bind (symlink-count total-count)
                      (package-symlink-count pkg-path target-dir)
                    (let ((status-str
                           (cond
                             ((and (> total-count 0) (= symlink-count total-count))
                              (stash-cl/colors:color-green "stashed"))
                             ((and (> total-count 0) (= symlink-count 0))
                              (stash-cl/colors:color-red "not stashed"))
                             ((= total-count 0)
                              (stash-cl/colors:color-yellow "empty"))
                             (t
                              (format nil "~A/~D"
                                      (stash-cl/colors:color-yellow "partial")
                                      symlink-count)))))
                      (format t "  ~A~A~A  [~A]  (~D file~:P)~%"
                              (stash-cl/colors:color-bold pkg)
                              (make-string (max 1 (- 20 (length pkg))) :initial-element #\space)
                              status-str
                              status-str
                              total-count)))
                  (format t "  ~A~A  [~A]~%"
                          pkg
                          (make-string (max 1 (- 20 (length pkg))) :initial-element #\space)
                          (stash-cl/colors:color-red "missing")))))
          (format t "~A~%" (make-string 60 :initial-element #\-))
          (format t "~D package~:P found~%~%" (length packages))))))

(defun handle-deploy (stash-dir target-dir &key simulate)
  "Deploy all packages from STASH-DIR to TARGET-DIR."
  (let ((packages (get-all-packages stash-dir)))
    (if packages
        (progn
          (format t "~%Deploying ~D package(s)...~%" (length packages))
          (dolist (package packages)
            (handle-stash-with-folding package stash-dir target-dir :simulate simulate)))
        (format t "No packages found in ~A~%" stash-dir))))

(defun handle-import (source-path package-name stash-dir target-dir &key simulate)
  "Import SOURCE-PATH into a new package PACKAGE-NAME.
Creates the package directory structure mirroring the path relative to TARGET-DIR,
moves the source into the package, and creates a symlink back."
  
  ;; Expand and normalize paths
  (let* ((source-abs (namestring (truename (expand-home source-path))))
         (target-abs (namestring (truename (expand-home target-dir))))
         (stash-abs (expand-home stash-dir)))
    
    ;; Ensure target-abs ends with /
    (unless (char= (char target-abs (1- (length target-abs))) #\/)
      (setf target-abs (concatenate 'string target-abs "/")))
    
    ;; Check source exists
    (unless (probe-file source-abs)
      (format t "~A~%" (stash-cl/colors:format-error 
                        (format nil "Source does not exist: ~A" source-path)))
      (return-from handle-import nil))
    
    ;; Check source is under target directory
    (unless (and (>= (length source-abs) (length target-abs))
                 (string= (subseq source-abs 0 (length target-abs)) target-abs))
      (format t "~A~%" (stash-cl/colors:format-error 
                        (format nil "Source ~A is not under target directory ~A" source-path target-dir)
                        "The file/directory must be inside the target directory"))
      (return-from handle-import nil))
    
    ;; Compute relative path from target (strip trailing slash for consistency)
    (let* ((rel-path (string-right-trim "/" (subseq source-abs (length target-abs))))
           (source-clean (string-right-trim "/" source-abs))
           ;; Package destination: stash-dir/package-name/rel-path
           (package-dir (concatenate 'string stash-abs "/" package-name "/"))
           (dest-path (concatenate 'string package-dir rel-path)))
      
      (format t "~%Importing into package: ~A~%" package-name)
      (format t "  Source: ~A~%" source-clean)
      (format t "  Relative path: ~A~%" rel-path)
      (format t "  Package location: ~A~%" dest-path)
      
      (if simulate
          (progn
            (format t "~%SIMULATION MODE - No changes will be made~%")
            (format t "  Would create: ~A~%" dest-path)
            (format t "  Would move: ~A -> ~A~%" source-clean dest-path)
            (format t "  Would create symlink: ~A -> ~A~%" source-clean dest-path))
          (progn
            ;; Create package directory structure (parent of dest-path only)
            ;; For dest /tmp/test-stash/testapp/.config/testapp, create /tmp/test-stash/testapp/.config/
            ;; We need to find the parent directory of the final component
            (let* ((last-slash (position #\/ dest-path :from-end t))
                   (parent-path (when last-slash (subseq dest-path 0 (1+ last-slash)))))
              (when parent-path
                (ensure-directories-exist parent-path)
                (format t "  Created parent directory: ~A~%" parent-path)))
            
            ;; Move source to package
            (let ((is-dir (stash-cl/file-ops:file-is-directory-p source-clean)))
              (if is-dir
                  ;; For directories, use mv command
                  (progn
                    (uiop:run-program (list "mv" source-clean dest-path))
                    (format t "  ~A ~A~%" 
                            (stash-cl/colors:color-green "Moved directory:")
                            rel-path))
                  ;; For files
                  (progn
                    (uiop:rename-file-overwriting-target source-clean dest-path)
                    (format t "  ~A ~A~%" 
                            (stash-cl/colors:color-green "Moved file:")
                            rel-path)))
              
              ;; Create symlink directly (source is now gone, dest-path has the content)
              (format t "~%Creating symlink...~%")
              (stash-cl/file-ops:create-symlink source-clean dest-path)
              (format t "  ~A ~A -> ~A~%" 
                      (stash-cl/colors:color-green "Created symlink:")
                      rel-path dest-path))))
      
      (format t "~%Import complete!~%")
      t)))

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
                 (list-mode (getf options :list))
                 (adopt (getf options :adopt))
                 (import-path (getf options :import))
                 (package-name (getf options :package))
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
                 (cli-defer-patterns (loop for (key val) on options by #'cddr
                                          when (eq key :defer)
                                          collect val))
                 (cli-override-patterns (loop for (key val) on options by #'cddr
                                             when (eq key :override)
                                             collect val))
                 (packages free-args))
            
            ;; Set folding options
            (setf *folding-enabled* (not no-folding))
            (setf *folding-verbosity* verbosity)
            
            ;; Resolve directories
            (setf stash-dir (expand-home stash-dir))
            (let ((target-dir (resolve-target-path target stash-dir)))
              
              (cond
                ;; List mode - show all packages and their status
                (list-mode
                 (handle-list stash-dir target-dir))
                
                ;; Import mode - import existing file/directory into a package
                (import-path
                 (if package-name
                     (handle-import import-path package-name stash-dir target-dir :simulate simulate)
                     (format t "~A~%" (stash-cl/colors:format-error 
                                       "Package name required for import" 
                                       "Try: stash --import ~/.bashrc --package bash"))))
                
                ;; Deploy mode
                (deploy
                 (handle-deploy stash-dir target-dir :simulate simulate))
                
                ;; Restash mode
                (restash
                 (if packages
                     (dolist (pkg packages)
                       (handle-restash pkg stash-dir target-dir :simulate simulate))
                     (format t "~A~%" (stash-cl/colors:format-error "No packages specified for restash" "Try: stash -R PACKAGE"))))
                
                ;; Unstash mode
                (delete
                 (if packages
                     (dolist (pkg packages)
                       (handle-unstash-with-refolding pkg stash-dir target-dir :simulate simulate))
                     (format t "~A~%" (stash-cl/colors:format-error "No packages specified for unstash" "Try: stash -D PACKAGE"))))
                
                ;; Stash mode (default)
                (packages
                 (dolist (pkg packages)
                   (handle-stash-with-folding pkg stash-dir target-dir 
                                             :simulate simulate 
                                             :adopt adopt
                                             :cli-patterns cli-ignore-patterns
                                             :defer-patterns cli-defer-patterns
                                             :override-patterns cli-override-patterns)))
                
                ;; No action specified
                (t
                 (format t "~A~%" (stash-cl/colors:format-error "No packages specified" "Try: stash PACKAGE or stash -h for help"))
                 (uiop:quit 1))))))
      
      (opts:unknown-option (condition)
        (format t "~A~%" (stash-cl/colors:format-error (format nil "Unknown option: ~A" (opts:option condition)) "Use -h to see available options"))
        (uiop:quit 1))
      
      (opts:missing-arg (condition)
        (format t "~A~%" (stash-cl/colors:format-error (format nil "Missing argument for option: ~A" (opts:option condition)) "This option requires a value"))
        (uiop:quit 1))
      
      (opts:arg-parser-failed (condition)
        (format t "~A~%" (stash-cl/colors:format-error (format nil "Failed to parse argument for option: ~A" (opts:option condition)) "Check the argument format"))
        (uiop:quit 1))
      
      (error (condition)
        (format t "~A~%" (stash-cl/colors:format-error (format nil "~A" condition)))
        (uiop:quit 1)))))

(defun toplevel-entry ()
  "Entry point for the standalone executable."
  (main)
  (uiop:quit 0))
