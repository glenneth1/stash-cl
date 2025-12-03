;;;; folding.lisp --- Enhanced tree folding/unfolding for stash-cl

(in-package #:stash-cl/folding)

;;; Folding state and configuration

(defparameter *folding-enabled* t
  "Whether tree folding is enabled. Set to NIL for --no-folding mode.")

(defparameter *folding-verbosity* 1
  "Verbosity level for folding operations. 0=silent, 1=basic, 2=detailed, 3=debug")

(defstruct folding-stats
  "Statistics about folding operations."
  (directories-folded 0 :type integer)
  (directories-unfolded 0 :type integer)
  (directories-kept-folded 0 :type integer)
  (file-symlinks-created 0 :type integer))

(defparameter *folding-stats* (make-folding-stats)
  "Current folding statistics.")

;;; Core folding detection

(defun can-fold-directory-p (target-path package-path)
  "Check if TARGET-PATH can be folded into a single symlink to PACKAGE-PATH.
Returns T if the directory can be folded, NIL otherwise."
  
  ;; Can't fold if folding is disabled
  (unless *folding-enabled*
    (return-from can-fold-directory-p nil))
  
  ;; Can't fold if target already exists and is not a symlink
  (when (and (probe-file target-path)
             (not (stash-cl/file-ops:file-is-symlink-p target-path)))
    (cond
      ;; If it's a directory, we need to check if it's empty or owned by us
      ((stash-cl/file-ops:file-is-directory-p target-path)
       (return-from can-fold-directory-p (directory-empty-or-owned-p target-path)))
      ;; If it's a regular file, we can't fold
      (t
       (return-from can-fold-directory-p nil))))
  
  ;; If target is a symlink, check if it points to our package
  (when (stash-cl/file-ops:file-is-symlink-p target-path)
    (let ((link-dest (read-symlink target-path)))
      ;; If it points to our package, we can keep it folded
      (when (symlink-points-to-package-p link-dest package-path)
        (return-from can-fold-directory-p t))
      ;; If it points elsewhere, we need to unfold
      (return-from can-fold-directory-p nil)))
  
  ;; Target doesn't exist - we can fold
  t)

(defun directory-empty-or-owned-p (dir-path)
  "Check if DIR-PATH is empty.
For multiple package support, we only fold truly empty directories."
  (let* ((dir-path-with-slash (if (char= (char dir-path (1- (length dir-path))) #\/)
                                   dir-path
                                   (concatenate 'string dir-path "/")))
         (files (uiop:directory-files dir-path-with-slash))
         (subdirs (uiop:subdirectories dir-path-with-slash)))
    ;; Directory must be completely empty (no files, no subdirs)
    (and (null files) (null subdirs))))

(defun read-symlink (path)
  "Read the target of a symlink at PATH."
  #+osicat
  (osicat:read-link path)
  #-osicat
  (string-trim '(#\Newline #\Return)
               (uiop:run-program (list "readlink" path)
                                :output :string)))

(defun symlink-points-to-package-p (link-dest package-path)
  "Check if LINK-DEST points to PACKAGE-PATH or a subdirectory of it."
  (let ((normalized-dest (namestring (truename link-dest)))
        (normalized-package (namestring (truename package-path))))
    (alexandria:starts-with-subseq normalized-package normalized-dest)))

;;; Enhanced folding strategies

(defun should-fold-at-depth-p (target-path package-path depth)
  "Determine if we should fold at this DEPTH or go deeper.
Returns :fold to fold here, :descend to go deeper, :unfold to create file links."
  (declare (ignore depth))
  
  ;; If we can't fold at all, descend or unfold
  (unless (can-fold-directory-p target-path package-path)
    (return-from should-fold-at-depth-p 
      (if (stash-cl/file-ops:file-is-directory-p package-path)
          :descend
          :unfold)))
  
  ;; Check if target has conflicts that would require unfolding
  (when (target-has-conflicts-p target-path package-path)
    (return-from should-fold-at-depth-p :descend))
  
  ;; Default: fold at this level
  :fold)

(defun target-has-conflicts-p (target-path package-path)
  "Check if TARGET-PATH has existing content that conflicts with PACKAGE-PATH."
  (declare (ignore package-path))
  
  ;; If target doesn't exist, no conflicts
  (unless (probe-file target-path)
    (return-from target-has-conflicts-p nil))
  
  ;; If target is a symlink to our package, no conflict
  (when (stash-cl/file-ops:file-is-symlink-p target-path)
    (return-from target-has-conflicts-p nil))
  
  ;; If target is a directory with non-stash content, conflict
  (when (stash-cl/file-ops:file-is-directory-p target-path)
    (return-from target-has-conflicts-p 
      (not (directory-empty-or-owned-p target-path))))
  
  ;; If target is a regular file, conflict
  t)

;;; Folding operations

(defun fold-directory (target-path package-path)
  "Fold TARGET-PATH into a single symlink pointing to PACKAGE-PATH.
Returns T on success, NIL on failure."
  
  (when (>= *folding-verbosity* 2)
    (format t "  ✓ Folding ~A -> ~A~%" target-path package-path))
  
  ;; If target exists, we need to remove it first
  (when (probe-file target-path)
    (cond
      ;; If it's a symlink, just remove the symlink
      ((stash-cl/file-ops:file-is-symlink-p target-path)
       (stash-cl/task-planner:plan-remove-link target-path))
      
      ;; If it's a real directory, check if it's safe to remove
      ((stash-cl/file-ops:file-is-directory-p target-path)
       (unless (directory-empty-or-owned-p target-path)
         (stash-cl/task-planner:add-conflict 
          "folding"
          (format nil "Cannot fold ~A: directory not empty" target-path)
          target-path)
         (return-from fold-directory nil))
       ;; Remove empty directory
       (stash-cl/task-planner:plan-remove-dir target-path))
      
      ;; It's a regular file - can't fold
      (t
       (stash-cl/task-planner:add-conflict 
        "folding"
        (format nil "Cannot fold ~A: path is a regular file" target-path)
        target-path)
       (return-from fold-directory nil))))
  
  ;; Create symlink (skip conflict checking since we've already validated and planned removal)
  (stash-cl/task-planner:plan-create-link target-path package-path :check-conflicts nil)
  (incf (folding-stats-directories-folded *folding-stats*))
  t)

(defun unfold-directory (target-path package-path &key minimal)
  "Unfold a directory symlink at TARGET-PATH.
If MINIMAL is T, only unfold to the necessary depth (ENHANCEMENT).
Returns T on success, NIL on failure."
  
  (when (>= *folding-verbosity* 2)
    (format t "  ⚠ Unfolding ~A~%" target-path))
  
  ;; If target is a symlink, we need to preserve the original package's content
  (let ((original-package-path nil))
    (when (stash-cl/file-ops:file-is-symlink-p target-path)
      ;; Remember what the symlink pointed to
      (setf original-package-path (read-symlink target-path))
      ;; Remove the symlink
      (stash-cl/task-planner:plan-remove-link target-path))
    
    ;; Create actual directory
    (stash-cl/task-planner:plan-create-dir target-path)
    
    ;; First, stash the original package's content if there was one
    (when original-package-path
      (when (>= *folding-verbosity* 2)
        (format t "  → Preserving original package content from ~A~%" original-package-path))
      (stash-directory-contents target-path original-package-path))
    
    ;; Then stash the new package's contents with smart partial folding (ENHANCEMENT)
    (if minimal
        (unfold-contents-minimal target-path package-path)
        (unfold-contents-full target-path package-path)))
  
  (incf (folding-stats-directories-unfolded *folding-stats*))
  t)

(defun unfold-contents-minimal (target-path package-path)
  "Unfold contents but keep subdirectories folded where possible (ENHANCEMENT)."
  
  (let ((entries (uiop:subdirectories package-path)))
    (dolist (entry entries)
      (let* ((entry-path (namestring entry))
             (entry-name (file-namestring (string-right-trim "/" entry-path)))
             (target-entry (concatenate 'string target-path "/" entry-name))
             (package-entry entry-path))
        
        ;; Try to keep this subdirectory folded
        (if (can-fold-directory-p target-entry package-entry)
            (progn
              (fold-directory target-entry package-entry)
              (when (>= *folding-verbosity* 3)
                (format t "    ✓ Keeping ~A folded~%" target-entry))
              (incf (folding-stats-directories-kept-folded *folding-stats*)))
            ;; Need to descend further
            (stash-directory-contents target-entry package-entry))))))

(defun unfold-contents-full (target-path package-path)
  "Unfold contents completely, creating file symlinks."
  (stash-directory-contents target-path package-path))

(defun stash-directory-contents-no-conflicts (target-dir package-dir)
  "Stash contents of PACKAGE-DIR into TARGET-DIR without conflict checking.
Used when unfolding to preserve original package content."
  
  (let ((files (uiop:directory-files package-dir))
        (subdirs (uiop:subdirectories package-dir)))
    
    ;; Handle files - skip conflict checking
    (dolist (file files)
      (let* ((file-path (namestring file))
             (file-name (file-namestring file-path))
             (target-file (concatenate 'string target-dir "/" file-name))
             (package-file file-path))
        ;; Check ignore patterns even when preserving content
        (unless (and *current-ignore-patterns*
                     (stash-cl/package-mgmt:should-ignore-p file-name *current-ignore-patterns*))
          (stash-cl/task-planner:plan-create-link target-file package-file :check-conflicts nil)
          (incf (folding-stats-file-symlinks-created *folding-stats*)))))
    
    ;; Handle subdirectories - skip conflict checking, just fold
    (dolist (subdir subdirs)
      (let* ((subdir-path (namestring subdir))
             (subdir-name (file-namestring (string-right-trim "/" subdir-path)))
             (target-subdir (concatenate 'string target-dir "/" subdir-name))
             (package-subdir subdir-path))
        ;; Check ignore patterns even when preserving content
        (unless (and *current-ignore-patterns*
                     (stash-cl/package-mgmt:should-ignore-p subdir-name *current-ignore-patterns*))
          ;; Just fold without conflict checking
          (stash-cl/task-planner:plan-create-link target-subdir package-subdir :check-conflicts nil)
          (incf (folding-stats-directories-folded *folding-stats*)))))))

(defun stash-directory-contents (target-dir package-dir)
  "Stash contents of PACKAGE-DIR into TARGET-DIR.
This is called during unfolding or when we can't fold."
  
  ;; First check if target directory has any non-stash files (conflicts)
  ;; Skip this check if target-dir itself is a symlink (we're unfolding it)
  ;; Symlinks are OK (they're from other stash packages), but regular files are conflicts
  (when (and (probe-file target-dir)
             (not (stash-cl/file-ops:file-is-symlink-p target-dir)))
    (let* ((target-dir-with-slash (if (char= (char target-dir (1- (length target-dir))) #\/)
                                       target-dir
                                       (concatenate 'string target-dir "/")))
           (existing-files (uiop:directory-files target-dir-with-slash)))
      (dolist (existing-file existing-files)
        (let ((existing-path (namestring existing-file)))
          ;; Symlinks are OK (from other packages), regular files are conflicts
          (when (not (stash-cl/file-ops:file-is-symlink-p existing-path))
            (stash-cl/task-planner:add-conflict 
             "stash"
             (format nil "Regular file already exists at: ~A" existing-path)
             existing-path)
            (error 'stash-cl/task-planner:conflict-error
                   :message (format nil "Regular file already exists at: ~A" existing-path)
                   :path existing-path))))))
  
  (let ((files (uiop:directory-files package-dir))
        (subdirs (uiop:subdirectories package-dir)))
    
    ;; Handle files
    (dolist (file files)
      (let* ((file-path (namestring file))
             (file-name (file-namestring file-path))
             (target-file (concatenate 'string target-dir "/" file-name))
             (package-file file-path))
        ;; Check if file should be ignored
        (unless (and *current-ignore-patterns*
                     (stash-cl/package-mgmt:should-ignore-p file-name *current-ignore-patterns*))
          (stash-cl/task-planner:plan-create-link target-file package-file)
          (incf (folding-stats-file-symlinks-created *folding-stats*)))))
    
    ;; Handle subdirectories
    (dolist (subdir subdirs)
      (let* ((subdir-path (namestring subdir))
             (subdir-name (file-namestring (string-right-trim "/" subdir-path)))
             (target-subdir (concatenate 'string target-dir "/" subdir-name))
             (package-subdir subdir-path))
        
        ;; Check if directory should be ignored
        (unless (and *current-ignore-patterns*
                     (stash-cl/package-mgmt:should-ignore-p subdir-name *current-ignore-patterns*))
          ;; Try to fold this subdirectory
          (if (can-fold-directory-p target-subdir package-subdir)
              (fold-directory target-subdir package-subdir)
            ;; Can't fold, need to descend
            (progn
              ;; If target is a symlink to another package, preserve that package's content
              (when (and (probe-file target-subdir)
                        (stash-cl/file-ops:file-is-symlink-p target-subdir))
                (let* ((original-target (read-symlink target-subdir))
                       ;; Ensure trailing slash for directory operations
                       (original-target-dir (if (char= (char original-target (1- (length original-target))) #\/)
                                                original-target
                                                (concatenate 'string original-target "/"))))
                  (when (>= *folding-verbosity* 2)
                    (format t "  → Unfolding and preserving ~A (was -> ~A)~%" target-subdir original-target))
                  ;; Create directory (this will remove the symlink automatically)
                  (stash-cl/task-planner:plan-create-dir target-subdir)
                  ;; Preserve original package's content (skip conflict checking since we're unfolding)
                  (stash-directory-contents-no-conflicts target-subdir original-target-dir)))
              
              ;; If not a symlink, just create directory if needed
              (unless (probe-file target-subdir)
                (stash-cl/task-planner:plan-create-dir target-subdir))
              
              ;; Stash new package's content
              (stash-directory-contents target-subdir package-subdir))))))))

;;; Refolding after unstash (ENHANCEMENT)

(defun can-refold-directory-p (target-path)
  "Check if TARGET-PATH can be refolded after unstashing a package.
This is an ENHANCEMENT over GNU Stow which doesn't actively refold."
  
  (unless (stash-cl/file-ops:file-is-directory-p target-path)
    (return-from can-refold-directory-p nil))
  
  ;; Get all entries in the directory
  (let ((entries (uiop:directory-files target-path)))
    
    ;; Can refold if:
    ;; 1. Directory has exactly one entry
    ;; 2. That entry is a symlink
    ;; 3. The symlink points to a stash package
    (and (= (length entries) 1)
         (let ((entry-path (namestring (first entries))))
           (and (stash-cl/file-ops:file-is-symlink-p entry-path)
                (symlink-points-to-stash-p entry-path))))))

(defun symlink-points-to-stash-p (symlink-path)
  "Check if SYMLINK-PATH points to a stash package directory."
  ;; Simple heuristic: check if path contains "stash" or ".stash"
  (let ((target (read-symlink symlink-path)))
    (or (search "stash" target :test #'char-equal)
        (search ".stash" target :test #'char-equal))))

(defun refold-directory (target-path)
  "Refold TARGET-PATH if possible after unstashing (ENHANCEMENT)."
  
  (when (can-refold-directory-p target-path)
    (let* ((entries (uiop:directory-files target-path))
           (single-entry (namestring (first entries)))
           (link-target (read-symlink single-entry)))
      
      (when (>= *folding-verbosity* 2)
        (format t "  ✓ Refolding ~A -> ~A~%" target-path link-target))
      
      ;; Remove the directory and its single symlink
      (stash-cl/task-planner:plan-remove-link single-entry)
      (stash-cl/task-planner:plan-remove-dir target-path)
      
      ;; Create directory symlink
      (stash-cl/task-planner:plan-create-link target-path link-target)
      
      (incf (folding-stats-directories-folded *folding-stats*))
      t)))

;;; Folding statistics and reporting (ENHANCEMENT)

(defun reset-folding-stats ()
  "Reset folding statistics."
  (setf *folding-stats* (make-folding-stats)))

(defun print-folding-stats ()
  "Print folding statistics (ENHANCEMENT)."
  (when (>= *folding-verbosity* 1)
    (let ((stats *folding-stats*))
      (format t "~%Folding Summary:~%")
      (format t "  Directories folded: ~D~%" 
              (folding-stats-directories-folded stats))
      (format t "  Directories unfolded: ~D~%" 
              (folding-stats-directories-unfolded stats))
      (when (> (folding-stats-directories-kept-folded stats) 0)
        (format t "  Subdirectories kept folded: ~D (smart partial folding)~%" 
                (folding-stats-directories-kept-folded stats)))
      (format t "  File symlinks created: ~D~%" 
              (folding-stats-file-symlinks-created stats))
      (format t "  Total symlinks: ~D~%"
              (+ (folding-stats-directories-folded stats)
                 (folding-stats-file-symlinks-created stats))))))

;;; High-level folding interface

(defparameter *current-ignore-patterns* nil
  "Ignore patterns for the current package being stashed.")

(defun stash-package-with-folding (package-path target-path)
  "Stash PACKAGE-PATH to TARGET-PATH with intelligent folding."
  
  (reset-folding-stats)
  
  ;; Read ignore patterns for this package
  (setf *current-ignore-patterns* 
        (stash-cl/package-mgmt:read-ignore-patterns package-path))
  
  (when (>= *folding-verbosity* 1)
    (format t "~%Analyzing package structure for optimal folding...~%"))
  
  ;; Always stash contents into target, never fold the target itself
  ;; The target directory is the stash target (e.g., /usr/local), not a package
  (stash-cl/task-planner:plan-create-dir target-path)
  (stash-directory-contents target-path package-path)
  
  (print-folding-stats))

(defun unstash-package-with-refolding (package-path target-path)
  "Unstash PACKAGE-PATH from TARGET-PATH with intelligent refolding (ENHANCEMENT)."
  
  (reset-folding-stats)
  
  (when (>= *folding-verbosity* 1)
    (format t "~%Unstashing package with refolding analysis...~%"))
  
  ;; Remove symlinks for this package by traversing the package structure
  (unstash-directory-contents package-path target-path)
  
  ;; After removal, check for refolding opportunities in parent directories
  (when (probe-file target-path)
    (refold-directory target-path))
  
  (print-folding-stats))

(defun unstash-directory-contents (package-dir target-dir)
  "Remove symlinks from TARGET-DIR that point to PACKAGE-DIR."
  
  (let ((files (uiop:directory-files package-dir))
        (subdirs (uiop:subdirectories package-dir)))
    
    ;; Handle files - remove symlinks that point to files in this package
    (dolist (file files)
      (let* ((file-path (namestring file))
             (file-name (file-namestring file-path))
             (target-file (concatenate 'string target-dir "/" file-name)))
        (when (and (probe-file target-file)
                   (stash-cl/file-ops:file-is-symlink-p target-file))
          (let ((link-target (read-symlink target-file)))
            (when (string= (namestring (truename link-target)) 
                          (namestring (truename file-path)))
              (stash-cl/task-planner:plan-remove-link target-file)
              (when (>= *folding-verbosity* 2)
                (format t "  ✓ Removing symlink ~A~%" target-file)))))))
    
    ;; Handle subdirectories
    (dolist (subdir subdirs)
      (let* ((subdir-path (namestring subdir))
             (subdir-name (file-namestring (string-right-trim "/" subdir-path)))
             (target-subdir (concatenate 'string target-dir "/" subdir-name)))
        
        (when (probe-file target-subdir)
          (cond
            ;; If target is a symlink to this package directory, remove it
            ((and (stash-cl/file-ops:file-is-symlink-p target-subdir)
                  (let ((link-target (read-symlink target-subdir)))
                    (string= (namestring (truename link-target))
                            (namestring (truename subdir-path)))))
             (stash-cl/task-planner:plan-remove-link target-subdir)
             (when (>= *folding-verbosity* 2)
               (format t "  ✓ Removing directory symlink ~A~%" target-subdir))
             (incf (folding-stats-directories-unfolded *folding-stats*)))
            
            ;; If target is a real directory, recurse into it
            ((stash-cl/file-ops:file-is-directory-p target-subdir)
             (unstash-directory-contents subdir-path target-subdir)
             ;; After removing contents, check if directory is now empty
             (when (and (probe-file target-subdir)
                       (null (uiop:directory-files target-subdir))
                       (null (uiop:subdirectories target-subdir)))
               (stash-cl/task-planner:plan-remove-dir target-subdir)
               (when (>= *folding-verbosity* 2)
                 (format t "  ✓ Removing empty directory ~A~%" target-subdir))))))))))
