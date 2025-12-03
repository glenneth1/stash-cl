;;;; tree.lisp --- Tree analysis for stash-cl

(in-package #:stash-cl/tree)

(defstruct tree-node
  "Node in a directory tree."
  path
  type  ; :file or :directory
  children)

(defun analyze-tree (package-info)
  "Analyze the directory tree of PACKAGE-INFO."
  (let ((root-path (package-info-path package-info))
        (ignore-patterns (package-info-ignore-patterns package-info)))
    (analyze-directory root-path ignore-patterns)))

(defun analyze-directory (dir-path ignore-patterns)
  "Recursively analyze directory at DIR-PATH."
  (let ((children nil))
    
    ;; Analyze files
    (dolist (file (uiop:directory-files dir-path))
      (let ((filename (file-namestring file)))
        (unless (should-ignore-p filename ignore-patterns)
          (push (make-tree-node :path (namestring file)
                               :type :file
                               :children nil)
                children))))
    
    ;; Analyze subdirectories
    (dolist (subdir (uiop:subdirectories dir-path))
      (let ((dirname (car (last (pathname-directory subdir)))))
        (unless (should-ignore-p dirname ignore-patterns)
          (push (analyze-directory (namestring subdir) ignore-patterns)
                children))))
    
    (make-tree-node :path dir-path
                   :type :directory
                   :children (nreverse children))))

(defun plan-operations (tree-node target-base)
  "Plan operations to deploy TREE-NODE to TARGET-BASE."
  (let ((operations nil))
    (labels ((process-node (node target-path)
               (case (tree-node-type node)
                 (:file
                  (push (list :create-symlink target-path (tree-node-path node))
                        operations))
                 (:directory
                  (push (list :mkdir target-path) operations)
                  (dolist (child (tree-node-children node))
                    (let ((child-name (file-namestring (tree-node-path child)))
                          (child-target (concatenate 'string target-path "/" 
                                                    (file-namestring (tree-node-path child)))))
                      (process-node child child-target)))))))
      
      (process-node tree-node target-base))
    
    (nreverse operations)))
