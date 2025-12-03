;;;; package.lisp --- Package definitions for stash-cl

(defpackage #:stash-cl/colors
  (:use #:cl)
  (:export #:*use-colors*
           #:color-red
           #:color-green
           #:color-yellow
           #:color-blue
           #:color-reset))

(defpackage #:stash-cl/log
  (:use #:cl)
  (:import-from #:stash-cl/colors)
  (:export #:*log-file*
           #:log-action
           #:current-timestamp))

(defpackage #:stash-cl/paths
  (:use #:cl)
  (:import-from #:alexandria
                #:starts-with-subseq)
  (:export #:expand-home
           #:concat-path
           #:ensure-config-path
           #:canonicalize-path))

(defpackage #:stash-cl/conflict
  (:use #:cl)
  (:import-from #:stash-cl/log
                #:log-action)
  (:export #:prompt-user-for-action
           #:handle-conflict))

(defpackage #:stash-cl/file-ops
  (:use #:cl)
  (:import-from #:stash-cl/log
                #:log-action)
  (:import-from #:stash-cl/paths
                #:expand-home
                #:concat-path
                #:ensure-config-path)
  (:import-from #:stash-cl/conflict
                #:handle-conflict)
  (:export #:move-source-to-target
           #:create-symlink
           #:delete-directory
           #:mkdir-p
           #:execute-operations
           #:file-is-symlink-p
           #:file-is-directory-p
           #:file-is-regular-p))

(defpackage #:stash-cl/package-mgmt
  (:use #:cl)
  (:export #:make-package-info
           #:package-info-p
           #:package-info-name
           #:package-info-path
           #:package-info-target
           #:package-info-ignore-patterns
           #:read-ignore-patterns
           #:should-ignore-p))

(defpackage #:stash-cl/tree
  (:use #:cl)
  (:import-from #:stash-cl/package-mgmt
                #:package-info-path
                #:package-info-ignore-patterns
                #:should-ignore-p)
  (:import-from #:stash-cl/file-ops
                #:file-is-symlink-p
                #:file-is-directory-p)
  (:export #:make-tree-node
           #:tree-node-p
           #:tree-node-path
           #:tree-node-type
           #:tree-node-children
           #:analyze-tree
           #:plan-operations))

(defpackage #:stash-cl/task-planner
  (:use #:cl)
  (:import-from #:stash-cl/file-ops
                #:create-symlink
                #:delete-directory
                #:mkdir-p)
  (:export #:init-planner
           #:reset-planner
           #:add-task
           #:find-dir-task
           #:find-link-task
           #:add-conflict
           #:has-conflicts-p
           #:get-conflicts
           #:get-conflict-count
           #:validate-all-tasks
           #:execute-all-tasks
           #:get-tasks
           #:get-task-count
           #:print-task-summary
           #:plan-create-link
           #:plan-remove-link
           #:plan-create-dir
           #:plan-remove-dir
           #:plan-move-file
           #:task
           #:make-task
           #:task-action
           #:task-type
           #:task-path
           #:task-source
           #:task-dest
           #:planner-state
           #:*planner-state*))

(defpackage #:stash-cl/folding
  (:use #:cl)
  (:import-from #:stash-cl/file-ops
                #:file-is-symlink-p
                #:file-is-directory-p)
  (:import-from #:stash-cl/task-planner
                #:plan-create-link
                #:plan-remove-link
                #:plan-create-dir
                #:plan-remove-dir
                #:add-conflict)
  (:import-from #:alexandria
                #:starts-with-subseq)
  (:export #:*folding-enabled*
           #:*folding-verbosity*
           #:can-fold-directory-p
           #:fold-directory
           #:unfold-directory
           #:refold-directory
           #:stash-package-with-folding
           #:unstash-package-with-refolding
           #:reset-folding-stats
           #:print-folding-stats))

(defpackage #:stash-cl/help
  (:use #:cl)
  (:export #:display-help
           #:display-version))

(defpackage #:stash-cl
  (:use #:cl)
  (:import-from #:stash-cl/help
                #:display-help
                #:display-version)
  (:import-from #:stash-cl/file-ops
                #:file-is-regular-p
                #:file-is-directory-p
                #:file-is-symlink-p
                #:create-symlink
                #:mkdir-p)
  (:import-from #:stash-cl/paths
                #:canonicalize-path
                #:expand-home)
  (:import-from #:stash-cl/package-mgmt
                #:make-package-info
                #:read-ignore-patterns)
  (:import-from #:stash-cl/tree
                #:analyze-tree
                #:plan-operations)
  (:import-from #:stash-cl/task-planner
                #:init-planner
                #:reset-planner
                #:add-conflict
                #:has-conflicts-p
                #:get-conflicts
                #:execute-all-tasks
                #:print-task-summary
                #:plan-create-link
                #:plan-remove-link
                #:plan-create-dir
                #:plan-remove-dir)
  (:import-from #:stash-cl/folding
                #:*folding-enabled*
                #:*folding-verbosity*
                #:stash-package-with-folding
                #:unstash-package-with-refolding
                #:print-folding-stats)
  (:export #:main))
