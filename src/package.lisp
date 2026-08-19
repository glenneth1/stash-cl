;;;; package.lisp --- Package definitions for stash-cl

(defpackage #:stash-cl/colors
  (:use #:cl)
  (:export ;; CLOS terminal-style class
           #:terminal-style
           #:style-code
           #:style-name
           #:apply-style
           ;; Pre-defined style instances
           #:*style-red*
           #:*style-green*
           #:*style-yellow*
           #:*style-blue*
           #:*style-bold*
           ;; Configuration
           #:*use-colors*
           ;; Backward compatible functions
           #:color-red
           #:color-green
           #:color-yellow
           #:color-blue
           #:color-bold
           #:color-reset
           #:format-error
           #:format-warning
           #:format-success))

(defpackage #:stash-cl/log
  (:use #:cl)
  (:import-from #:stash-cl/colors)
  (:export ;; CLOS logger class
           #:logger
           #:logger-log-file
           #:logger-enabled
           #:log-message
           #:current-timestamp
           ;; Global logger instance
           #:*logger*
           ;; Backward compatible
           #:*log-file*
           #:log-action))

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
  (:export ;; CLOS conflict resolver classes
           #:conflict-resolver
           #:interactive-resolver
           #:auto-skip-resolver
           #:auto-overwrite-resolver
           #:auto-abort-resolver
           #:resolve-conflict
           ;; Global resolver
           #:*conflict-resolver*
           ;; Backward compatible
           #:prompt-user-for-action
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
  (:export ;; CLOS file operation classes
           #:file-operation
           #:operation-path
           #:execute-operation
           #:operation-description
           #:symlink-operation
           #:operation-source
           #:mkdir-operation
           #:move-operation
           #:operation-destination
           #:delete-operation
           ;; File type predicates
           #:file-is-symlink-p
           #:file-is-directory-p
           #:file-is-regular-p
           #:read-symlink
           ;; Low-level operations
           #:move-source-to-target
           #:create-symlink
           #:delete-directory
           #:mkdir-p
           ;; Backward compatible
           #:execute-operations))

(defpackage #:stash-cl/package-mgmt
  (:use #:cl)
  (:export ;; CLOS stash-package class
           #:stash-package
           #:stash-package-name
           #:stash-package-path
           #:stash-package-target
           #:stash-package-ignore-patterns
           #:load-ignore-patterns
           ;; Backward compatible accessors
           #:make-package-info
           #:package-info-p
           #:package-info-name
           #:package-info-path
           #:package-info-target
           #:package-info-ignore-patterns
           ;; Functions
           #:read-ignore-patterns
           #:should-ignore-p
           #:should-ignore-with-overrides-p))

(defpackage #:stash-cl/tree
  (:use #:cl)
  (:import-from #:stash-cl/package-mgmt
                #:package-info-path
                #:package-info-ignore-patterns
                #:should-ignore-p)
  (:import-from #:stash-cl/file-ops
                #:file-is-symlink-p
                #:file-is-directory-p)
  (:export ;; CLOS tree-node class
           #:tree-node
           #:tree-node-path
           #:tree-node-type
           #:tree-node-children
           ;; Backward compatible
           #:make-tree-node
           #:tree-node-p
           ;; Functions
           #:analyze-tree
           #:plan-operations))

(defpackage #:stash-cl/task-planner
  (:use #:cl)
  (:import-from #:stash-cl/file-ops
                #:create-symlink
                #:delete-directory
                #:mkdir-p)
  (:export ;; Planner functions
           #:init-planner
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
           ;; CLOS task classes
           #:task
           #:create-task
           #:remove-task
           #:link-task
           #:dir-task
           #:create-link-task
           #:remove-link-task
           #:create-dir-task
           #:remove-dir-task
           #:move-task
           ;; Generic functions
           #:execute-task
           #:validate-task
           #:task-action
           #:task-type
           #:task-path
           #:task-source
           #:task-dest
           ;; CLOS planner class
           #:task-planner
           #:planner-tasks
           #:planner-conflicts
           #:planner-stash-path
           #:planner-target-dir
           ;; Backward compatibility
           #:planner-state-tasks
           #:planner-state-conflicts
           #:*planner-state*
           ;; Conditions
           #:conflict-error
           #:conflict-error-message
           #:conflict-error-path))

(defpackage #:stash-cl/folding
  (:use #:cl)
  (:import-from #:stash-cl/file-ops
                #:file-is-symlink-p
                #:file-is-directory-p
                #:read-symlink)
  (:import-from #:stash-cl/task-planner
                #:plan-create-link
                #:plan-remove-link
                #:plan-create-dir
                #:plan-remove-dir
                #:add-conflict)
  (:import-from #:alexandria
                #:starts-with-subseq)
  (:export ;; CLOS folding context class
           #:folding-context
           #:*folding-context*
           #:folding-enabled
           #:folding-verbosity
           #:folding-ignore-patterns
           #:folding-directories-folded
           #:folding-directories-unfolded
           #:folding-directories-kept-folded
           #:folding-file-symlinks-created
           ;; Backward compatible variables
           #:*folding-enabled*
           #:*folding-verbosity*
           ;; Functions
           #:can-fold-directory-p
           #:fold-directory
           #:unfold-directory
           #:refold-directory
           #:stash-package-with-folding
           #:unstash-package-with-refolding
           #:reset-folding-stats
           #:print-folding-stats))

(defpackage #:stash-cl/config
  (:use #:cl)
  (:export #:stash-config
           #:stash-config-dir
           #:stash-config-target
           #:stash-config-source
           #:stash-config-verbose
           #:stash-config-no-folding
           #:stash-config-ignore-patterns
           #:stash-config-defer-patterns
           #:stash-config-override-patterns
           #:load-config-file
           #:merge-config-with-options
           #:find-config-file
           #:config-file-paths
           #:parse-config-line))

(defpackage #:stash-cl/help
  (:use #:cl)
  (:export #:display-help
           #:display-version
           #:display-completion))

(defpackage #:stash-cl
  (:use #:cl)
  (:import-from #:stash-cl/help
                #:display-help
                #:display-version
                #:display-completion)
  (:import-from #:stash-cl/config
                #:load-config-file
                #:merge-config-with-options
                #:find-config-file)
  (:import-from #:stash-cl/file-ops
                #:file-is-regular-p
                #:file-is-directory-p
                #:file-is-symlink-p
                #:create-symlink
                #:mkdir-p
                #:read-symlink)
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
  (:export #:main
           #:toplevel-entry))
