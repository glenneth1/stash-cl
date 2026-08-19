;;;; test-unit.lisp --- Unit tests for stash-cl core functions
;;;; Run with: sbcl --non-interactive --load test-unit.lisp

(load "stash-cl.asd")
(ql:quickload :stash-cl)

(defpackage #:stash-cl/test
  (:use #:cl)
  (:import-from #:stash-cl/config
                #:parse-config-line
                #:load-config-file
                #:stash-config-dir
                #:stash-config-target
                #:stash-config-verbose
                #:stash-config-no-folding
                #:stash-config-ignore-patterns
                #:stash-config-defer-patterns
                #:stash-config-override-patterns)
  (:import-from #:stash-cl/paths
                #:expand-home)
  (:import-from #:stash-cl/file-ops
                #:file-is-symlink-p
                #:file-is-directory-p
                #:file-is-regular-p)
  (:import-from #:stash-cl/task-planner
                #:init-planner
                #:reset-planner
                #:add-task
                #:get-tasks
                #:get-task-count
                #:add-conflict
                #:get-conflicts
                #:get-conflict-count
                #:has-conflicts-p
                #:validate-all-tasks
                #:plan-create-link
                #:plan-remove-link
                #:plan-create-dir
                #:*planner-state*))

(in-package #:stash-cl/test)

(defvar *test-count* 0)
(defvar *pass-count* 0)
(defvar *fail-count* 0)
(defvar *failures* nil)

(defmacro test (name &body body)
  `(progn
     (incf *test-count*)
     (handler-case
         (progn ,@body)
       (error (e)
         (incf *fail-count*)
         (push (format nil "~A: ERROR - ~A" ,name e) *failures*)))))

(defun assert-equal (expected actual name)
  (if (equal expected actual)
      (incf *pass-count*)
      (progn
        (incf *fail-count*)
        (push (format nil "~A: expected ~A, got ~A" name expected actual) *failures*))))

(defun assert-true (value name)
  (if value
      (incf *pass-count*)
      (progn
        (incf *fail-count*)
        (push (format nil "~A: expected non-NIL, got NIL" name) *failures*))))

(defun assert-false (value name)
  (if (not value)
      (incf *pass-count*)
      (progn
        (incf *fail-count*)
        (push (format nil "~A: expected NIL, got ~A" name value) *failures*))))

(defun print-results ()
  (format t "~%~%=========================================~%")
  (format t "  Unit Test Results~%")
  (format t "=========================================~%")
  (format t "  Total:  ~D~%" *test-count*)
  (format t "  Passed: ~D~%" *pass-count*)
  (format t "  Failed: ~D~%" *fail-count*)
  (when *failures*
    (format t "~%Failures:~%")
    (dolist (f (reverse *failures*))
      (format t "  - ~A~%" f)))
  (format t "=========================================~%~%")
  (if (= *fail-count* 0)
      (uiop:quit 0)
      (uiop:quit 1)))

;;; --- Config file parsing tests ---

(test "parse-config-line: key=value"
  (assert-equal (cons :dir "/home/user/dotfiles")
                (parse-config-line "dir = /home/user/dotfiles")
                "parse-config-line dir"))

(test "parse-config-line: key=value no spaces"
  (assert-equal (cons :target "/home/user")
                (parse-config-line "target=/home/user")
                "parse-config-line target no spaces"))

(test "parse-config-line: comment returns NIL"
  (assert-false (parse-config-line "# this is a comment")
                "parse-config-line comment"))

(test "parse-config-line: empty line returns NIL"
  (assert-false (parse-config-line "")
                "parse-config-line empty"))

(test "parse-config-line: whitespace only returns NIL"
  (assert-false (parse-config-line "   ")
                "parse-config-line whitespace"))

(test "parse-config-line: key with extra spaces"
  (assert-equal (cons :verbose "2")
                (parse-config-line "  verbose  =  2  ")
                "parse-config-line verbose"))

(test "parse-config-line: no equals returns NIL"
  (assert-false (parse-config-line "just a line")
                "parse-config-line no equals"))

(test "load-config-file: no file returns defaults"
  (let ((config (load-config-file "/nonexistent/path/config")))
    (assert-false (stash-config-dir config) "config no file dir")
    (assert-false (stash-config-target config) "config no file target")))

(test "load-config-file: with temp file"
  (let ((tmpfile "/tmp/stash-test-config"))
    (with-open-file (s tmpfile :direction :output :if-exists :supersede)
      (write-line "dir = /test/dir" s)
      (write-line "target = /test/target" s)
      (write-line "verbose = 2" s)
      (write-line "no-folding = true" s)
      (write-line "ignore = .*\\.bak" s)
      (write-line "ignore = .*\\.tmp" s)
      (write-line "defer = .*\\.cache" s)
      (write-line "override = important" s)
      (write-line "# comment line" s)
      (write-line "" s))
    (let ((config (load-config-file tmpfile)))
      (assert-equal "/test/dir" (stash-config-dir config) "config dir")
      (assert-equal "/test/target" (stash-config-target config) "config target")
      (assert-equal 2 (stash-config-verbose config) "config verbose")
      (assert-true (stash-config-no-folding config) "config no-folding")
      (assert-equal 2 (length (stash-config-ignore-patterns config)) "config ignore count")
      (assert-equal 1 (length (stash-config-defer-patterns config)) "config defer count")
      (assert-equal 1 (length (stash-config-override-patterns config)) "config override count"))
    (delete-file tmpfile)))

;;; --- Path utility tests ---

(test "expand-home: tilde expansion"
  (let ((expanded (expand-home "~/test")))
    (assert-true (> (length expanded) 5) "expand-home tilde")
    (assert-false (char= (char expanded 0) #\~) "expand-home no tilde")))

(test "expand-home: no tilde returns as-is"
  (assert-equal "/absolute/path" (expand-home "/absolute/path") "expand-home absolute"))

(test "expand-home: empty string"
  (assert-equal "" (expand-home "") "expand-home empty"))

;;; --- File operation predicate tests ---

(test "file-is-directory-p: on directory"
  (assert-true (file-is-directory-p "test-env/") "file-is-directory-p on dir"))

(test "file-is-directory-p: on file returns NIL"
  (assert-false (file-is-directory-p "Makefile") "file-is-directory-p on file"))

(test "file-is-regular-p: on regular file"
  (assert-true (file-is-regular-p "Makefile") "file-is-regular-p on file"))

(test "file-is-regular-p: on directory returns NIL"
  (assert-false (file-is-regular-p "test-env/") "file-is-regular-p on dir"))

;;; --- Task planner tests ---

(test "init-planner: creates planner state"
  (init-planner "/test/stash" "/test/target")
  (assert-true stash-cl/task-planner::*planner-state* "planner state exists"))

(test "reset-planner: clears state"
  (init-planner "/test/stash" "/test/target")
  (add-task :create :dir "/test/dir1")
  (reset-planner)
  (assert-equal 0 (get-task-count) "reset task count"))

(test "add-task: increments task count"
  (init-planner "/test/stash" "/test/target")
  (add-task :create :dir "/test/dir1")
  (add-task :create :dir "/test/dir2")
  (assert-equal 2 (get-task-count) "add-task count"))

(test "add-conflict: increments conflict count"
  (init-planner "/test/stash" "/test/target")
  (add-conflict "test" "file conflict" "/test/path")
  (add-conflict "test" "dir conflict" "/test/dir")
  (assert-equal 2 (get-conflict-count) "add-conflict count"))

(test "has-conflicts-p: true with conflicts"
  (init-planner "/test/stash" "/test/target")
  (add-conflict "test" "conflict" "/test/path")
  (assert-true (has-conflicts-p) "has-conflicts-p true"))

(test "has-conflicts-p: false without conflicts"
  (init-planner "/test/stash" "/test/target")
  (assert-false (has-conflicts-p) "has-conflicts-p false"))

(test "get-conflicts: returns conflicts in order"
  (init-planner "/test/stash" "/test/target")
  (add-conflict "pkg1" "first" "/path1")
  (add-conflict "pkg2" "second" "/path2")
  (let ((conflicts (get-conflicts)))
    (assert-equal 2 (length conflicts) "get-conflicts length")
    (assert-equal "first" (getf (first conflicts) :message) "get-conflicts first")))

(test "plan-create-dir: adds task"
  (init-planner "/test/stash" "/test/target")
  (plan-create-dir "/test/newdir")
  (assert-equal 1 (get-task-count) "plan-create-dir task count"))

(test "plan-remove-link: adds task"
  (init-planner "/test/stash" "/test/target")
  (plan-remove-link "/test/link")
  (assert-equal 1 (get-task-count) "plan-remove-link task count"))

(test "validate-all-tasks: empty returns T"
  (init-planner "/test/stash" "/test/target")
  (assert-true (validate-all-tasks) "validate empty"))

;;; --- Run tests ---

(print-results)
