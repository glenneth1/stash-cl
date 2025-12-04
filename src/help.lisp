;;;; help.lisp --- Help and version information for stash-cl

(in-package #:stash-cl/help)

(defun display-help ()
  "Display help message explaining how to use stash-cl."
  (princ "Usage: stash [OPTION...] [PACKAGE|.]

Enhanced dotfile and package manager with intelligent tree folding.

Options:
  -s, --source=DIR      Source directory to stash
  -t, --target=DIR      Target directory
  --dir=DIR             Stash directory (default: current)
  -d, --deploy          Deploy mode
  -D, --delete          Unstash packages
  -R, --restash         Restash packages (unstash then stash)
  -n, --simulate        Simulation mode (dry-run)
  --adopt               Adopt existing files into package
  --ignore=REGEX        Ignore pattern (can be used multiple times)
  --no-folding          Disable tree folding
  -v, --verbose         Increase verbosity (-v, -vv, -vvv)
  -h, --help            Display this help
  --version             Display version

Examples:
  cd ~/.dotfiles && stash emacs         # Stash emacs package
  stash -D emacs                        # Unstash emacs
  stash -R emacs                        # Restash emacs
  stash -n vim                          # Simulate stashing vim
  stash -vv perl                        # Stash with verbose output
  stash --adopt emacs                   # Adopt existing files
  stash --ignore='.*\\.bak' vim         # Ignore .bak files
"))

(defun display-version ()
  "Display version information."
  (format t "stash-cl version 0.1.0~%")
  (format t "Common Lisp rewrite of GNU Stow replacement~%"))
