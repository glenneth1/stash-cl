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
  -l, --list            List all packages and their stashed status
  -D, --delete          Unstash packages
  -R, --restash         Restash packages (unstash then stash)
  -i, --import=PATH     Import existing file/directory into a package
  -p, --package=NAME    Package name (used with --import)
  -n, --simulate        Simulation mode (dry-run)
  --adopt               Adopt existing files into package
  --conflicts           List all conflicts without taking action
  -I, --interactive     Interactive mode (prompt on conflicts)
  --ignore=REGEX        Ignore pattern (can be used multiple times)
  --defer=REGEX         Defer pattern - skip files matching REGEX
  --override=REGEX      Override pattern - force stow files matching REGEX
  --no-folding          Disable tree folding
  --completion=SHELL    Output shell completion script (bash/zsh/fish)
  -v, --verbose         Increase verbosity (-v, -vv, -vvv)
  -h, --help            Display this help
  -V, --version         Display version

Examples:
  cd ~/.dotfiles && stash emacs         # Stash emacs package
  stash -D emacs                        # Unstash emacs
  stash -l                              # List all packages and status
  stash -R emacs                        # Restash emacs
  stash -n vim                          # Simulate stashing vim
  stash -vv perl                        # Stash with verbose output
  stash --adopt emacs                   # Adopt existing files
  stash --adopt -I emacs                # Adopt interactively
  stash --conflicts emacs               # Check for conflicts only
  stash --ignore='.*\\.bak' vim         # Ignore .bak files

Import (create package from existing files):
  stash --import ~/.bashrc --package bash --dir ~/.files --target ~
  stash -i ~/.config/nvim -p neovim --dir ~/.files --target ~

Shell completion:
  eval \"$(stash --completion=bash)\"   # Bash
  eval \"$(stash --completion=zsh)\"    # Zsh
  eval \"$(stash --completion=fish)\"   # Fish

Configuration:
  Config file: ~/.config/stash/config or ~/.stashrc
  Set defaults: dir, target, verbose, no-folding, ignore, defer, override
"))

(defun display-version ()
  "Display version information."
  (format t "stash-cl version 0.3.0~%")
  (format t "Common Lisp rewrite of GNU Stow replacement~%"))

(defun display-completion (shell)
  "Output shell completion script for SHELL (bash, zsh, or fish)."
  (cond
    ((string= shell "bash")
     (princ "_stash()
{
    local cur prev opts
    COMPREPLY=()
    cur=${COMP_WORDS[COMP_CWORD]}
    prev=${COMP_WORDS[COMP_CWORD-1]}

    opts='-s --source -t --target --dir -d --deploy -l --list -D --delete
-R --restash -i --import -p --package -n --simulate --adopt --conflicts
-I --interactive --ignore --defer --override --no-folding --completion
-v --verbose -h --help -V --version'

    if [[ $cur == -* ]]; then
        COMPREPLY=( $(compgen -W \"$opts\" -- $cur) )
        return 0
    fi

    case $prev in
        --dir|--target|--source|-s|-t)
            COMPREPLY=( $(compgen -d -- $cur) )
            return 0
            ;;
        --import|-i)
            COMPREPLY=( $(compgen -f -- $cur) )
            return 0
            ;;
        --completion)
            COMPREPLY=( $(compgen -W 'bash zsh fish' -- $cur) )
            return 0
            ;;
    esac

    # Complete package names from stash directory
    local stash_dir
    stash_dir=$(pwd)
    if [ -d \"$stash_dir\" ]; then
        local packages
        packages=$(ls -d $stash_dir/*/ 2>/dev/null | xargs -n1 basename 2>/dev/null)
        if [ -n \"$packages\" ]; then
            COMPREPLY=( $(compgen -W \"$packages\" -- $cur) )
        fi
    fi
}
complete -F _stash stash
"))
    ((string= shell "zsh")
     (princ "#compdef stash

_stash() {
    local -a opts packages

    opts=(
        '-s[Source directory]:dir:_files -/'
        '--source[Source directory]:dir:_files -/'
        '-t[Target directory]:dir:_files -/'
        '--target[Target directory]:dir:_files -/'
        '--dir[Stash directory]:dir:_files -/'
        '-d[Deploy mode]'
        '--deploy[Deploy mode]'
        '-l[List packages]'
        '--list[List packages]'
        '-D[Unstash packages]'
        '--delete[Unstash packages]'
        '-R[Restash packages]'
        '--restash[Restash packages]'
        '-i[Import file]:file:_files'
        '--import[Import file]:file:_files'
        '-p[Package name]:package'
        '--package[Package name]:package'
        '-n[Simulation mode]'
        '--simulate[Simulation mode]'
        '--adopt[Adopt existing files]'
        '--conflicts[List conflicts only]'
        '-I[Interactive mode]'
        '--interactive[Interactive mode]'
        '--ignore[Ignore pattern]:regex'
        '--defer[Defer pattern]:regex'
        '--override[Override pattern]:regex'
        '--no-folding[Disable tree folding]'
        '--completion[Shell completion]:shell:(bash zsh fish)'
        '-v[Verbose]'
        '--verbose[Verbose]'
        '-h[Help]'
        '--help[Help]'
        '-V[Version]'
        '--version[Version]'
    )

    _arguments -C $opts
}

_stash \"$@\"
"))
    ((string= shell "fish")
     (princ "complete -c stash -s h -l help -d 'Display help'
complete -c stash -s V -l version -d 'Display version'
complete -c stash -s s -l source -d 'Source directory' -r
complete -c stash -s t -l target -d 'Target directory' -r
complete -c stash -l dir -d 'Stash directory' -r
complete -c stash -s d -l deploy -d 'Deploy mode'
complete -c stash -s l -l list -d 'List packages'
complete -c stash -s D -l delete -d 'Unstash packages'
complete -c stash -s R -l restash -d 'Restash packages'
complete -c stash -s i -l import -d 'Import file' -r
complete -c stash -s p -l package -d 'Package name' -r
complete -c stash -s n -l simulate -d 'Simulation mode'
complete -c stash -l adopt -d 'Adopt existing files'
complete -c stash -l conflicts -d 'List conflicts only'
complete -c stash -s I -l interactive -d 'Interactive mode'
complete -c stash -l ignore -d 'Ignore pattern' -r
complete -c stash -l defer -d 'Defer pattern' -r
complete -c stash -l override -d 'Override pattern' -r
complete -c stash -l no-folding -d 'Disable tree folding'
complete -c stash -l completion -d 'Shell completion' -x -a 'bash zsh fish'
complete -c stash -s v -l verbose -d 'Verbose'
"))
    (t
     (format t "Unknown shell: ~A. Supported: bash, zsh, fish~%" shell))))
