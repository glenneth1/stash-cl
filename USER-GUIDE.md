# Stash-CL User Guide

## Table of Contents

1. [Introduction](#introduction)
2. [Installation](#installation)
3. [Quick Start](#quick-start)
4. [Basic Concepts](#basic-concepts)
5. [Command Reference](#command-reference)
6. [Package Structure](#package-structure)
7. [Ignore Patterns](#ignore-patterns)
8. [Advanced Usage](#advanced-usage)
9. [Troubleshooting](#troubleshooting)
10. [Examples](#examples)

---

## Introduction

**Stash-CL** is a symlink farm manager inspired by GNU Stow, written in Common Lisp. It helps you manage dotfiles, software installations, and configuration files by creating symbolic links from a central package directory to a target directory (typically your home directory or `/usr/local`).

### Key Features

- **Intelligent Tree Folding**: Automatically creates directory symlinks when possible, reducing clutter
- **Multiple Package Support**: Install multiple packages with overlapping directories
- **Conflict Detection**: Detects and reports conflicts before making changes
- **Simulation Mode**: Preview changes without modifying your system
- **Ignore Patterns**: Exclude files using glob patterns or regex
- **Defer Patterns**: Skip files matching a regex during a single operation
- **Override Patterns**: Force-include files that would otherwise be ignored or deferred
- **Task Planning**: Validates all operations before executing
- **Deploy Mode**: Install all packages at once
- **Import Mode**: Create packages from existing files with automatic symlink creation
- **List Mode**: Show all packages and their stashing status
- **Configuration File**: Set defaults in ~/.config/stash/config or ~/.stashrc
- **Shell Completion**: Built-in bash, zsh, and fish completion
- **Conflict Checking**: List conflicts without taking action with --conflicts
- **Interactive Mode**: Prompt on conflicts with diff display with --interactive
- **Progress Indicators**: Progress tracking during operations

### Why Stash-CL?

- **Smarter than GNU Stow**: Intelligent folding reduces symlink count
- **Safer**: Conflict detection and simulation mode prevent accidents
- **Cleaner**: Fewer symlinks = cleaner filesystem
- **Faster**: Written in compiled Common Lisp for performance

---

## Installation

### Prerequisites

- SBCL (Steel Bank Common Lisp) 2.0 or later
- Quicklisp (for dependencies)
- Git

### Building from Source

```bash
git clone https://github.com/glenneth1/stash-cl.git
cd stash-cl
make
```

This creates:
- `./stash` - Compressed standalone executable (~16MB)

### Installing System-Wide

Use the provided Makefile target:

```bash
sudo make install
```

This installs `stash` to `/usr/local/bin/` and the man page to `/usr/local/share/man/man1/`.

Or manually:

```bash
sudo cp stash /usr/local/bin/
sudo cp stash.1 /usr/local/share/man/man1/
```

### Uninstalling

```bash
sudo make uninstall
```

---

## Quick Start

### 1. Create a Package Directory

```bash
mkdir -p ~/dotfiles/vim
```

### 2. Add Files to Your Package

```bash
# Create vim configuration
mkdir -p ~/dotfiles/vim/.vim
echo 'set number' > ~/dotfiles/vim/.vimrc
```

### 3. Stash the Package

```bash
stash --dir ~/dotfiles --target ~ vim
```

This creates:
- `~/.vimrc` → `~/dotfiles/vim/.vimrc`
- `~/.vim/` → `~/dotfiles/vim/.vim/`

### 4. Verify

```bash
ls -la ~/.vimrc
# lrwxrwxrwx ... .vimrc -> /home/user/dotfiles/vim/.vimrc
```

### 5. Import Existing Files

Instead of creating packages manually, you can import existing files:

```bash
stash --import ~/.bashrc --package bash --dir ~/dotfiles --target ~
```

This moves the file into the package, creates the package directory, and symlinks
the original location. See [Import](#import-create-package-from-existing-files) below.

---

## Basic Concepts

### Package

A **package** is a directory containing files and subdirectories you want to symlink. Each package represents a logical unit (e.g., vim, emacs, bash).

```
dotfiles/
├── vim/
│   ├── .vimrc
│   └── .vim/
├── bash/
│   └── .bashrc
└── emacs/
    └── .emacs.d/
```

### Target Directory

The **target directory** is where symlinks are created. Common targets:
- `~` (home directory) for dotfiles
- `/usr/local` for software installations
- `/etc` for system configuration (requires sudo)

### Stash Directory

The **stash directory** contains all your packages. Default is current directory, but you can specify with `--dir`.

### Tree Folding

**Folding** creates directory symlinks instead of individual file symlinks when possible:

**Without folding** (GNU Stow style):
```
~/.vim/colors/molokai.vim -> ~/dotfiles/vim/.vim/colors/molokai.vim
~/.vim/syntax/lisp.vim   -> ~/dotfiles/vim/.vim/syntax/lisp.vim
```

**With folding** (Stash-CL):
```
~/.vim/ -> ~/dotfiles/vim/.vim/
```

### Unfolding

When multiple packages share a directory, Stash-CL **unfolds** it:

```bash
stash vim    # Creates: ~/.vim/ -> vim/.vim/
stash emacs  # Unfolds: ~/.vim/ becomes real directory
             # Creates: ~/.vim/vim/ -> vim/.vim/
             #          ~/.vim/emacs/ -> emacs/.vim/
```

---

## Command Reference

### Basic Commands

#### Stash (Install)

```bash
stash [OPTIONS] PACKAGE [PACKAGE...]
```

Install one or more packages.

**Examples:**
```bash
stash vim                              # Stash vim package
stash vim bash emacs                   # Stash multiple packages
stash --dir ~/dotfiles --target ~ vim  # Specify directories
```

#### Unstash (Uninstall)

```bash
stash -D [OPTIONS] PACKAGE [PACKAGE...]
stash --delete [OPTIONS] PACKAGE [PACKAGE...]
```

Remove symlinks for one or more packages.

**Examples:**
```bash
stash -D vim                    # Unstash vim
stash --delete vim bash emacs   # Unstash multiple packages
```

#### Restash (Reinstall)

```bash
stash -R [OPTIONS] PACKAGE [PACKAGE...]
stash --restash [OPTIONS] PACKAGE [PACKAGE...]
```

Unstash then stash (useful after package updates).

**Examples:**
```bash
stash -R vim                    # Restash vim
stash --restash vim bash        # Restash multiple packages
```

#### Import (Create Package from Existing Files)

```bash
stash -i PATH -p NAME [OPTIONS]
stash --import PATH --package NAME [OPTIONS]
```

Import an existing file or directory into a new package. This moves the
file/directory into the package and creates a symlink back to the original location.

**Examples:**
```bash
stash -i ~/.bashrc -p bash --dir ~/.dotfiles --target ~
stash --import ~/.config/nvim --package neovim --dir ~/.dotfiles --target ~
stash -n -i ~/.vimrc -p vim --dir ~/.dotfiles --target ~  # Simulate first
```

#### Deploy (Install All)

```bash
stash -d [OPTIONS]
stash --deploy [OPTIONS]
```

Install all packages in the stash directory.

**Examples:**
```bash
stash -d                              # Deploy all packages
stash -d --dir ~/dotfiles --target ~  # Deploy with custom paths
```

#### List (Show Status)

```bash
stash -l [OPTIONS]
stash --list [OPTIONS]
```

List all packages in the stash directory and show their stashing status.

**Examples:**
```bash
stash -l                              # List all packages
stash -l --dir ~/dotfiles --target ~  # List with custom paths
```

#### Adopt (Move Existing Files)

```bash
stash --adopt [OPTIONS] PACKAGE [PACKAGE...]
```

Move existing files in the target directory into the package directory, then
create symlinks. Useful for converting existing configurations into managed
packages.

**Examples:**
```bash
stash --adopt vim                     # Adopt existing vim files
stash -n --adopt vim                  # Simulate adoption first
stash --adopt -I vim                  # Interactive adoption (prompt per file)
```

#### Conflicts (Check Without Action)

```bash
stash --conflicts [OPTIONS] PACKAGE [PACKAGE...]
```

List all conflicts that would occur when stashing the specified packages
without actually performing any action. Useful for checking before stashing.

**Examples:**
```bash
stash --conflicts vim                 # Check conflicts for vim
stash --conflicts emacs bash          # Check conflicts for multiple packages
stash --dir ~/dotfiles --target ~ --conflicts vim  # With explicit paths
```

#### Interactive Mode

```bash
stash -I [OPTIONS] PACKAGE [PACKAGE...]
stash --interactive [OPTIONS] PACKAGE [PACKAGE...]
```

Interactive mode prompts the user when conflicts are detected, offering
options to skip, simulate, or abort. When used with --adopt, prompts for
each file with options to adopt, skip, or abort, and shows diffs before
adopting.

**Examples:**
```bash
stash -I vim                          # Interactive stashing
stash --interactive --adopt vim       # Interactive adoption with diffs
```

#### Version

```bash
stash -V
stash --version
```

Display version information.

### Options

#### Directory Options

- `--dir=DIR` - Stash directory (default: current directory). Note: `--dir` has no short flag; `-d` is used for `--deploy`.
- `--target=DIR` - Target directory (default: parent of stash directory)
- `-s, --source=DIR` - Source directory (alternative to `--dir`)

**Examples:**
```bash
stash --dir ~/dotfiles --target ~ vim
stash --dir /opt/packages --target /usr/local myapp
```

#### Mode Options

- `-n, --simulate` - Simulation mode (dry-run, no changes made)
- `--no-folding` - Disable tree folding (create individual file symlinks)
- `--adopt` - Adopt existing files into package
- `--conflicts` - List conflicts without taking action
- `-I, --interactive` - Interactive mode (prompt on conflicts, show diffs)

**Examples:**
```bash
stash -n vim                    # Preview what would happen
stash --simulate vim bash       # Simulate multiple packages
stash --no-folding vim          # Force individual file symlinks
stash --adopt vim               # Adopt existing files
stash --conflicts vim           # Check for conflicts
stash -I vim                    # Interactive stashing
stash --adopt -I vim            # Interactive adoption with diffs
```

#### Pattern Options

- `--ignore=REGEX` - Ignore files matching REGEX (can be used multiple times)
- `--defer=REGEX` - Skip files matching REGEX for this operation only (can be used multiple times)
- `--override=REGEX` - Force-include files matching REGEX, even if ignored or deferred (can be used multiple times)

**Examples:**
```bash
stash --ignore='.*\.bak' vim          # Ignore .bak files
stash --defer='.*\.cache' emacs       # Skip cache files this time
stash --override='important\.conf' vim  # Force-include despite ignore rules
```

#### Verbosity Options

- `-v, --verbose` - Increase verbosity (can be repeated)
  - `-v` - Show operations
  - `-vv` - Show detailed folding decisions
  - `-vvv` - Show debug information

**Examples:**
```bash
stash -v vim                    # Show operations
stash -vv vim                   # Show folding details
stash -vvv vim                  # Show debug info
```

#### Help and Version

- `-h, --help` - Show help message
- `-V, --version` - Show version information
- `--completion=SHELL` - Output shell completion script (bash, zsh, or fish)

**Examples:**
```bash
stash --help                    # Show help
stash --version                 # Show version
stash --completion=bash         # Output bash completion script
stash --completion=zsh          # Output zsh completion script
stash --completion=fish         # Output fish completion script
```

To install shell completion permanently, add to your shell config:
```bash
# Bash (~/.bashrc)
eval "$(stash --completion=bash)"

# Zsh (~/.zshrc)
eval "$(stash --completion=zsh)"

# Fish (~/.config/fish/config.fish)
eval "$(stash --completion=fish)"
```

---

## Configuration File

Stash-CL automatically reads configuration from the first existing file in this order:

1. `$XDG_CONFIG_HOME/stash/config` (default: `~/.config/stash/config`)
2. `~/.stashrc`

### Config File Format

The config file uses simple `key = value` lines. Lines starting with `#` are
comments. Supported keys:

- `dir` = PATH - Default stash directory
- `target` = PATH - Default target directory
- `source` = PATH - Default source directory
- `verbose` = N - Default verbosity level (0-3)
- `no-folding` = true|false - Disable tree folding by default
- `ignore` = REGEX - Default ignore pattern (can be repeated)
- `defer` = REGEX - Default defer pattern (can be repeated)
- `override` = REGEX - Default override pattern (can be repeated)

Command-line options always override config file values.

### Example Config

```bash
# ~/.config/stash/config
dir = ~/dotfiles
target = ~
verbose = 1
ignore = .*\.bak
ignore = .*~
```

With this config, you can run `stash vim` without specifying `--dir` or `--target`.

---

## Package Structure

### Recommended Layout

```
stash-directory/
├── package1/
│   ├── .config/
│   │   └── app/
│   │       └── config.yaml
│   ├── .local/
│   │   └── bin/
│   │       └── script
│   └── .stash-local-ignore
├── package2/
│   └── .bashrc
└── .stash-global-ignore
```

### Package Contents

A package can contain:
- **Dotfiles**: `.vimrc`, `.bashrc`, etc.
- **Directories**: `.vim/`, `.config/`, etc.
- **Regular files**: Any file you want to symlink
- **Ignore file**: `.stash-local-ignore` (optional)

### What Gets Stashed

By default, everything in the package directory gets stashed **except**:
- `.git/` and `.gitignore`
- `README`, `LICENSE`, `COPYING`
- Files matching ignore patterns

---

## Ignore Patterns

### Global Ignore File

Create `~/.stash-global-ignore` to ignore patterns across all packages:

```
# Global ignore patterns
*.log
*.tmp
.DS_Store
*.swp
*~
```

### Local Ignore File

Create `.stash-local-ignore` in a package to ignore patterns for that package:

```
# Local ignore patterns for this package
temp/*
*.bak
test-*
```

### Pattern Syntax

Ignore patterns support glob syntax:
- `*` - Match any characters
- `?` - Match single character
- `*.log` - Match all .log files
- `test-*` - Match files starting with "test-"
- `*-backup` - Match files ending with "-backup"
- `temp/*` - Match everything in temp/ directory

### Pattern Matching

File-based patterns (`.stash-global-ignore` and `.stash-local-ignore`) are matched
against **filenames only**, not full paths:
- `*.log` matches `debug.log`
- `test-*` matches `test-script.sh`
- `config/*.log` does not work (use `*.log` instead)

CLI patterns (`--ignore`, `--defer`, `--override`) use **regular expressions**
matched against the full relative path:
- `--ignore='.*\.bak'` matches any file ending in `.bak`
- `--ignore='cache/.*'` matches everything inside `cache/` directories

### Combining Patterns

All pattern sources are applied together:
1. Global patterns from `~/.stash-global-ignore`
2. Local patterns from `package/.stash-local-ignore`
3. CLI patterns from `--ignore`, `--defer`, and `--override` flags
4. Default patterns (`.git`, `README`, etc.)

`--override` patterns take priority over all ignore and defer patterns.

---

## Advanced Usage

### Multiple Packages with Overlapping Directories

Stash-CL handles overlapping directories automatically:

```bash
# Package 1: vim
dotfiles/vim/
└── .vim/
    └── colors/
        └── molokai.vim

# Package 2: emacs
dotfiles/emacs/
└── .vim/
    └── syntax/
        └── lisp.vim

# After stashing both:
~/.vim/                    # Real directory (unfolded)
├── colors/                # Symlink -> vim/.vim/colors/
└── syntax/                # Symlink -> emacs/.vim/syntax/
```

### Simulation Mode

Always test with simulation mode first:

```bash
# Preview changes
stash -n vim

# Output shows what WOULD happen:
# SIMULATE: CREATE DIR /home/user
# SIMULATE: CREATE LINK /home/user/.vimrc
# SIMULATE: CREATE LINK /home/user/.vim
```

### No-Folding Mode

Force individual file symlinks:

```bash
stash --no-folding vim

# Creates individual symlinks:
~/.vim/colors/molokai.vim -> dotfiles/vim/.vim/colors/molokai.vim
~/.vim/syntax/lisp.vim    -> dotfiles/vim/.vim/syntax/lisp.vim
```

### Verbosity Levels

Control output detail:

```bash
# Level 0 (default): Minimal output
stash vim

# Level 1 (-v): Show operations
stash -v vim
# Output: CREATE LINK /home/user/.vimrc

# Level 2 (-vv): Show folding decisions
stash -vv vim
# Output: Folding .vim/ (empty target)

# Level 3 (-vvv): Debug information
stash -vvv vim
# Output: Checking if can fold .vim/...
```

### Restashing After Updates

After updating a package, restash it:

```bash
# Update package files
cd ~/dotfiles/vim
git pull

# Restash to update symlinks
stash -R vim
```

### Deploy All Packages

Install everything at once:

```bash
# Deploy all packages in ~/dotfiles
cd ~/dotfiles
stash -d

# Or specify directory
stash -d --dir ~/dotfiles --target ~
```

---

## Troubleshooting

### Conflict: File Already Exists

**Problem:**
```
Error: Conflict detected: File already exists at /home/user/.vimrc
```

**Solution:**
1. Check what's there: `ls -la ~/.vimrc`
2. If it's a regular file, back it up: `mv ~/.vimrc ~/.vimrc.backup`
3. Try stashing again: `stash vim`

### Conflict: Directory Already Exists

**Problem:**
```
Error: Conflict detected: Directory already exists at /home/user/.vim
```

**Solution:**
1. Check contents: `ls -la ~/.vim/`
2. If it contains files from other sources, you have two options:
   - Move files into your package: `mv ~/.vim/* ~/dotfiles/vim/.vim/`
   - Use `--no-folding` to create individual symlinks: `stash --no-folding vim`

### Broken Symlinks

**Problem:**
```
Error: Broken symlink detected at /home/user/.vimrc
```

**Solution:**
1. Remove broken symlink: `rm ~/.vimrc`
2. Try stashing again: `stash vim`

### Permission Denied

**Problem:**
```
Error: Permission denied when creating /usr/local/bin/script
```

**Solution:**
Use `sudo` for system directories:
```bash
sudo stash --dir ~/packages --target /usr/local myapp
```

### Symlink Points to Wrong Location

**Problem:**
Symlink points to old package location after moving packages.

**Solution:**
1. Unstash: `stash -D vim`
2. Stash again: `stash vim`

Or use restash: `stash -R vim`

---

## Examples

### Example 1: Import Existing Dotfiles

```bash
# Import your existing .bashrc into a package
stash --import ~/.bashrc --package bash --dir ~/dotfiles --target ~

# Import a .config directory
stash --import ~/.config/nvim --package neovim --dir ~/dotfiles --target ~

# Verify - original location is now a symlink
ls -la ~/.bashrc
# lrwxrwxrwx ... .bashrc -> /home/user/dotfiles/bash/.bashrc

# The file is now in the package
cat ~/dotfiles/bash/.bashrc
```

### Example 2: Basic Dotfiles Setup (Manual)

```bash
# Create package structure manually
mkdir -p ~/dotfiles/vim/.vim
echo 'set number' > ~/dotfiles/vim/.vimrc

# Stash
cd ~/dotfiles
stash vim

# Verify
ls -la ~/.vimrc
# lrwxrwxrwx ... .vimrc -> /home/user/dotfiles/vim/.vimrc
```

### Example 3: Multiple Packages (Manual Setup)

```bash
# Create multiple packages
mkdir -p ~/dotfiles/{vim,bash,git}
echo 'set number' > ~/dotfiles/vim/.vimrc
echo 'export PS1="> "' > ~/dotfiles/bash/.bashrc
echo '[user]' > ~/dotfiles/git/.gitconfig

# Stash all at once
cd ~/dotfiles
stash vim bash git

# Or use deploy mode
stash -d
```

### Example 4: Ignore Patterns

```bash
# Create global ignore
cat > ~/.stash-global-ignore << 'EOF'
*.log
*.tmp
.DS_Store
EOF

# Create local ignore
cat > ~/dotfiles/vim/.stash-local-ignore << 'EOF'
*.swp
*~
EOF

# Stash (ignored files won't be symlinked)
stash vim
```

### Example 5: Software Installation

```bash
# Package structure for custom software
mkdir -p ~/packages/myapp/{bin,lib,share}
echo '#!/bin/bash' > ~/packages/myapp/bin/myapp
chmod +x ~/packages/myapp/bin/myapp

# Install to /usr/local
sudo stash --dir ~/packages --target /usr/local myapp

# Verify
which myapp
# /usr/local/bin/myapp
```

### Example 6: Simulation Mode

```bash
# Always test first
stash -n vim

# Review output
# SIMULATE: CREATE DIR /home/user
# SIMULATE: CREATE LINK /home/user/.vimrc
# SIMULATE: CREATE LINK /home/user/.vim

# If looks good, run for real
stash vim
```

### Example 7: Overlapping Directories

```bash
# Two packages with .config/
mkdir -p ~/dotfiles/app1/.config/app1
mkdir -p ~/dotfiles/app2/.config/app2

# Stash both
stash app1 app2

# Result: .config/ is unfolded
ls -la ~/.config/
# drwxr-xr-x ... .config/
# lrwxrwxrwx ... app1 -> /home/user/dotfiles/app1/.config/app1
# lrwxrwxrwx ... app2 -> /home/user/dotfiles/app2/.config/app2
```

### Example 8: Restashing After Updates

```bash
# Update package
cd ~/dotfiles/vim
git pull

# Restash to update symlinks
stash -R vim

# Or unstash and stash manually
stash -D vim
stash vim
```

### Example 9: No-Folding Mode

```bash
# Force individual file symlinks
stash --no-folding vim

# Result: individual symlinks for each file
ls -la ~/.vim/
# lrwxrwxrwx ... colors/molokai.vim -> .../vim/.vim/colors/molokai.vim
# lrwxrwxrwx ... syntax/lisp.vim -> .../vim/.vim/syntax/lisp.vim
```

---

## Comparison with GNU Stow

| Feature | GNU Stow | Stash-CL |
|---------|----------|----------|
| Tree Folding | Basic | Yes (intelligent with partial folding) |
| Multiple Packages | Yes | Yes (with unfolding) |
| Conflict Detection | Basic | Comprehensive |
| Simulation Mode | Yes | Yes |
| Ignore Patterns | Yes (file-based) | Yes (file-based glob + CLI regex) |
| Defer Patterns | Yes | Yes |
| Override Patterns | Yes | Yes |
| Task Planning | No | Yes |
| Verbosity Levels | Limited | 4 levels |
| Deploy Mode | No | Yes (`-d, --deploy`) |
| List Mode | No | Yes (`-l, --list`) |
| Import Mode | No | Yes (`-i, --import`) |
| Adopt Mode | Yes | Yes (`--adopt`) |
| Configuration File | No | Yes (`~/.config/stash/config` or `~/.stashrc`) |
| Shell Completion | No | Yes (bash, zsh, fish) |
| Conflict Checking | No | Yes (`--conflicts`) |
| Interactive Mode | No | Yes (`-I, --interactive`) |
| Progress Indicators | No | Yes |
| Performance | Perl | Compiled Lisp |

---

## Tips and Best Practices

### 1. Use Simulation Mode First

Always test with `-n` before making changes:
```bash
stash -n vim  # Preview
stash vim     # Execute
```

### 2. Organize by Logical Units

Group related files into packages:
```
dotfiles/
├── vim/        # All vim config
├── bash/       # All bash config
├── git/        # All git config
└── ssh/        # All ssh config
```

### 3. Use Version Control

Keep your packages in git:
```bash
cd ~/dotfiles
git init
git add .
git commit -m "Initial dotfiles"
```

### 4. Create Ignore Files

Use ignore patterns to exclude temporary files:
```bash
echo '*.log' >> ~/.stash-global-ignore
echo '*.swp' >> ~/dotfiles/vim/.stash-local-ignore
```

### 5. Test on Fresh System

Use a VM or container to test your setup:
```bash
# In VM
git clone https://github.com/you/dotfiles.git
cd dotfiles
stash -d
```

### 6. Document Your Packages

Add README files to packages (they won't be stashed):
```bash
echo '# Vim Configuration' > ~/dotfiles/vim/README.md
```

### 7. Use Descriptive Package Names

Choose clear names:
- ✅ `vim`, `bash`, `git`
- ❌ `config1`, `stuff`, `misc`

### 8. Back Up Before Major Changes

Before major restructuring:
```bash
# Unstash everything
stash -D vim bash git

# Make changes
# ...

# Restash
stash vim bash git
```

---

## Getting Help

### Command-Line Help

```bash
stash --help
```

### Verbose Output

Use `-vv` or `-vvv` for detailed information:
```bash
stash -vv vim
```

### Simulation Mode

Use `-n` to see what would happen:
```bash
stash -n vim
```

### Bug Reports

Report issues at: https://github.com/glenneth1/stash-cl/issues

---

## License

Stash-CL is released under the MIT License. See LICENSE file for details.

---

## Credits

Inspired by GNU Stow by Bob Glickstein.

Written in Common Lisp using SBCL.
