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
- **Task Planning**: Validates all operations before executing
- **Deploy Mode**: Install all packages at once

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

This creates two files:
- `./stash` - Shell wrapper script (handles --version and --help)
- `./stash.bin` - Compressed SBCL executable (~16MB)

### Installing System-Wide

Use the provided Makefile target:

```bash
sudo make install
```

This installs both files to `/usr/local/bin/` and the man page to `/usr/local/share/man/man1/`.

Or manually:

```bash
sudo cp stash stash.bin /usr/local/bin/
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

### Options

#### Directory Options

- `--dir=DIR` - Stash directory (default: current directory)
- `--target=DIR` - Target directory (default: parent of stash directory)

**Examples:**
```bash
stash --dir ~/dotfiles --target ~ vim
stash --dir /opt/packages --target /usr/local myapp
```

#### Mode Options

- `-n, --simulate` - Simulation mode (dry-run, no changes made)
- `--no-folding` - Disable tree folding (create individual file symlinks)

**Examples:**
```bash
stash -n vim                    # Preview what would happen
stash --simulate vim bash       # Simulate multiple packages
stash --no-folding vim          # Force individual file symlinks
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

#### Help

- `-h, --help` - Show help message

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

Patterns are matched against **filenames only**, not full paths:
- ✅ `*.log` matches `debug.log`
- ✅ `test-*` matches `test-script.sh`
- ❌ `config/*.log` doesn't work (use `*.log` instead)

### Combining Patterns

Both global and local patterns are applied:
1. Global patterns from `~/.stash-global-ignore`
2. Local patterns from `package/.stash-local-ignore`
3. Default patterns (`.git`, `README`, etc.)

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

### Example 1: Basic Dotfiles Setup

```bash
# Create package structure
mkdir -p ~/dotfiles/vim/.vim
echo 'set number' > ~/dotfiles/vim/.vimrc

# Stash
cd ~/dotfiles
stash vim

# Verify
ls -la ~/.vimrc
# lrwxrwxrwx ... .vimrc -> /home/user/dotfiles/vim/.vimrc
```

### Example 2: Multiple Packages

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

### Example 3: Ignore Patterns

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

### Example 4: Software Installation

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

### Example 5: Simulation Mode

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

### Example 6: Overlapping Directories

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

### Example 7: Restashing After Updates

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

### Example 8: No-Folding Mode

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
| Tree Folding | No | Yes (intelligent) |
| Multiple Packages | Yes | Yes (with unfolding) |
| Conflict Detection | Basic | Comprehensive |
| Simulation Mode | Yes | Yes |
| Ignore Patterns | No | Yes (glob + regex) |
| Task Planning | No | Yes |
| Verbosity Levels | Limited | 4 levels |
| Deploy Mode | No | Yes |
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
