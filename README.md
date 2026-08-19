# Stash-CL

A GNU Stow replacement written in Common Lisp with enhanced tree folding and intelligent symlink management.

**Repository**: https://github.com/glenneth1/stash-cl

## Features

### Core Features
- ✅ **Task Planning System** - Validates all operations before executing
- ✅ **Enhanced Tree Folding** - Smart directory symlinks with partial folding
- ✅ **Automatic Refolding** - Refolds directories after unstashing
- ✅ **Simulation Mode** - Dry-run to see what would happen
- ✅ **Restash Operation** - Unstash then re-stash to update packages
- ✅ **Adopt Existing Files** - Move existing files into package management
- ✅ **Ignore Patterns** - Global, local, and CLI-based file exclusion
- ✅ **Folding Statistics** - Detailed reports of operations
- ✅ **Colored Output** - Clear, actionable error messages

### New in v0.3.0
- ✅ **Configuration File** - Set defaults in ~/.config/stash/config or ~/.stashrc
- ✅ **Shell Completion** - Built-in bash, zsh, and fish completion via --completion=SHELL
- ✅ **Conflicts Flag** - List all conflicts without taking action with --conflicts
- ✅ **Interactive Mode** - Prompt on conflicts with diff display with --interactive/-I
- ✅ **Progress Indicators** - Progress tracking with [N/Total] percentage display
- ✅ **Expanded Test Suite** - Unit tests (26 tests) + integration tests (50 assertions)

### Enhancements Over GNU Stow
- **Smart Partial Folding** - Keeps subdirectories folded when possible
- **Automatic Refolding** - Refolds after unstash automatically
- **File Adoption** - Easily adopt existing dotfiles into management
- **CLI Ignore Patterns** - Add ignore patterns from command line
- **Defer and Override Patterns** - Skip or force-include files per operation
- **Import Mode** - Create packages from existing files with symlinks
- **List Mode** - Show all packages and their stashing status
- **Deploy Mode** - Stash all packages in one command
- **Colored Error Messages** - Helpful suggestions with every error
- **Detailed Statistics** - Shows exactly what happened
- **Better Verbosity** - 4 levels with clear output
- **Smaller Footprint** - 16MB standalone executable vs Perl + dependencies

## Building

```bash
# Build the executable (with SBCL compression)
make

# Build with maximum compression (SBCL + UPX)
make compress-max

# Or manually:
sbcl --load build.lisp
```

This produces:
- **`stash`** - Compressed standalone executable (~16 MB)

See [COMPRESSION.md](COMPRESSION.md) for details on compression options and [PACKAGING.md](PACKAGING.md) for distribution guidelines.

## Installation

### System-wide Installation
```bash
sudo make install
```

This installs:
- `stash` to `/usr/local/bin/`
- Man page to `/usr/local/share/man/man1/`

### Custom Installation Directory
```bash
sudo make install PREFIX=/opt/stash
```

### Uninstall
```bash
sudo make uninstall
```

## Usage

For detailed usage information, see the man page:
```bash
man stash
```

### Quick Start

```bash
# Basic stashing
cd ~/.dotfiles
stash emacs

# Unstash
stash -D emacs

# Restash (update after changes)
stash -R emacs

# Import existing files into a new package
stash --import ~/.bashrc --package bash --dir ~/.dotfiles --target ~
stash -i ~/.config/nvim -p neovim --dir ~/.dotfiles --target ~
```

### Common Workflows

#### Importing Existing Dotfiles
```bash
# Import a single file into a package
stash --import ~/.bashrc --package bash --dir ~/.dotfiles --target ~

# Import a .config directory into a package
stash --import ~/.config/qutebrowser --package qutebrowser --dir ~/.dotfiles --target ~

# Simulate import first
stash -n --import ~/.vimrc --package vim --dir ~/.dotfiles --target ~
```

#### Managing Dotfiles
```bash
# Stash your dotfiles from ~/.dotfiles to ~
cd ~/.dotfiles
stash --target ~ vim emacs zsh

# Adopt existing dotfiles into management
stash --adopt --target ~ myconfig

# Simulate before making changes
stash -n --adopt myconfig
```

#### Using Ignore Patterns
```bash
# Ignore backup files
stash --ignore='.*\.bak' --ignore='.*~' vim

# Use ignore files
echo "*.log" > ~/.stash-global-ignore
echo "temp/*" > ~/.dotfiles/vim/.stash-local-ignore
stash vim
```

#### Checking for Conflicts
```bash
# Check what would conflict before stashing
stash --conflicts vim
stash --conflicts emacs bash

# Interactive mode: prompt on conflicts
stash -I vim

# Interactive adoption: review each file before adopting
stash --adopt -I vim
```

#### Shell Completion
```bash
# Bash
eval "$(stash --completion=bash)"

# Zsh
eval "$(stash --completion=zsh)"

# Fish
eval "$(stash --completion=fish)"
```

Add the appropriate line to your shell config file (~/.bashrc, ~/.zshrc, or ~/.config/fish/config.fish) for permanent completion.

#### Configuration File
```bash
# Create ~/.config/stash/config or ~/.stashrc
cat > ~/.config/stash/config << 'EOF'
dir = ~/dotfiles
target = ~
verbose = 1
ignore = .*\.bak
EOF

# Now you can just run:
stash vim          # Uses dir and target from config
stash --conflicts vim  # Check conflicts with config defaults
```

#### Verbose Output and Debugging
```bash
# Show what's being folded
stash -v emacs

# Show detailed folding decisions
stash -vv emacs

# Show all internal operations
stash -vvv emacs
```

#### Advanced Options
```bash
# Deploy all packages at once
cd ~/.dotfiles
stash -d

# Disable tree folding (create individual file symlinks)
stash --no-folding perl

# Custom directories
stash --dir ~/my-packages --target /opt/local myapp

# Simulation mode (dry-run)
stash -n --adopt --ignore='test-*' mypackage
```

## Testing

```bash
# Run all tests (unit + integration)
make test

# Run unit tests only
make test-unit

# Run integration tests only
make test-integration
```

The test suite includes:
- **Unit tests** (test-unit.lisp): 26 tests covering config parsing, path utilities, file operations, and task planner
- **Integration tests** (test-integration.sh): 50 assertions covering CLI features including stash, unstash, deploy, list, conflicts, config files, shell completion, ignore patterns, import, and more

## Project Structure

```
stash-cl/
├── src/
│   ├── package.lisp        - Package definitions
│   ├── main.lisp           - CLI and main entry point
│   ├── task-planner.lisp   - Task planning system
│   ├── folding.lisp        - Enhanced tree folding
│   ├── help.lisp           - Help and version info
│   ├── colors.lisp         - ANSI color utilities
│   ├── log.lisp            - Logging
│   ├── paths.lisp          - Path manipulation
│   ├── conflict.lisp       - Conflict handling
│   ├── file-ops.lisp       - File operations
│   ├── package-mgmt.lisp   - Package management
│   └── tree.lisp           - Tree analysis
├── test-env/
│   └── setup-test-packages.sh  - Test environment setup
├── stash-cl.asd            - ASDF system definition
├── build.lisp              - Build script
├── Makefile                - Build automation
├── test-unit.lisp          - Unit tests
├── test-integration.sh     - Integration tests
├── test-cli-integration.sh - Basic CLI tests
└── README.md               - This file
```

## Dependencies

- SBCL (Steel Bank Common Lisp)
- Quicklisp
- Libraries: uiop, unix-opts, cl-ppcre, alexandria, local-time

## License

GPL-3.0

## Troubleshooting

### Common Issues

**Symlinks not created**
- Check that the package directory exists
- Verify target directory permissions
- Use `-vv` to see detailed folding decisions

**Conflicts with existing files**
- Use `--adopt` to move existing files into the package
- Or manually move/remove conflicting files
- Use `-n` to simulate and see what would conflict

**Files being ignored unexpectedly**
- Check `~/.stash-global-ignore` for global patterns
- Check `.stash-local-ignore` in the package directory
- Use `-vv` to see which files are being ignored

**Permission denied errors**
- Ensure you have write permissions to target directory
- Use `sudo` if installing to system directories
- Check file ownership with `ls -la`

### Getting Help

```bash
# Show help message
stash -h

# Show version
stash --version

# View man page
man stash

# Run in simulation mode to see what would happen
stash -n <package>
```

## FAQ

**Q: How is stash-cl different from GNU Stow?**  
A: stash-cl provides enhanced tree folding, automatic refolding, file adoption, CLI ignore patterns, defer/override patterns, import mode, list mode, deploy mode, configuration file support, shell completion, conflict checking, interactive mode, colored error messages, and a smaller footprint (16MB standalone vs Perl + dependencies).

**Q: Can I use stash-cl as a drop-in replacement for GNU Stow?**  
A: Yes! stash-cl supports all core GNU Stow operations. The main difference is improved folding behavior and additional features.

**Q: What's tree folding?**  
A: Instead of creating individual symlinks for each file, stash-cl creates directory symlinks when possible. This results in fewer symlinks and cleaner target directories.

**Q: How do I adopt my existing dotfiles?**  
A: Use `stash --adopt <package>`. This will scan your target directory, move existing files into the package, and create symlinks.

**Q: Can I ignore certain files?**  
A: Yes! Use `~/.stash-global-ignore` for global patterns, `.stash-local-ignore` in packages for local patterns, or `--ignore=PATTERN` on the command line.

**Q: What if I want individual file symlinks instead of directory symlinks?**  
A: Use the `--no-folding` flag to disable tree folding.

**Q: How do I update a package after making changes?**  
A: Use `stash -R <package>` to restash (unstash then stash again).

**Q: Is it safe to use?**  
A: Yes! Use `-n` (simulation mode) to see what would happen before making changes. The task planning system validates all operations before executing.

## Comparison with GNU Stow

| Feature | GNU Stow | stash-cl |
|---------|----------|----------|
| Basic stashing | ✅ | ✅ |
| Unstashing | ✅ | ✅ |
| Tree folding | ✅ Basic | ✅ Enhanced with partial folding |
| Automatic refolding | ❌ | ✅ |
| File adoption | ❌ | ✅ |
| CLI ignore patterns | ❌ | ✅ |
| Colored error messages | ❌ | ✅ |
| Task validation | ❌ | ✅ |
| Restash operation | ❌ | ✅ Single command |
| Configuration file | ❌ | ✅ ~/.config/stash/config or ~/.stashrc |
| Shell completion | ❌ | ✅ bash, zsh, fish |
| Conflict checking | ❌ | ✅ --conflicts flag |
| Interactive mode | ❌ | ✅ --interactive with diffs |
| Progress indicators | ❌ | ✅ |
| Executable size | ~5MB + Perl | 16MB standalone |
| Dependencies | Perl | None (standalone binary) |
| Man page | ✅ | ✅ |

## Development

This project is a Common Lisp rewrite of GNU Stow, with significant enhancements to the tree folding system and overall architecture.

### Key Improvements
- Enhanced tree folding with partial folding support
- Automatic refolding after unstash operations
- File adoption for managing existing dotfiles
- CLI ignore patterns for flexible file exclusion
- Colored error messages with actionable suggestions
- Integrated task planning system for validation
- Comprehensive test suite
- Multiple verbosity levels (0-3)

### Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for development guidelines.

### Reporting Issues

Report bugs and feature requests at: https://github.com/glenneth1/stash-cl/issues
