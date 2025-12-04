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
- ✅ **Folding Statistics** - Detailed reports of operations

### Enhancements Over GNU Stow
- **Smart Partial Folding** - Keeps subdirectories folded when possible
- **Automatic Refolding** - Refolds after unstash automatically
- **Detailed Statistics** - Shows exactly what happened
- **Better Verbosity** - 4 levels with clear output

## Building

```bash
# Build the executable (with SBCL compression)
make

# Build with maximum compression (SBCL + UPX)
make compress-max

# Or manually:
sbcl --load build.lisp
```

This produces two files:
- **`stash`** - Shell wrapper script (handles --version and --help)
- **`stash.bin`** - Compressed executable (~14-16 MB)

See [COMPRESSION.md](COMPRESSION.md) for details on compression options and [PACKAGING.md](PACKAGING.md) for distribution guidelines.

## Usage

```bash
# Stash a package
stash emacs

# Stash with options
stash --dir ~/dotfiles --target ~ emacs

# Simulation mode (dry-run)
stash -n emacs

# Verbose output
stash -vv emacs

# Unstash
stash -D emacs

# Restash (unstash then stash)
stash -R emacs

# Disable folding
stash --no-folding emacs
```

## Testing

```bash
# Run the integration test suite
./test-cli-integration.sh
```

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
├── test-cli-integration.sh - Integration tests
└── README.md               - This file
```

## Dependencies

- SBCL (Steel Bank Common Lisp)
- Quicklisp
- Libraries: uiop, osicat, unix-opts, cl-ppcre, alexandria, local-time

## License

GPL-3.0

## Development

This project is a Common Lisp rewrite of GNU Stow, with significant enhancements to the tree folding system and overall architecture.

### Key Improvements
- Enhanced tree folding with partial folding support
- Automatic refolding after unstash operations
- Integrated task planning system for validation
- Comprehensive test suite
- Multiple verbosity levels (0-3)
