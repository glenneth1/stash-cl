# Contributing to stash-cl

Thank you for your interest in contributing to stash-cl! This document provides guidelines and information for contributors.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Setup](#development-setup)
- [Project Structure](#project-structure)
- [Coding Standards](#coding-standards)
- [Testing](#testing)
- [Submitting Changes](#submitting-changes)
- [Good First Issues](#good-first-issues)

## Code of Conduct

- Be respectful and constructive
- Focus on what is best for the project and community
- Show empathy towards other community members
- Accept constructive criticism gracefully

## Getting Started

### Prerequisites

- SBCL (Steel Bank Common Lisp) 2.0 or later
- Quicklisp
- Git
- Make
- (Optional) UPX for maximum compression

### Fork and Clone

```bash
# Fork the repository on GitHub, then:
git clone https://github.com/YOUR_USERNAME/stash-cl.git
cd stash-cl
```

## Development Setup

### Building from Source

```bash
# Build the executable
make

# Run tests
make test

# Clean build artifacts
make clean
```

### Running in Development

```bash
# Load the system in SBCL REPL
sbcl
* (ql:quickload :stash-cl)
* (in-package :stash-cl)
* (main '("--help"))
```

### Testing Changes

```bash
# Run integration tests
./test-cli-integration.sh

# Run specific test
./test-ignore-patterns.sh

# Test with simulation mode
./stash -n <package>
```

## Project Structure

```
stash-cl/
├── src/
│   ├── package.lisp        - Package definitions and exports
│   ├── main.lisp           - CLI parsing and main entry point
│   ├── task-planner.lisp   - Task planning and validation
│   ├── folding.lisp        - Tree folding logic
│   ├── package-mgmt.lisp   - Package and ignore pattern management
│   ├── file-ops.lisp       - File system operations
│   ├── conflict.lisp       - Conflict detection and handling
│   ├── colors.lisp         - ANSI color utilities
│   ├── help.lisp           - Help and version display
│   ├── log.lisp            - Logging utilities
│   ├── paths.lisp          - Path manipulation
│   └── tree.lisp           - Tree analysis
├── test-env/               - Test environment and fixtures
├── stash-cl.asd            - ASDF system definition
├── build.lisp              - Build script
├── Makefile                - Build automation
└── test-*.sh               - Integration test scripts
```

## Coding Standards

### Common Lisp Style

- Follow standard Common Lisp conventions
- Use 2-space indentation
- Keep lines under 100 characters when possible
- Use descriptive variable and function names

### Naming Conventions

```lisp
;; Functions: lowercase with hyphens
(defun my-function-name ()
  ...)

;; Variables: *special-variables* with earmuffs
(defparameter *my-variable* value)

;; Constants: +MY-CONSTANT+ with plus signs
(defconstant +my-constant+ value)

;; Predicates: end with -p
(defun file-exists-p (path)
  ...)
```

### Documentation

- Add docstrings to all exported functions
- Document complex algorithms with comments
- Update man page for new CLI options
- Add examples to README for new features

```lisp
(defun my-function (arg1 arg2)
  "Brief description of what the function does.
ARG1 is the first argument.
ARG2 is the second argument.
Returns the result of the operation."
  (body))
```

### Error Handling

- Use colored error messages with `stash-cl/colors:format-error`
- Provide actionable suggestions
- Handle edge cases gracefully

```lisp
(unless (probe-file path)
  (error (stash-cl/colors:format-error 
          (format nil "File not found: ~A" path)
          "Check that the file exists")))
```

## Testing

### Writing Tests

- Add integration tests for new features
- Test both success and failure cases
- Test edge cases and boundary conditions
- Use simulation mode to verify behavior

### Test Structure

```bash
#!/bin/bash
# test-my-feature.sh

# Setup
rm -rf test-env/target
mkdir -p test-env/target

# Test case 1
./stash -n my-package
if [ $? -eq 0 ]; then
    echo "✓ Test passed"
else
    echo "✗ Test failed"
fi

# Cleanup
rm -rf test-env/target
```

### Running Tests

```bash
# Run all integration tests
make test

# Run specific test
./test-my-feature.sh

# Test with verbose output
./stash -vv <package>
```

## Submitting Changes

### Before Submitting

1. **Test your changes** - Run all tests
2. **Update documentation** - README, man page, docstrings
3. **Follow coding standards** - Check style and formatting
4. **Write clear commit messages** - Explain what and why

### Commit Message Format

```
Short summary (50 chars or less)

More detailed explanation if needed. Wrap at 72 characters.
Explain the problem this commit solves and why you chose
this solution.

- Bullet points are okay
- Use present tense ("Add feature" not "Added feature")
- Reference issues: "Fixes #123" or "Relates to #456"
```

### Pull Request Process

1. **Create a feature branch**
   ```bash
   git checkout -b feature/my-new-feature
   ```

2. **Make your changes**
   - Write code
   - Add tests
   - Update documentation

3. **Commit your changes**
   ```bash
   git add .
   git commit -m "Add my new feature"
   ```

4. **Push to your fork**
   ```bash
   git push origin feature/my-new-feature
   ```

5. **Create a Pull Request**
   - Go to GitHub and create a PR
   - Describe your changes
   - Reference any related issues
   - Wait for review

### Code Review

- Be open to feedback
- Respond to review comments
- Make requested changes
- Be patient - reviews take time

## Good First Issues

Looking to contribute but not sure where to start? Here are some good first issues:

### Easy
- Improve error messages
- Add more examples to README
- Fix typos in documentation
- Add shell completion scripts
- Improve test coverage

### Medium
- Add new CLI options
- Improve performance
- Add more ignore pattern features
- Enhance verbose output
- Add progress indicators

### Advanced
- Implement new folding strategies
- Add hooks system
- Implement undo/redo functionality
- Add interactive mode
- Performance optimization

## Development Tips

### Debugging

```lisp
;; Add debug output
(when (>= *folding-verbosity* 3)
  (format t "Debug: ~A~%" value))

;; Use SBCL debugger
(break)  ; Drop into debugger

;; Trace function calls
(trace my-function)
```

### Testing Locally

```bash
# Create test environment
mkdir -p test-env/packages/mytest
echo "test" > test-env/packages/mytest/file.txt

# Test your changes
./stash --dir test-env/packages --target test-env/target mytest

# Check results
ls -la test-env/target/
```

### Performance Profiling

```lisp
;; Profile code
(require :sb-sprof)
(sb-sprof:with-profiling (:report :flat)
  (my-function))
```

## Questions?

- Open an issue for questions
- Check existing issues and PRs
- Read the man page: `man stash`
- Check the README and documentation

## License

By contributing to stash-cl, you agree that your contributions will be licensed under the GPL-3.0 license.

---

Thank you for contributing to stash-cl! 🚀
