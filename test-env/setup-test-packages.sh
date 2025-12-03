#!/bin/bash
# setup-test-packages.sh - Create test packages for stash-cl testing
# SAFE VERSION with proper error checking

set -euo pipefail  # Exit on error, undefined variables, pipe failures

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Define directories relative to script location
PACKAGES_DIR="${SCRIPT_DIR}/packages"
TARGET_DIR="${SCRIPT_DIR}/target"

# SAFETY CHECK: Verify we're in the right place
if [[ ! "$SCRIPT_DIR" =~ stash-cl/test-env$ ]]; then
    echo "ERROR: This script must be run from stash-cl/test-env directory"
    echo "Current location: $SCRIPT_DIR"
    exit 1
fi

# SAFETY CHECK: Never allow empty directory variables
if [[ -z "$PACKAGES_DIR" ]] || [[ -z "$TARGET_DIR" ]]; then
    echo "FATAL ERROR: Directory variables are empty!"
    exit 1
fi

# SAFETY CHECK: Directories must be under test-env
if [[ ! "$PACKAGES_DIR" =~ test-env/packages$ ]]; then
    echo "FATAL ERROR: PACKAGES_DIR is not in test-env: $PACKAGES_DIR"
    exit 1
fi

if [[ ! "$TARGET_DIR" =~ test-env/target$ ]]; then
    echo "FATAL ERROR: TARGET_DIR is not in test-env: $TARGET_DIR"
    exit 1
fi

echo "========================================="
echo "  Setting up test environment"
echo "========================================="
echo ""
echo "Packages directory: $PACKAGES_DIR"
echo "Target directory: $TARGET_DIR"
echo ""

# Clean up old test environment SAFELY
if [[ -d "$PACKAGES_DIR" ]]; then
    echo "Removing old packages directory..."
    rm -rf "$PACKAGES_DIR"
fi

if [[ -d "$TARGET_DIR" ]]; then
    echo "Removing old target directory..."
    rm -rf "$TARGET_DIR"
fi

# Create fresh directories
mkdir -p "$PACKAGES_DIR"
mkdir -p "$TARGET_DIR"

echo ""
echo "Creating test packages..."
echo ""

# Package 1: emacs (has bin/, share/, lib/)
echo "Creating emacs package..."
mkdir -p "$PACKAGES_DIR/emacs/bin"
mkdir -p "$PACKAGES_DIR/emacs/share/emacs/site-lisp"
mkdir -p "$PACKAGES_DIR/emacs/lib"

cat > "$PACKAGES_DIR/emacs/bin/emacs" << 'EOF'
#!/bin/bash
echo "Emacs (fake)"
EOF
chmod +x "$PACKAGES_DIR/emacs/bin/emacs"

cat > "$PACKAGES_DIR/emacs/bin/emacsclient" << 'EOF'
#!/bin/bash
echo "Emacs Client (fake)"
EOF
chmod +x "$PACKAGES_DIR/emacs/bin/emacsclient"

echo "test" > "$PACKAGES_DIR/emacs/share/emacs/site-lisp/init.el"
echo "lib" > "$PACKAGES_DIR/emacs/lib/libemacs.so"

# Package 2: perl (has bin/, lib/, man/)
echo "Creating perl package..."
mkdir -p "$PACKAGES_DIR/perl/bin"
mkdir -p "$PACKAGES_DIR/perl/lib/perl5"
mkdir -p "$PACKAGES_DIR/perl/man/man1"

cat > "$PACKAGES_DIR/perl/bin/perl" << 'EOF'
#!/bin/bash
echo "Perl (fake)"
EOF
chmod +x "$PACKAGES_DIR/perl/bin/perl"

echo "test" > "$PACKAGES_DIR/perl/lib/perl5/strict.pm"
echo "test" > "$PACKAGES_DIR/perl/man/man1/perl.1"

# Package 3: vim (has bin/, share/)
echo "Creating vim package..."
mkdir -p "$PACKAGES_DIR/vim/bin"
mkdir -p "$PACKAGES_DIR/vim/share/vim/vim90"

cat > "$PACKAGES_DIR/vim/bin/vim" << 'EOF'
#!/bin/bash
echo "Vim (fake)"
EOF
chmod +x "$PACKAGES_DIR/vim/bin/vim"

echo "test" > "$PACKAGES_DIR/vim/share/vim/vim90/vimrc"

# Package 4: simple (just files, no subdirs - for no-folding test)
echo "Creating simple package..."
mkdir -p "$PACKAGES_DIR/simple"
echo "file1 content" > "$PACKAGES_DIR/simple/file1.txt"
echo "file2 content" > "$PACKAGES_DIR/simple/file2.txt"
echo "file3 content" > "$PACKAGES_DIR/simple/file3.txt"

# Create ignore files
echo "Creating .stash-global-ignore..."
cat > "$PACKAGES_DIR/.stash-global-ignore" << 'EOF'
# Global ignore patterns for stash
^\.git$
^\.gitignore$
^README
^LICENSE
^COPYING
EOF

echo "Creating .stash-local-ignore in emacs package..."
cat > "$PACKAGES_DIR/emacs/.stash-local-ignore" << 'EOF'
# Local ignore for emacs package
^\.DS_Store$
^Thumbs\.db$
EOF

echo ""
echo "========================================="
echo "  Test environment ready!"
echo "========================================="
echo ""
echo "Packages created:"
ls -1 "$PACKAGES_DIR"
echo ""
echo "You can now test stash-cl with:"
echo "  cd $SCRIPT_DIR/.."
echo "  ./stash --dir test-env/packages --target test-env/target emacs"
echo ""
