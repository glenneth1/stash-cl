#!/bin/bash
# Test script for no-folding mode in stash-cl

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

TEST_DIR="$SCRIPT_DIR/test-env"
PACKAGES_DIR="$TEST_DIR/packages"
TARGET_DIR="$TEST_DIR/target"

echo -e "${BLUE}=== Stash-CL No-Folding Mode Tests ===${NC}\n"

TESTS_PASSED=0
TESTS_FAILED=0

# Test 1: No-folding creates individual file symlinks
echo -e "${YELLOW}=== Test 1: No-Folding Mode ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

./stash --no-folding --dir "$PACKAGES_DIR" --target "$TARGET_DIR" emacs > /dev/null 2>&1

# Count directory symlinks (should be 0 with no-folding)
dir_symlinks=$(find "$TARGET_DIR" -type l -exec sh -c 'test -d "$(readlink -f "$1")"' _ {} \; -print | wc -l)

# Count file symlinks (should be > 0)
file_symlinks=$(find "$TARGET_DIR" -type l -exec sh -c 'test -f "$(readlink -f "$1")"' _ {} \; -print | wc -l)

# Count real directories (should be > 0)
real_dirs=$(find "$TARGET_DIR" -type d | wc -l)

echo "  Directory symlinks: $dir_symlinks (expected: 0)"
echo "  File symlinks: $file_symlinks (expected: > 0)"
echo "  Real directories: $real_dirs (expected: > 0)"

if [ "$dir_symlinks" -eq 0 ] && [ "$file_symlinks" -gt 0 ] && [ "$real_dirs" -gt 0 ]; then
    echo -e "${GREEN}  ✓ No-folding mode working correctly${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "${RED}  ✗ No-folding mode failed${NC}"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

# Test 2: Compare with folding mode
echo -e "\n${YELLOW}=== Test 2: Folding Mode (Default) ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

./stash --dir "$PACKAGES_DIR" --target "$TARGET_DIR" emacs > /dev/null 2>&1

# Count directory symlinks (should be > 0 with folding)
dir_symlinks_fold=$(find "$TARGET_DIR" -type l -exec sh -c 'test -d "$(readlink -f "$1")"' _ {} \; -print | wc -l)

# Count file symlinks (should be 0 or minimal)
file_symlinks_fold=$(find "$TARGET_DIR" -type l -exec sh -c 'test -f "$(readlink -f "$1")"' _ {} \; -print | wc -l)

echo "  Directory symlinks: $dir_symlinks_fold (expected: > 0)"
echo "  File symlinks: $file_symlinks_fold (expected: minimal)"

if [ "$dir_symlinks_fold" -gt 0 ]; then
    echo -e "${GREEN}  ✓ Folding mode working correctly${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "${RED}  ✗ Folding mode failed${NC}"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

# Test 3: Verify all files are accessible in both modes
echo -e "\n${YELLOW}=== Test 3: File Accessibility ===${NC}"

# No-folding
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"
./stash --no-folding --dir "$PACKAGES_DIR" --target "$TARGET_DIR" emacs > /dev/null 2>&1

if [ -e "$TARGET_DIR/bin/emacs" ] && \
   [ -e "$TARGET_DIR/bin/emacsclient" ] && \
   [ -e "$TARGET_DIR/lib/libemacs.so" ] && \
   [ -e "$TARGET_DIR/share/emacs/site-lisp/init.el" ]; then
    echo -e "${GREEN}  ✓ All files accessible with no-folding${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "${RED}  ✗ Some files missing with no-folding${NC}"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

# Folding
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"
./stash --dir "$PACKAGES_DIR" --target "$TARGET_DIR" emacs > /dev/null 2>&1

if [ -e "$TARGET_DIR/bin/emacs" ] && \
   [ -e "$TARGET_DIR/bin/emacsclient" ] && \
   [ -e "$TARGET_DIR/lib/libemacs.so" ] && \
   [ -e "$TARGET_DIR/share/emacs/site-lisp/init.el" ]; then
    echo -e "${GREEN}  ✓ All files accessible with folding${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "${RED}  ✗ Some files missing with folding${NC}"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

# Test 4: Unstash works in both modes
echo -e "\n${YELLOW}=== Test 4: Unstash Compatibility ===${NC}"

# Unstash no-folding
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"
./stash --no-folding --dir "$PACKAGES_DIR" --target "$TARGET_DIR" emacs > /dev/null 2>&1
./stash -D --dir "$PACKAGES_DIR" --target "$TARGET_DIR" emacs > /dev/null 2>&1

remaining=$(find "$TARGET_DIR" -type l | wc -l)
if [ "$remaining" -eq 0 ]; then
    echo -e "${GREEN}  ✓ Unstash works with no-folding${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "${RED}  ✗ Unstash failed with no-folding ($remaining symlinks remain)${NC}"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

# Cleanup
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

# Summary
echo -e "\n${BLUE}=== Test Summary ===${NC}"
echo "Total tests: $((TESTS_PASSED + TESTS_FAILED))"
echo -e "${GREEN}Passed: $TESTS_PASSED${NC}"
echo -e "${RED}Failed: $TESTS_FAILED${NC}"

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "\n${GREEN}All no-folding tests passed! ✓${NC}"
    exit 0
else
    echo -e "\n${RED}Some tests failed.${NC}"
    exit 1
fi
