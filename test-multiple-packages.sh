#!/bin/bash
# Test script for multiple package handling in stash-cl

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Test environment
TEST_DIR="$SCRIPT_DIR/test-env"
PACKAGES_DIR="$TEST_DIR/packages"
TARGET_DIR="$TEST_DIR/target"

echo -e "${BLUE}=== Stash-CL Multiple Package Tests ===${NC}\n"

# Clean up from previous tests
echo -e "${YELLOW}Cleaning up test environment...${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

# Test counter
TESTS_PASSED=0
TESTS_FAILED=0
TOTAL_TESTS=0

# Helper function to run a test
run_test() {
    local test_name="$1"
    shift
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    echo -e "\n${BLUE}Test $TOTAL_TESTS: $test_name${NC}"
    
    if "$@" 2>&1; then
        echo -e "${GREEN}✓ PASS${NC}"
        TESTS_PASSED=$((TESTS_PASSED + 1))
        return 0
    else
        echo -e "${RED}✗ FAIL${NC}"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        return 1
    fi
}

# Test 1: Stash two packages simultaneously
echo -e "\n${YELLOW}=== Test 1: Stash Two Packages ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

run_test "Stash emacs and vim together" \
    ./stash --dir "$PACKAGES_DIR" --target "$TARGET_DIR" emacs vim

# Verify both packages were stashed
if [ -L "$TARGET_DIR/bin" ]; then
    echo -e "${GREEN}  ✓ bin directory exists${NC}"
else
    echo -e "${RED}  ✗ bin directory missing${NC}"
fi

# Check that both emacs and vim binaries are accessible
if [ -e "$TARGET_DIR/bin/emacs" ] && [ -e "$TARGET_DIR/bin/vim" ]; then
    echo -e "${GREEN}  ✓ Both emacs and vim binaries accessible${NC}"
else
    echo -e "${RED}  ✗ Missing binaries${NC}"
fi

# Test 2: Stash three packages
echo -e "\n${YELLOW}=== Test 2: Stash Three Packages ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

run_test "Stash emacs, vim, and perl together" \
    ./stash --dir "$PACKAGES_DIR" --target "$TARGET_DIR" emacs vim perl

# Verify all three packages
if [ -e "$TARGET_DIR/bin/emacs" ] && [ -e "$TARGET_DIR/bin/vim" ] && [ -e "$TARGET_DIR/bin/perl" ]; then
    echo -e "${GREEN}  ✓ All three binaries accessible${NC}"
else
    echo -e "${RED}  ✗ Missing binaries${NC}"
fi

# Test 3: Stash packages with overlapping directories
echo -e "\n${YELLOW}=== Test 3: Overlapping Directories ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

# emacs and vim both have bin/ and share/ directories
run_test "Stash packages with overlapping directories" \
    ./stash -vv --dir "$PACKAGES_DIR" --target "$TARGET_DIR" emacs vim

# Check that both packages' files are in the shared directories
echo -e "${BLUE}Checking bin/ directory contents:${NC}"
ls -la "$TARGET_DIR/bin/" 2>/dev/null || echo "bin/ doesn't exist"

echo -e "${BLUE}Checking share/ directory contents:${NC}"
ls -la "$TARGET_DIR/share/" 2>/dev/null || echo "share/ doesn't exist"

# Test 4: Unstash one package, keep others
echo -e "\n${YELLOW}=== Test 4: Unstash One Package ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

# First stash both
./stash --dir "$PACKAGES_DIR" --target "$TARGET_DIR" emacs vim > /dev/null 2>&1

# Then unstash only emacs
run_test "Unstash emacs while keeping vim" \
    ./stash -D --dir "$PACKAGES_DIR" --target "$TARGET_DIR" emacs

# Verify vim is still there but emacs is gone
if [ -e "$TARGET_DIR/bin/vim" ] && [ ! -e "$TARGET_DIR/bin/emacs" ]; then
    echo -e "${GREEN}  ✓ vim still present, emacs removed${NC}"
else
    echo -e "${RED}  ✗ Incorrect state after unstash${NC}"
fi

# Test 5: Restash multiple packages
echo -e "\n${YELLOW}=== Test 5: Restash Multiple Packages ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

# First stash
./stash --dir "$PACKAGES_DIR" --target "$TARGET_DIR" emacs vim > /dev/null 2>&1

# Then restash both
run_test "Restash emacs and vim" \
    ./stash -R --dir "$PACKAGES_DIR" --target "$TARGET_DIR" emacs vim

# Verify both are still there
if [ -e "$TARGET_DIR/bin/emacs" ] && [ -e "$TARGET_DIR/bin/vim" ]; then
    echo -e "${GREEN}  ✓ Both packages still present after restash${NC}"
else
    echo -e "${RED}  ✗ Missing packages after restash${NC}"
fi

# Test 6: Simulation mode with multiple packages
echo -e "\n${YELLOW}=== Test 6: Simulation Mode ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

run_test "Simulate stashing multiple packages" \
    ./stash -n --dir "$PACKAGES_DIR" --target "$TARGET_DIR" emacs vim perl

# Verify nothing was actually created
if [ ! -e "$TARGET_DIR/bin" ]; then
    echo -e "${GREEN}  ✓ Simulation mode didn't create files${NC}"
else
    echo -e "${RED}  ✗ Simulation mode created files${NC}"
fi

# Test 7: Stash simple package with others (no subdirectories)
echo -e "\n${YELLOW}=== Test 7: Mix of Package Types ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

run_test "Stash simple (files only) with emacs (directories)" \
    ./stash --dir "$PACKAGES_DIR" --target "$TARGET_DIR" simple emacs

# Verify both types work together
if [ -e "$TARGET_DIR/file1.txt" ] && [ -e "$TARGET_DIR/bin/emacs" ]; then
    echo -e "${GREEN}  ✓ Both file-only and directory packages work together${NC}"
else
    echo -e "${RED}  ✗ Package type mixing failed${NC}"
fi

# Test 8: Verbose output with multiple packages
echo -e "\n${YELLOW}=== Test 8: Verbose Mode ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

echo -e "${BLUE}Running with verbose mode:${NC}"
./stash -vv --dir "$PACKAGES_DIR" --target "$TARGET_DIR" emacs vim 2>&1 | head -30

# Final cleanup
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

# Summary
echo -e "\n${BLUE}=== Test Summary ===${NC}"
echo -e "Total tests: $TOTAL_TESTS"
echo -e "${GREEN}Passed: $TESTS_PASSED${NC}"
echo -e "${RED}Failed: $TESTS_FAILED${NC}"

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "\n${GREEN}All multiple package tests passed! ✓${NC}"
    exit 0
else
    echo -e "\n${RED}Some tests failed. Review the output above.${NC}"
    exit 1
fi
