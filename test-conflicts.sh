#!/bin/bash
# Test script for conflict handling in stash-cl

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

echo -e "${BLUE}=== Stash-CL Conflict Handling Tests ===${NC}\n"

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
    local expected_result="$2"  # "pass" or "fail"
    shift 2
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    echo -e "\n${BLUE}Test $TOTAL_TESTS: $test_name${NC}"
    
    if "$@" 2>&1; then
        if [ "$expected_result" = "pass" ]; then
            echo -e "${GREEN}✓ PASS${NC}"
            TESTS_PASSED=$((TESTS_PASSED + 1))
        else
            echo -e "${RED}✗ FAIL (expected to fail but passed)${NC}"
            TESTS_FAILED=$((TESTS_FAILED + 1))
        fi
    else
        if [ "$expected_result" = "fail" ]; then
            echo -e "${GREEN}✓ PASS (correctly detected conflict)${NC}"
            TESTS_PASSED=$((TESTS_PASSED + 1))
        else
            echo -e "${RED}✗ FAIL (unexpected error)${NC}"
            TESTS_FAILED=$((TESTS_FAILED + 1))
        fi
    fi
}

# Test 1: Existing regular file conflicts with symlink
echo -e "\n${YELLOW}=== Test 1: Existing File Conflict ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"
echo "existing content" > "$TARGET_DIR/file1.txt"

run_test "Stash with existing file in target" "fail" \
    ./stash --dir "$PACKAGES_DIR" --target "$TARGET_DIR" simple

# Clean up
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

# Test 2: Existing directory conflicts with symlink
echo -e "\n${YELLOW}=== Test 2: Existing Directory Conflict ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR/bin"
echo "existing script" > "$TARGET_DIR/bin/existing"

run_test "Stash with existing directory in target" "fail" \
    ./stash --dir "$PACKAGES_DIR" --target "$TARGET_DIR" emacs

# Clean up
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

# Test 3: Broken symlink in target
echo -e "\n${YELLOW}=== Test 3: Broken Symlink Conflict ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"
ln -s /nonexistent/path "$TARGET_DIR/file1.txt"

run_test "Stash with broken symlink in target" "fail" \
    ./stash --dir "$PACKAGES_DIR" --target "$TARGET_DIR" simple

# Clean up
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

# Test 4: Symlink pointing to different package
echo -e "\n${YELLOW}=== Test 4: Symlink to Different Package ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"
# First stash emacs
./stash --dir "$PACKAGES_DIR" --target "$TARGET_DIR" emacs > /dev/null 2>&1
# Try to stash vim (which also has bin/ directory)
run_test "Stash with symlink to different package" "fail" \
    ./stash --dir "$PACKAGES_DIR" --target "$TARGET_DIR" vim

# Clean up
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

# Test 5: Symlink pointing to same package (should succeed)
echo -e "\n${YELLOW}=== Test 5: Symlink to Same Package (Re-stash) ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"
# First stash
./stash --dir "$PACKAGES_DIR" --target "$TARGET_DIR" simple > /dev/null 2>&1
# Try to stash again (should detect existing symlinks)
run_test "Re-stash same package" "pass" \
    ./stash --dir "$PACKAGES_DIR" --target "$TARGET_DIR" simple

# Clean up
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

# Test 6: Mixed content directory (some files exist, some don't)
echo -e "\n${YELLOW}=== Test 6: Mixed Content Directory ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"
echo "existing" > "$TARGET_DIR/file1.txt"
# file2.txt and file3.txt don't exist

run_test "Stash with partial conflicts" "fail" \
    ./stash --dir "$PACKAGES_DIR" --target "$TARGET_DIR" simple

# Clean up
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

# Test 7: Simulation mode with conflicts (should not fail, just report)
echo -e "\n${YELLOW}=== Test 7: Simulation Mode with Conflicts ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"
echo "existing content" > "$TARGET_DIR/file1.txt"

run_test "Simulation mode with conflicts" "pass" \
    ./stash -n --dir "$PACKAGES_DIR" --target "$TARGET_DIR" simple

# Clean up
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

# Test 8: Verbose mode shows conflict details
echo -e "\n${YELLOW}=== Test 8: Verbose Mode Shows Conflict Details ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"
echo "existing content" > "$TARGET_DIR/file1.txt"

echo -e "${BLUE}Running with verbose mode to see conflict details:${NC}"
./stash -vv --dir "$PACKAGES_DIR" --target "$TARGET_DIR" simple 2>&1 || true

# Clean up
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

# Test 9: Directory with files vs symlink to directory
echo -e "\n${YELLOW}=== Test 9: Real Directory vs Symlink Directory ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR/bin"
echo "my script" > "$TARGET_DIR/bin/myscript"

run_test "Stash when target has real directory with files" "fail" \
    ./stash --dir "$PACKAGES_DIR" --target "$TARGET_DIR" emacs

# Clean up
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

# Test 10: Circular symlink
echo -e "\n${YELLOW}=== Test 10: Circular Symlink ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"
ln -s "$TARGET_DIR/file2.txt" "$TARGET_DIR/file1.txt"
ln -s "$TARGET_DIR/file1.txt" "$TARGET_DIR/file2.txt"

run_test "Stash with circular symlinks in target" "fail" \
    ./stash --dir "$PACKAGES_DIR" --target "$TARGET_DIR" simple

# Final cleanup
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

# Summary
echo -e "\n${BLUE}=== Test Summary ===${NC}"
echo -e "Total tests: $TOTAL_TESTS"
echo -e "${GREEN}Passed: $TESTS_PASSED${NC}"
echo -e "${RED}Failed: $TESTS_FAILED${NC}"

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "\n${GREEN}All conflict handling tests passed! ✓${NC}"
    exit 0
else
    echo -e "\n${RED}Some tests failed. Review the output above.${NC}"
    exit 1
fi
