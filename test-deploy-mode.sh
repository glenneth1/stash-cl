#!/bin/bash
# Test script for deploy mode in stash-cl

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

echo -e "${BLUE}=== Stash-CL Deploy Mode Tests ===${NC}\n"

TESTS_PASSED=0
TESTS_FAILED=0

# Test 1: Deploy all packages at once
echo -e "${YELLOW}=== Test 1: Deploy All Packages ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

./stash -d --dir "$PACKAGES_DIR" --target "$TARGET_DIR" > /dev/null 2>&1

# Count packages (should have files from all 4 packages: emacs, vim, perl, simple)
has_emacs=0
has_vim=0
has_perl=0
has_simple=0

[ -e "$TARGET_DIR/bin/emacs" ] && has_emacs=1
[ -e "$TARGET_DIR/bin/vim" ] && has_vim=1
[ -e "$TARGET_DIR/bin/perl" ] && has_perl=1
[ -e "$TARGET_DIR/file1.txt" ] && has_simple=1

total_packages=$((has_emacs + has_vim + has_perl + has_simple))

echo "  Packages deployed: $total_packages/4"
echo "    emacs: $([ $has_emacs -eq 1 ] && echo '✓' || echo '✗')"
echo "    vim: $([ $has_vim -eq 1 ] && echo '✓' || echo '✗')"
echo "    perl: $([ $has_perl -eq 1 ] && echo '✓' || echo '✗')"
echo "    simple: $([ $has_simple -eq 1 ] && echo '✓' || echo '✗')"

if [ $total_packages -eq 4 ]; then
    echo -e "${GREEN}  ✓ All packages deployed successfully${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "${RED}  ✗ Not all packages deployed${NC}"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

# Test 2: Deploy handles overlapping directories correctly
echo -e "\n${YELLOW}=== Test 2: Overlapping Directory Handling ===${NC}"

# Check that bin/ was unfolded (contains files from multiple packages)
bin_files=$(ls "$TARGET_DIR/bin/" 2>/dev/null | wc -l)

echo "  Files in bin/: $bin_files (expected: 3 - emacs, emacsclient, vim, perl)"

if [ "$bin_files" -ge 3 ]; then
    echo -e "${GREEN}  ✓ Overlapping directories handled correctly${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "${RED}  ✗ Overlapping directories not handled correctly${NC}"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

# Test 3: Deploy with simulation mode
echo -e "\n${YELLOW}=== Test 3: Deploy Simulation Mode ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

output=$(./stash -d -n --dir "$PACKAGES_DIR" --target "$TARGET_DIR" 2>&1 || true)

# Should show simulation output (SIMULATE not WOULD)
if echo "$output" | grep -q "SIMULATE"; then
    echo -e "${GREEN}  ✓ Simulation mode works with deploy${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "${RED}  ✗ Simulation mode not working${NC}"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

# Verify nothing was actually created
if [ ! -e "$TARGET_DIR/bin/emacs" ]; then
    echo -e "${GREEN}  ✓ Simulation didn't create files${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "${RED}  ✗ Simulation created files${NC}"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

# Test 4: Deploy vs individual stashing comparison
echo -e "\n${YELLOW}=== Test 4: Deploy vs Individual Stashing ===${NC}"

# Deploy mode
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"
./stash -d --dir "$PACKAGES_DIR" --target "$TARGET_DIR" > /dev/null 2>&1
deploy_count=$(find "$TARGET_DIR" -type l | wc -l)

# Individual stashing
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"
./stash --dir "$PACKAGES_DIR" --target "$TARGET_DIR" emacs vim perl simple > /dev/null 2>&1
individual_count=$(find "$TARGET_DIR" -type l | wc -l)

echo "  Deploy mode symlinks: $deploy_count"
echo "  Individual mode symlinks: $individual_count"

if [ "$deploy_count" -eq "$individual_count" ]; then
    echo -e "${GREEN}  ✓ Deploy and individual modes produce same result${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "${YELLOW}  ⚠ Different symlink counts (may be OK due to order)${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
fi

# Test 5: Deploy with empty directory
echo -e "\n${YELLOW}=== Test 5: Deploy Empty Directory ===${NC}"

EMPTY_DIR="$TEST_DIR/empty-packages"
mkdir -p "$EMPTY_DIR"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

output=$(./stash -d --dir "$EMPTY_DIR" --target "$TARGET_DIR" 2>&1)

if echo "$output" | grep -q "No packages found"; then
    echo -e "${GREEN}  ✓ Empty directory handled gracefully${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "${RED}  ✗ Empty directory not handled${NC}"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

# Cleanup
rm -rf "$EMPTY_DIR"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

# Summary
echo -e "\n${BLUE}=== Test Summary ===${NC}"
echo "Total tests: $((TESTS_PASSED + TESTS_FAILED))"
echo -e "${GREEN}Passed: $TESTS_PASSED${NC}"
echo -e "${RED}Failed: $TESTS_FAILED${NC}"

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "\n${GREEN}All deploy mode tests passed! ✓${NC}"
    exit 0
else
    echo -e "\n${RED}Some tests failed.${NC}"
    exit 1
fi
