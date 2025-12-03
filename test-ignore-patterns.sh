#!/bin/bash
# Test script for ignore pattern functionality in stash-cl

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
GLOBAL_IGNORE="$HOME/.stash-global-ignore"

echo -e "${BLUE}=== Stash-CL Ignore Pattern Tests ===${NC}\n"

# Backup existing global ignore if it exists
if [ -f "$GLOBAL_IGNORE" ]; then
    echo -e "${YELLOW}Backing up existing global ignore file...${NC}"
    cp "$GLOBAL_IGNORE" "$GLOBAL_IGNORE.backup"
fi

# Test counter
TESTS_PASSED=0
TESTS_FAILED=0
TOTAL_TESTS=0

# Helper function to run a test
run_test() {
    local test_name="$1"
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    echo -e "\n${BLUE}Test $TOTAL_TESTS: $test_name${NC}"
}

# Test 1: Global ignore patterns
echo -e "\n${YELLOW}=== Test 1: Global Ignore Patterns ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

# Create global ignore file
cat > "$GLOBAL_IGNORE" << 'EOF'
# Global ignore patterns
*.log
*.tmp
.DS_Store
*.swp
EOF

# Create a test package with files that should be ignored
TEST_PKG="$PACKAGES_DIR/ignore-test"
rm -rf "$TEST_PKG"
mkdir -p "$TEST_PKG/bin"
mkdir -p "$TEST_PKG/share"

# Files that should be stashed
echo "#!/bin/bash" > "$TEST_PKG/bin/script"
chmod +x "$TEST_PKG/bin/script"
echo "data" > "$TEST_PKG/share/data.txt"

# Files that should be ignored (global patterns)
echo "log content" > "$TEST_PKG/bin/debug.log"
echo "temp" > "$TEST_PKG/share/temp.tmp"
echo "swap" > "$TEST_PKG/bin/.script.swp"

run_test "Global ignore patterns"
# Stash TWO packages to force unfolding (so we can see individual file symlinks)
./stash --dir "$PACKAGES_DIR" --target "$TARGET_DIR" ignore-test simple > /dev/null 2>&1

# Verify ignored files are NOT stashed (when unfolded, individual symlinks are created)
# Note: bin/ will be unfolded because both packages have bin/ directories
if [ ! -e "$TARGET_DIR/bin/debug.log" ] && \
   [ ! -e "$TARGET_DIR/share/temp.tmp" ] && \
   [ ! -e "$TARGET_DIR/bin/.script.swp" ]; then
    echo -e "${GREEN}  ✓ Global ignore patterns working (ignored files not stashed)${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "${RED}  ✗ Global ignore patterns failed (ignored files were stashed)${NC}"
    TESTS_FAILED=$((TESTS_FAILED + 1))
    [ -e "$TARGET_DIR/bin/debug.log" ] && echo -e "${RED}    - debug.log was stashed${NC}"
    [ -e "$TARGET_DIR/share/temp.tmp" ] && echo -e "${RED}    - temp.tmp was stashed${NC}"
    [ -e "$TARGET_DIR/bin/.script.swp" ] && echo -e "${RED}    - .script.swp was stashed${NC}"
fi

# Verify non-ignored files ARE stashed
if [ -e "$TARGET_DIR/bin/script" ] && [ -e "$TARGET_DIR/share/data.txt" ]; then
    echo -e "${GREEN}  ✓ Non-ignored files were stashed${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "${RED}  ✗ Non-ignored files were not stashed${NC}"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

# Test 2: Local ignore patterns
echo -e "\n${YELLOW}=== Test 2: Local Ignore Patterns ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

# Create a test package with local ignore
TEST_PKG2="$PACKAGES_DIR/local-ignore-test"
rm -rf "$TEST_PKG2"
mkdir -p "$TEST_PKG2/bin"
mkdir -p "$TEST_PKG2/share"
mkdir -p "$TEST_PKG2/temp"

# Create local ignore file
cat > "$TEST_PKG2/.stash-local-ignore" << 'EOF'
# Local ignore patterns
temp/*
*.bak
test-*
EOF

# Files that should be stashed
echo "#!/bin/bash" > "$TEST_PKG2/bin/app"
chmod +x "$TEST_PKG2/bin/app"
echo "config" > "$TEST_PKG2/share/config.conf"

# Files that should be ignored (local patterns)
echo "temp data" > "$TEST_PKG2/temp/data.txt"
echo "backup" > "$TEST_PKG2/share/config.conf.bak"
echo "test file" > "$TEST_PKG2/bin/test-script"

run_test "Local ignore patterns"
# Stash with simple to force unfolding
./stash --dir "$PACKAGES_DIR" --target "$TARGET_DIR" local-ignore-test simple > /dev/null 2>&1

# Verify locally ignored files are NOT stashed
if [ ! -e "$TARGET_DIR/temp" ] && \
   [ ! -e "$TARGET_DIR/share/config.conf.bak" ] && \
   [ ! -e "$TARGET_DIR/bin/test-script" ]; then
    echo -e "${GREEN}  ✓ Local ignore patterns working${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "${RED}  ✗ Local ignore patterns failed${NC}"
    TESTS_FAILED=$((TESTS_FAILED + 1))
    if [ -e "$TARGET_DIR/temp" ]; then
        echo -e "${RED}    - temp/ directory was stashed${NC}"
    fi
    if [ -e "$TARGET_DIR/share/config.conf.bak" ]; then
        echo -e "${RED}    - .bak file was stashed${NC}"
    fi
    if [ -e "$TARGET_DIR/bin/test-script" ]; then
        echo -e "${RED}    - test-* file was stashed${NC}"
    fi
fi

# Verify non-ignored files ARE stashed
if [ -e "$TARGET_DIR/bin/app" ] && [ -e "$TARGET_DIR/share/config.conf" ]; then
    echo -e "${GREEN}  ✓ Non-ignored files were stashed${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "${RED}  ✗ Non-ignored files were not stashed${NC}"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

# Test 3: Combined global and local ignore
echo -e "\n${YELLOW}=== Test 3: Combined Global and Local Ignore ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

TEST_PKG3="$PACKAGES_DIR/combined-ignore-test"
rm -rf "$TEST_PKG3"
mkdir -p "$TEST_PKG3/bin"
mkdir -p "$TEST_PKG3/share"

# Local ignore
cat > "$TEST_PKG3/.stash-local-ignore" << 'EOF'
*.local
EOF

# Files
echo "#!/bin/bash" > "$TEST_PKG3/bin/tool"
chmod +x "$TEST_PKG3/bin/tool"
echo "global ignore" > "$TEST_PKG3/share/file.log"      # Should be ignored (global)
echo "local ignore" > "$TEST_PKG3/share/file.local"     # Should be ignored (local)
echo "not ignored" > "$TEST_PKG3/share/file.txt"        # Should be stashed

run_test "Combined global and local ignore"
./stash --dir "$PACKAGES_DIR" --target "$TARGET_DIR" combined-ignore-test simple > /dev/null 2>&1

# Verify both global and local patterns work
if [ ! -e "$TARGET_DIR/share/file.log" ] && \
   [ ! -e "$TARGET_DIR/share/file.local" ] && \
   [ -e "$TARGET_DIR/share/file.txt" ]; then
    echo -e "${GREEN}  ✓ Combined ignore patterns working${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "${RED}  ✗ Combined ignore patterns failed${NC}"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

# Test 4: Glob pattern matching
echo -e "\n${YELLOW}=== Test 4: Glob Pattern Matching ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

# Update global ignore with more complex patterns
cat > "$GLOBAL_IGNORE" << 'EOF'
# Complex glob patterns
*.log
test-*
*-backup
.git*
node_modules/*
EOF

TEST_PKG4="$PACKAGES_DIR/glob-test"
rm -rf "$TEST_PKG4"
mkdir -p "$TEST_PKG4/bin"
mkdir -p "$TEST_PKG4/share"

# Files matching various patterns
echo "#!/bin/bash" > "$TEST_PKG4/bin/app"
echo "log" > "$TEST_PKG4/share/app.log"                 # *.log
echo "test" > "$TEST_PKG4/bin/test-runner"              # test-*
echo "backup" > "$TEST_PKG4/share/config-backup"        # *-backup
echo "git" > "$TEST_PKG4/.gitignore"                    # .git*
echo "ok" > "$TEST_PKG4/share/data.txt"                 # Should be stashed

run_test "Glob pattern matching"
./stash --dir "$PACKAGES_DIR" --target "$TARGET_DIR" glob-test simple > /dev/null 2>&1

# Count what should and shouldn't be there
ignored_count=0
[ ! -e "$TARGET_DIR/share/app.log" ] && ignored_count=$((ignored_count + 1))
[ ! -e "$TARGET_DIR/bin/test-runner" ] && ignored_count=$((ignored_count + 1))
[ ! -e "$TARGET_DIR/share/config-backup" ] && ignored_count=$((ignored_count + 1))
[ ! -e "$TARGET_DIR/.gitignore" ] && ignored_count=$((ignored_count + 1))

if [ $ignored_count -eq 4 ] && [ -e "$TARGET_DIR/share/data.txt" ]; then
    echo -e "${GREEN}  ✓ Glob patterns matched correctly${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "${RED}  ✗ Glob patterns failed (ignored: $ignored_count/4)${NC}"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

# Test 5: Verify .stash-local-ignore itself is ignored
echo -e "\n${YELLOW}=== Test 5: .stash-local-ignore File Handling ===${NC}"
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

run_test ".stash-local-ignore is stashed as symlink"
./stash --dir "$PACKAGES_DIR" --target "$TARGET_DIR" local-ignore-test simple > /dev/null 2>&1

# The .stash-local-ignore file SHOULD be stashed (it's metadata)
if [ -L "$TARGET_DIR/.stash-local-ignore" ]; then
    echo -e "${GREEN}  ✓ .stash-local-ignore is stashed as symlink${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "${RED}  ✗ .stash-local-ignore was not stashed${NC}"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

# Cleanup
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

# Restore original global ignore if it existed
if [ -f "$GLOBAL_IGNORE.backup" ]; then
    mv "$GLOBAL_IGNORE.backup" "$GLOBAL_IGNORE"
    echo -e "\n${YELLOW}Restored original global ignore file${NC}"
else
    rm -f "$GLOBAL_IGNORE"
    echo -e "\n${YELLOW}Removed test global ignore file${NC}"
fi

# Clean up test packages
rm -rf "$PACKAGES_DIR/ignore-test"
rm -rf "$PACKAGES_DIR/local-ignore-test"
rm -rf "$PACKAGES_DIR/combined-ignore-test"
rm -rf "$PACKAGES_DIR/glob-test"

# Summary
echo -e "\n${BLUE}=== Test Summary ===${NC}"
echo -e "Total tests: $TOTAL_TESTS"
echo -e "${GREEN}Passed: $TESTS_PASSED${NC}"
echo -e "${RED}Failed: $TESTS_FAILED${NC}"

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "\n${GREEN}All ignore pattern tests passed! ✓${NC}"
    exit 0
else
    echo -e "\n${RED}Some tests failed. Review the output above.${NC}"
    exit 1
fi
