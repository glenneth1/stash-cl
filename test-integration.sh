#!/bin/bash
# test-integration.sh - Comprehensive integration tests for stash-cl
# Tests CLI features: stash, unstash, deploy, list, conflicts, config, completion, simulate

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [[ ! "$SCRIPT_DIR" =~ stash-cl$ ]]; then
    echo "ERROR: This script must be run from stash-cl directory"
    exit 1
fi

PASS=0
FAIL=0
FAILURES=""

assert_contains() {
    local output="$1"
    local expected="$2"
    local name="$3"
    if echo "$output" | grep -q -- "$expected"; then
        PASS=$((PASS + 1))
    else
        FAIL=$((FAIL + 1))
        FAILURES="$FAILURES\n  FAIL: $name - expected '$expected' in output"
    fi
}

assert_not_contains() {
    local output="$1"
    local unexpected="$2"
    local name="$3"
    if ! echo "$output" | grep -q -- "$unexpected"; then
        PASS=$((PASS + 1))
    else
        FAIL=$((FAIL + 1))
        FAILURES="$FAILURES\n  FAIL: $name - '$unexpected' should not be in output"
    fi
}

assert_exit_code() {
    local actual="$1"
    local expected="$2"
    local name="$3"
    if [ "$actual" -eq "$expected" ]; then
        PASS=$((PASS + 1))
    else
        FAIL=$((FAIL + 1))
        FAILURES="$FAILURES\n  FAIL: $name - expected exit code $expected, got $actual"
    fi
}

assert_file_exists() {
    local path="$1"
    local name="$2"
    if [ -e "$path" ]; then
        PASS=$((PASS + 1))
    else
        FAIL=$((FAIL + 1))
        FAILURES="$FAILURES\n  FAIL: $name - file should exist: $path"
    fi
}

assert_file_not_exists() {
    local path="$1"
    local name="$2"
    if [ ! -e "$path" ]; then
        PASS=$((PASS + 1))
    else
        FAIL=$((FAIL + 1))
        FAILURES="$FAILURES\n  FAIL: $name - file should not exist: $path"
    fi
}

assert_symlink() {
    local path="$1"
    local name="$2"
    if [ -L "$path" ]; then
        PASS=$((PASS + 1))
    else
        FAIL=$((FAIL + 1))
        FAILURES="$FAILURES\n  FAIL: $name - expected symlink at: $path"
    fi
}

setup_test_env() {
    local test_dir="$1"
    rm -rf "$test_dir"
    mkdir -p "$test_dir/packages/emacs/.emacs.d"
    mkdir -p "$test_dir/packages/vim/.vim"
    mkdir -p "$test_dir/packages/simple"
    echo "(setq foo 1)" > "$test_dir/packages/emacs/.emacs.d/init.el"
    echo "set number" > "$test_dir/packages/vim/.vim/vimrc"
    echo "hello" > "$test_dir/packages/simple/file.txt"
    mkdir -p "$test_dir/target"
}

cleanup_test_env() {
    local test_dir="$1"
    rm -rf "$test_dir"
}

STASH="$SCRIPT_DIR/stash"
if [ ! -f "$STASH" ]; then
    echo "Building stash executable..."
    cd "$SCRIPT_DIR"
    make build
fi

echo "========================================="
echo "  Integration Tests for stash-cl v0.3.0"
echo "========================================="
echo ""

TEST_DIR="/tmp/stash-integration-test"
STASH_DIR="$TEST_DIR/packages"
TARGET_DIR="$TEST_DIR/target"

# =========================================
echo "--- Test Group: Help and Version ---"
# =========================================

OUT=$("$STASH" --help 2>&1)
assert_contains "$OUT" "Usage: stash" "help shows usage"
assert_contains "$OUT" "--conflicts" "help shows --conflicts"
assert_contains "$OUT" "--interactive" "help shows --interactive"
assert_contains "$OUT" "--completion" "help shows --completion"
assert_contains "$OUT" "Configuration:" "help shows config section"

OUT=$("$STASH" --version 2>&1)
assert_contains "$OUT" "0.3.0" "version shows 0.3.0"

# =========================================
echo "--- Test Group: Shell Completion ---"
# =========================================

OUT=$("$STASH" --completion=bash 2>&1)
assert_contains "$OUT" "_stash()" "bash completion has function"
assert_contains "$OUT" "complete -F _stash stash" "bash completion has complete command"
assert_contains "$OUT" "--conflicts" "bash completion includes --conflicts"

OUT=$("$STASH" --completion=zsh 2>&1)
assert_contains "$OUT" "#compdef stash" "zsh completion has compdef"
assert_contains "$OUT" "--interactive" "zsh completion includes --interactive"

OUT=$("$STASH" --completion=fish 2>&1)
assert_contains "$OUT" "complete -c stash" "fish completion has complete"
assert_contains "$OUT" "conflicts" "fish completion includes conflicts"

OUT=$("$STASH" --completion=invalid 2>&1 || true)
assert_contains "$OUT" "Unknown shell" "invalid shell shows error"

# =========================================
echo "--- Test Group: Simulation Mode ---"
# =========================================

setup_test_env "$TEST_DIR"

OUT=$("$STASH" -n --dir "$STASH_DIR" --target "$TARGET_DIR" emacs 2>&1)
assert_contains "$OUT" "SIMULATION MODE" "simulate shows simulation banner"
assert_contains "$OUT" "emacs" "simulate mentions package"
assert_file_not_exists "$TARGET_DIR/.emacs.d" "simulate does not create symlinks"

# =========================================
echo "--- Test Group: Stash and Verify ---"
# =========================================

OUT=$("$STASH" --dir "$STASH_DIR" --target "$TARGET_DIR" emacs 2>&1)
assert_contains "$OUT" "Stashing package: emacs" "stash shows package name"
assert_file_exists "$TARGET_DIR/.emacs.d" "target .emacs.d exists after stash"
assert_symlink "$TARGET_DIR/.emacs.d" "target .emacs.d is symlink"

# =========================================
echo "--- Test Group: List ---"
# =========================================

OUT=$("$STASH" -l --dir "$STASH_DIR" --target "$TARGET_DIR" 2>&1)
assert_contains "$OUT" "emacs" "list shows emacs"
assert_contains "$OUT" "vim" "list shows vim"
assert_contains "$OUT" "stashed" "list shows stashed status for emacs"

# =========================================
echo "--- Test Group: Conflicts Flag ---"
# =========================================

# No conflict case
OUT=$("$STASH" --dir "$STASH_DIR" --target "$TARGET_DIR" --conflicts vim 2>&1)
assert_contains "$OUT" "No conflicts detected" "conflicts: no conflicts for vim"

# Create a conflict: make .vim a real directory with a file
rm -f "$TARGET_DIR/.vim" 2>/dev/null || true
mkdir -p "$TARGET_DIR/.vim"
echo "existing" > "$TARGET_DIR/.vim/vimrc"

OUT=$("$STASH" --dir "$STASH_DIR" --target "$TARGET_DIR" --conflicts vim 2>&1 || true)
assert_contains "$OUT" "Conflicts" "conflicts: detects conflict for vim"
assert_contains "$OUT" "vimrc" "conflicts: mentions conflicting file"

# =========================================
echo "--- Test Group: Unstash ---"
# =========================================

# Re-stash emacs cleanly first
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"
"$STASH" --dir "$STASH_DIR" --target "$TARGET_DIR" emacs >/dev/null 2>&1

OUT=$("$STASH" -D --dir "$STASH_DIR" --target "$TARGET_DIR" emacs 2>&1)
assert_contains "$OUT" "Unstashing" "unstash shows message"
assert_file_not_exists "$TARGET_DIR/.emacs.d" "unstash removes symlink"

# =========================================
echo "--- Test Group: Deploy ---"
# =========================================

rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

OUT=$("$STASH" -d --dir "$STASH_DIR" --target "$TARGET_DIR" 2>&1)
assert_contains "$OUT" "Deploying" "deploy shows deploying message"
assert_contains "$OUT" "emacs" "deploy processes emacs"
assert_file_exists "$TARGET_DIR/.emacs.d" "deploy creates emacs symlinks"
assert_file_exists "$TARGET_DIR/file.txt" "deploy creates simple symlinks"

# =========================================
echo "--- Test Group: Config File ---"
# =========================================

# Create config file
CONFIG_DIR="/tmp/stash-config-integration"
mkdir -p "$CONFIG_DIR/stash"
cat > "$CONFIG_DIR/stash/config" << CFGEOF
dir = $STASH_DIR
target = $TARGET_DIR
CFGEOF

# Clean target
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

OUT=$(XDG_CONFIG_HOME="$CONFIG_DIR" "$STASH" --conflicts emacs 2>&1)
assert_contains "$OUT" "$STASH_DIR" "config: uses dir from config file"
assert_contains "$OUT" "$TARGET_DIR" "config: uses target from config file"
assert_contains "$OUT" "No conflicts" "config: works with conflicts check"

# Test that CLI overrides config
rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"
OUT=$(XDG_CONFIG_HOME="$CONFIG_DIR" "$STASH" --dir /nonexistent --conflicts emacs 2>&1 || true)
assert_contains "$OUT" "does not exist" "config: CLI overrides config dir"

# Test ~/.stashrc
rm -rf "$CONFIG_DIR"
FAKE_HOME="/tmp/stash-fake-home"
mkdir -p "$FAKE_HOME"
cat > "$FAKE_HOME/.stashrc" << CFGEOF
dir = $STASH_DIR
target = $TARGET_DIR
CFGEOF

rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"
OUT=$(HOME="$FAKE_HOME" "$STASH" --conflicts vim 2>&1)
assert_contains "$OUT" "No conflicts" "config: .stashrc works"

rm -rf "$FAKE_HOME"

# =========================================
echo "--- Test Group: Ignore Patterns ---"
# =========================================

rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

OUT=$("$STASH" -v --dir "$STASH_DIR" --target "$TARGET_DIR" --ignore='.*\.txt' simple 2>&1)
assert_contains "$OUT" "1 CLI ignore pattern" "ignore: shows pattern count"
assert_file_not_exists "$TARGET_DIR/file.txt" "ignore: .txt file not symlinked"

# =========================================
echo "--- Test Group: No Folding ---"
# =========================================

rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

OUT=$("$STASH" -v --no-folding --dir "$STASH_DIR" --target "$TARGET_DIR" emacs 2>&1)
assert_contains "$OUT" "File symlinks created" "no-folding: creates file symlinks"
assert_file_exists "$TARGET_DIR/.emacs.d/init.el" "no-folding: creates file-level symlink"

# =========================================
echo "--- Test Group: Import ---"
# =========================================

rm -rf "$TEST_DIR"
mkdir -p "$STASH_DIR"
mkdir -p "$TARGET_DIR/.config"
echo "test config" > "$TARGET_DIR/.config/testapp"

OUT=$("$STASH" -i "$TARGET_DIR/.config/testapp" -p testapp --dir "$STASH_DIR" --target "$TARGET_DIR" 2>&1)
assert_contains "$OUT" "Import complete" "import: completes successfully"
assert_file_exists "$STASH_DIR/testapp/.config/testapp" "import: file moved to package"
assert_symlink "$TARGET_DIR/.config/testapp" "import: symlink created in target"

# =========================================
echo "--- Test Group: Restash ---"
# =========================================

setup_test_env "$TEST_DIR"
"$STASH" --dir "$STASH_DIR" --target "$TARGET_DIR" emacs >/dev/null 2>&1
assert_symlink "$TARGET_DIR/.emacs.d" "restash: symlink exists before restash"

OUT=$("$STASH" -R --dir "$STASH_DIR" --target "$TARGET_DIR" emacs 2>&1)
assert_contains "$OUT" "Restashing" "restash: shows restash message"
assert_contains "$OUT" "Phase 1" "restash: shows phase 1"
assert_contains "$OUT" "Phase 2" "restash: shows phase 2"
assert_symlink "$TARGET_DIR/.emacs.d" "restash: symlink exists after restash"

# =========================================
echo "--- Test Group: Verbose ---"
# =========================================

rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"

OUT=$("$STASH" -vv --dir "$STASH_DIR" --target "$TARGET_DIR" emacs 2>&1)
assert_contains "$OUT" "Folding" "verbose: shows folding info"

# =========================================
# Cleanup
# =========================================

cleanup_test_env "$TEST_DIR"
rm -rf /tmp/stash-config-integration /tmp/stash-fake-home

# =========================================
# Results
# =========================================

echo ""
echo "========================================="
echo "  Integration Test Results"
echo "========================================="
echo "  Passed: $PASS"
echo "  Failed: $FAIL"
if [ "$FAIL" -gt 0 ]; then
    echo -e "Failures:$FAILURES"
fi
echo "========================================="

if [ "$FAIL" -eq 0 ]; then
    echo "All tests passed!"
    exit 0
else
    echo "$FAIL test(s) failed!"
    exit 1
fi
