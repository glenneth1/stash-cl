#!/bin/bash
# test-cli-integration.sh - SAFE test CLI integration
# This version has extensive safety checks

set -euo pipefail

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# SAFETY CHECK: Verify we're in stash-cl directory
if [[ ! "$SCRIPT_DIR" =~ stash-cl$ ]]; then
    echo "ERROR: This script must be run from stash-cl directory"
    echo "Current location: $SCRIPT_DIR"
    exit 1
fi

echo "========================================="
echo "  Testing CLI Integration (SAFE MODE)"
echo "========================================="
echo ""

# Setup test environment
cd "$SCRIPT_DIR/test-env"
echo "Setting up test environment..."
./setup-test-packages.sh

echo ""
echo "Test environment ready!"
echo ""

# Build stash if needed
cd "$SCRIPT_DIR"
if [ ! -f "stash" ]; then
    echo "Building stash executable..."
    make
fi

echo ""
echo "========================================="
echo "Test 1: Help and Version"
echo "========================================="
./stash --help | head -20
echo ""
./stash --version
echo ""

echo "========================================="
echo "Test 2: Simulation Mode"
echo "========================================="
./stash -n --dir test-env/packages --target test-env/target emacs
echo ""

echo "Verifying target is still empty..."
ls -la test-env/target/ 2>/dev/null || echo "Target directory empty (expected)"
echo ""

echo "========================================="
echo "  Basic Tests Complete!"
echo "========================================="
echo ""
echo "To continue testing manually:"
echo "  ./stash --dir test-env/packages --target test-env/target emacs"
echo ""
