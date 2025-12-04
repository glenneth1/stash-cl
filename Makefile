# Makefile for stash-cl

.PHONY: all build clean test compress compress-max install-upx

all: build

build:
	@echo "Building stash-cl executable with compression..."
	sbcl --load build.lisp
	@echo "Creating wrapper script..."
	@echo '#!/bin/bash' > stash
	@echo '# Wrapper for stash to handle --version and --help' >> stash
	@echo 'SCRIPT_DIR="$$(cd "$$(dirname "$${BASH_SOURCE[0]}")" && pwd)"' >> stash
	@echo 'STASH_BIN="$$SCRIPT_DIR/stash.bin"' >> stash
	@echo 'for arg in "$$@"; do' >> stash
	@echo '  if [ "$$arg" = "--version" ]; then' >> stash
	@echo '    echo "stash-cl version 0.1.0"' >> stash
	@echo '    echo "Common Lisp rewrite of GNU Stow replacement"' >> stash
	@echo '    exit 0' >> stash
	@echo '  elif [ "$$arg" = "--help" ]; then' >> stash
	@echo '    exec "$$STASH_BIN" -h' >> stash
	@echo '  fi' >> stash
	@echo 'done' >> stash
	@echo 'exec "$$STASH_BIN" "$$@"' >> stash
	@chmod +x stash
	@echo "Build complete! Use ./stash or compress with 'make compress'"

# Apply UPX compression (requires upx to be installed)
compress: build
	@echo "Applying UPX compression to stash.bin..."
	@if command -v upx >/dev/null 2>&1; then \
		upx --best --lzma stash.bin; \
	else \
		echo "Error: UPX not found. Install with: sudo apt-get install upx-ucl (Debian/Ubuntu) or brew install upx (macOS)"; \
		exit 1; \
	fi

# Apply maximum UPX compression (slowest but smallest)
compress-max: build
	@echo "Applying maximum UPX compression to stash.bin (this may take a while)..."
	@if command -v upx >/dev/null 2>&1; then \
		upx --ultra-brute --lzma stash.bin; \
	else \
		echo "Error: UPX not found. Install with: sudo apt-get install upx-ucl (Debian/Ubuntu) or brew install upx (macOS)"; \
		exit 1; \
	fi

# Helper to install UPX
install-upx:
	@echo "Installing UPX..."
	@if command -v pacman >/dev/null 2>&1; then \
		sudo pacman -S --noconfirm upx; \
	elif command -v apt-get >/dev/null 2>&1; then \
		sudo apt-get update && sudo apt-get install -y upx-ucl; \
	elif command -v dnf >/dev/null 2>&1; then \
		sudo dnf install -y upx; \
	elif command -v yum >/dev/null 2>&1; then \
		sudo yum install -y upx; \
	elif command -v zypper >/dev/null 2>&1; then \
		sudo zypper install -y upx; \
	elif command -v brew >/dev/null 2>&1; then \
		brew install upx; \
	else \
		echo "Unsupported package manager. Please install UPX manually:"; \
		echo "  Arch/Manjaro: sudo pacman -S upx"; \
		echo "  Debian/Ubuntu: sudo apt-get install upx-ucl"; \
		echo "  Fedora: sudo dnf install upx"; \
		echo "  macOS: brew install upx"; \
		exit 1; \
	fi

clean:
	@echo "Cleaning build artifacts..."
	rm -f stash stash.bin stash.bin.upx.backup
	rm -rf ~/.cache/common-lisp/sbcl-*-linux-x64/home/$(USER)/SourceCode/stash-cl/

test: build
	@echo "Running tests..."
	./test-cli-integration.sh

help:
	@echo "Available targets:"
	@echo "  make build        - Build the stash executable with SBCL compression"
	@echo "  make compress     - Build and apply UPX compression (--best --lzma)"
	@echo "  make compress-max - Build and apply maximum UPX compression (--ultra-brute)"
	@echo "  make install-upx  - Install UPX compression tool"
	@echo "  make clean        - Remove build artifacts"
	@echo "  make test         - Run integration tests"
	@echo "  make help         - Show this help"
