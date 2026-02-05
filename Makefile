# Makefile for stash-cl

.PHONY: all build clean test compress compress-max install-upx install install-man uninstall

all: build

# File target - only rebuild if stash doesn't exist
stash:
	@echo "Building stash-cl executable with compression..."
	sbcl --non-interactive --load build.lisp
	@echo "Build complete! Use ./stash or compress with 'make compress'"

build: stash

# Apply UPX compression (requires upx to be installed)
compress: build
	@echo "Applying UPX compression to stash..."
	@if command -v upx >/dev/null 2>&1; then \
		upx --best --lzma stash; \
	else \
		echo "Error: UPX not found. Install with: sudo apt-get install upx-ucl (Debian/Ubuntu) or brew install upx (macOS)"; \
		exit 1; \
	fi

# Apply maximum UPX compression (slowest but smallest)
compress-max: build
	@echo "Applying maximum UPX compression to stash (this may take a while)..."
	@if command -v upx >/dev/null 2>&1; then \
		upx --ultra-brute --lzma stash; \
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
	rm -f stash stash.upx.backup
	rm -rf ~/.cache/common-lisp/sbcl-*-linux-x64/home/$(USER)/SourceCode/stash-cl/

test: build
	@echo "Running tests..."
	./test-cli-integration.sh

# Installation
PREFIX ?= /usr/local
BINDIR = $(PREFIX)/bin
MANDIR = $(PREFIX)/share/man/man1

install: stash
	@echo "Installing stash to $(BINDIR)..."
	install -d $(BINDIR)
	install -m 755 stash $(BINDIR)/
	@echo "Installing man page to $(MANDIR)..."
	install -d $(MANDIR)
	install -m 644 stash.1 $(MANDIR)/
	@echo "Installation complete!"
	@echo "Run 'stash --version' to verify"

install-man:
	@echo "Installing man page to $(MANDIR)..."
	install -d $(MANDIR)
	install -m 644 stash.1 $(MANDIR)/
	@echo "Man page installed. Run 'man stash' to view"

uninstall:
	@echo "Uninstalling stash..."
	rm -f $(BINDIR)/stash
	rm -f $(MANDIR)/stash.1
	@echo "Uninstall complete"

help:
	@echo "Available targets:"
	@echo "  make build        - Build the stash executable with SBCL compression"
	@echo "  make compress     - Build and apply UPX compression (--best --lzma)"
	@echo "  make compress-max - Build and apply maximum UPX compression (--ultra-brute)"
	@echo "  make install      - Install stash and man page to $(PREFIX)"
	@echo "  make install-man  - Install only the man page"
	@echo "  make uninstall    - Remove installed files"
	@echo "  make install-upx  - Install UPX compression tool"
	@echo "  make clean        - Remove build artifacts"
	@echo "  make test         - Run integration tests"
	@echo "  make help         - Show this help"
