# Makefile for stash-cl

.PHONY: all build clean test

all: build

build:
	@echo "Building stash-cl executable..."
	sbcl --load build.lisp

clean:
	@echo "Cleaning build artifacts..."
	rm -f stash
	rm -rf ~/.cache/common-lisp/sbcl-*-linux-x64/home/$(USER)/SourceCode/stash-cl/

test: build
	@echo "Running tests..."
	./test-cli-integration.sh

help:
	@echo "Available targets:"
	@echo "  make build  - Build the stash executable"
	@echo "  make clean  - Remove build artifacts"
	@echo "  make test   - Run integration tests"
	@echo "  make help   - Show this help"
