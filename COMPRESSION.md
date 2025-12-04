# Compression Guide for stash-cl

This document explains the compression options available for reducing the size of the stash-cl executable.

## Current Size
- **Uncompressed**: ~121 MB
- **With SBCL compression**: ~40-60 MB (expected)
- **With SBCL + UPX compression**: ~15-25 MB (expected)
- **With SBCL + UPX --ultra-brute**: ~10-20 MB (expected, maximum compression)

## Compression Layers

### 1. SBCL Built-in Compression

The following settings are configured in `stash-cl.asd` and `build.lisp`:

**In stash-cl.asd:**
```lisp
:compression t              ; Enable core compression
:compression-level 9        ; Maximum compression level (0-9)
:strip-debug-info t         ; Remove debug information
:save-runtime-options nil   ; Don't save runtime options
```

**In build.lisp:**
```lisp
(declaim (optimize (speed 2) (space 3) (debug 0) (safety 1) (compilation-speed 0)))
```

This optimization declaration prioritizes space (size) over speed and removes debug info.

### 2. UPX Executable Compression

UPX (Ultimate Packer for eXecutables) provides additional compression on top of SBCL's compression.

## Build Commands

### Standard Build with SBCL Compression
```bash
make build
```
This builds the executable with SBCL's built-in compression enabled.

### Build with UPX Compression (Recommended)
```bash
make compress
```
This builds with SBCL compression and applies UPX with `--best --lzma` flags.
- **Compression time**: Fast (seconds to minutes)
- **Size reduction**: Good (typically 50-70% reduction from SBCL-only build)

### Build with Maximum UPX Compression
```bash
make compress-max
```
This applies the most aggressive UPX compression using `--ultra-brute --lzma`.
- **Compression time**: Slow (several minutes)
- **Size reduction**: Maximum (typically 60-80% reduction from SBCL-only build)
- **Note**: This is the slowest option but produces the smallest executable

## Installing UPX

### Debian/Ubuntu
```bash
sudo apt-get install upx-ucl
```

Or use the Makefile helper:
```bash
make install-upx
```

### macOS
```bash
brew install upx
```

### Other Systems
Download from: https://upx.github.io/

## Compression Trade-offs

### SBCL Compression
- **Pros**: No external dependencies, fast decompression at runtime
- **Cons**: Moderate compression ratio
- **Impact**: Minimal runtime performance impact

### UPX Compression
- **Pros**: Excellent compression ratio, transparent to the application
- **Cons**: Slightly slower startup time (decompression on first run)
- **Impact**: Small startup delay (typically <100ms), no runtime impact after decompression

### Ultra-Brute Compression
- **Pros**: Maximum size reduction
- **Cons**: Long compression time, slightly longer startup time
- **Impact**: Best for distribution, not recommended for development builds

## Recommended Workflow

### For Development
```bash
make build
```
Use standard SBCL compression for faster builds.

### For Distribution
```bash
make compress-max
```
Use maximum compression to minimize download size and disk usage.

### For Testing
```bash
make compress
```
Use standard UPX compression for a good balance.

## Verifying Compression

Check the executable size:
```bash
ls -lh stash
```

Check if UPX was applied:
```bash
upx -t stash
```

Get detailed UPX information:
```bash
upx -l stash
```

## Decompression (if needed)

To decompress a UPX-compressed executable:
```bash
upx -d stash
```

This creates a backup at `stash.upx.backup` before decompression.

## Troubleshooting

### UPX Not Found
If you get "UPX not found" errors, install it using the instructions above.

### Compression Fails
- Ensure you have enough disk space
- Try `make clean` before rebuilding
- For UPX issues, try `upx --version` to verify installation

### Executable Won't Run
If the compressed executable has issues:
1. Decompress with `upx -d stash`
2. Test the uncompressed version
3. Report any issues with compression settings

## Performance Notes

- **Startup time**: UPX-compressed executables decompress into memory on first run
- **Memory usage**: Slightly higher during decompression, normal afterwards
- **Runtime performance**: No difference after initial decompression
- **File I/O**: Unaffected by compression

## Additional Optimization Tips

1. **Strip symbols**: Already enabled via `:strip-debug-info t`
2. **Remove unused code**: Ensure no dead code in source files
3. **Minimize dependencies**: Review `stash-cl.asd` dependencies
4. **Tree shaking**: SBCL automatically removes unused code during compilation

## Size Comparison Example

Typical compression results:
```
Original (no compression):     121 MB  (100%)
SBCL compression only:         ~50 MB  (~41%)
SBCL + UPX --best:            ~20 MB  (~16%)
SBCL + UPX --ultra-brute:     ~15 MB  (~12%)
```

Actual results may vary based on code and dependencies.

## Known Limitation: SBCL Runtime Flags

SBCL's runtime intercepts `--version` and `--help` flags before the application code runs, displaying SBCL's own version/help instead of the application's. This is standard SBCL behavior when building standalone executables.

**Workaround options:**

1. **Use short flags**: `-h` works correctly for help
2. **Use the wrapper script**: `stash-wrapper.sh` intercepts these flags and handles them correctly
3. **Accept the limitation**: Document that `--version` shows SBCL version

The application is fully functional - this only affects these two specific flags.
