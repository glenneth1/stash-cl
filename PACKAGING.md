# Packaging Guide for stash-cl

## Package Contents

When distributing stash-cl, you need to ship **three files**:

1. **`stash`** - Shell wrapper script (405 bytes)
2. **`stash.bin`** - Compressed Common Lisp executable (~14-16 MB)
3. **`stash.1`** - Man page (~6 KB)

## Why Two Files?

SBCL's runtime intercepts `--version` and `--help` flags before the application code runs. The wrapper script handles these flags correctly and passes all other arguments to the binary.

## Installation

Users should install all files:

```bash
# System-wide installation
sudo install -m 755 stash /usr/local/bin/
sudo install -m 755 stash.bin /usr/local/bin/
sudo install -m 644 stash.1 /usr/local/share/man/man1/

# User installation
install -m 755 stash ~/.local/bin/
install -m 755 stash.bin ~/.local/bin/
install -m 644 stash.1 ~/.local/share/man/man1/

# Or use the Makefile
sudo make install
```

## Package Formats

### Arch Linux (PKGBUILD)

```bash
pkgname=stash-cl
pkgver=0.1.0
pkgrel=1
pkgdesc="Enhanced dotfile and package manager with intelligent tree folding"
arch=('x86_64')
url="https://github.com/yourusername/stash-cl"
license=('GPL3')
depends=('bash')

package() {
    install -Dm755 "$srcdir/stash" "$pkgdir/usr/bin/stash"
    install -Dm755 "$srcdir/stash.bin" "$pkgdir/usr/bin/stash.bin"
    install -Dm644 "$srcdir/stash.1" "$pkgdir/usr/share/man/man1/stash.1"
}
```

### Debian/Ubuntu (.deb)

Create a `debian/install` file:
```
stash usr/bin
stash.bin usr/bin
stash.1 usr/share/man/man1
```

### RPM (.spec)

```spec
%install
install -Dm755 stash %{buildroot}%{_bindir}/stash
install -Dm755 stash.bin %{buildroot}%{_bindir}/stash.bin
install -Dm644 stash.1 %{buildroot}%{_mandir}/man1/stash.1

%files
%{_bindir}/stash
%{_bindir}/stash.bin
%{_mandir}/man1/stash.1
```

### Tarball Distribution

```bash
# Create release tarball
tar czf stash-cl-0.1.0-linux-x86_64.tar.gz stash stash.bin stash.1 README.md

# Users extract and install
tar xzf stash-cl-0.1.0-linux-x86_64.tar.gz
cd stash-cl-0.1.0
sudo install -m 755 stash stash.bin /usr/local/bin/
sudo install -m 644 stash.1 /usr/local/share/man/man1/

# Or use make
sudo make install
```

## Single-File Alternative (Not Recommended)

If you absolutely need a single file, you can:

1. **Accept the limitation**: Ship only `stash.bin` and document that users should use `-h` instead of `--help`
2. **Embed the wrapper**: Use a self-extracting archive (adds complexity)

The two-file approach is cleaner and more maintainable.

## File Sizes

- **stash**: ~400 bytes (shell script)
- **stash.bin** (SBCL compression only): ~16 MB
- **stash.bin** (with UPX --best): ~14 MB  
- **stash.bin** (with UPX --ultra-brute): ~13-14 MB

Total package size: **~14-16 MB** (wrapper is negligible)

## Verification

After installation, users can verify:

```bash
$ stash --version
stash-cl version 0.1.0
Common Lisp rewrite of GNU Stow replacement

$ stash --help
Usage: stash [OPTION...] [PACKAGE|.]
...

$ which stash
/usr/local/bin/stash

$ which stash.bin
/usr/local/bin/stash.bin
```

## Build for Distribution

```bash
# Build with maximum compression
make clean
make compress-max

# Verify files
ls -lh stash stash.bin

# Test before packaging
./stash --version
./stash --help
./stash  # Should show error about no packages
```

## Notes

- The wrapper script is POSIX-compliant bash
- Both files must be executable (`chmod +x`)
- Both files must be in the same directory or in PATH
- The wrapper finds `stash.bin` relative to its own location
