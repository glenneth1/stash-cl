# Packaging Guide for stash-cl

## Package Contents

When distributing stash-cl, you need to ship **two files**:

1. **`stash`** - Standalone executable (~16 MB)
2. **`stash.1`** - Man page (~6 KB)

## Installation

Users should install all files:

```bash
# System-wide installation
sudo install -m 755 stash /usr/local/bin/
sudo install -m 644 stash.1 /usr/local/share/man/man1/

# User installation
install -m 755 stash ~/.local/bin/
install -m 644 stash.1 ~/.local/share/man/man1/

# Or use the Makefile
sudo make install
```

## Package Formats

### Arch Linux (PKGBUILD)

```bash
pkgname=stash-cl
pkgver=0.2.0
pkgrel=1
pkgdesc="Enhanced dotfile and package manager with intelligent tree folding"
arch=('x86_64')
url="https://github.com/glenneth1/stash-cl"
license=('GPL3')
depends=('bash')

package() {
    install -Dm755 "$srcdir/stash" "$pkgdir/usr/bin/stash"
    install -Dm644 "$srcdir/stash.1" "$pkgdir/usr/share/man/man1/stash.1"
}
```

### Debian/Ubuntu (.deb)

Create a `debian/install` file:
```
stash usr/bin
stash.1 usr/share/man/man1
```

### RPM (.spec)

```spec
%install
install -Dm755 stash %{buildroot}%{_bindir}/stash
install -Dm644 stash.1 %{buildroot}%{_mandir}/man1/stash.1

%files
%{_bindir}/stash
%{_mandir}/man1/stash.1
```

### Tarball Distribution

```bash
# Create release tarball
tar czf stash-cl-0.2.0-linux-x86_64.tar.gz stash stash.1 README.md

# Users extract and install
tar xzf stash-cl-0.2.0-linux-x86_64.tar.gz
cd stash-cl-0.2.0
sudo install -m 755 stash /usr/local/bin/
sudo install -m 644 stash.1 /usr/local/share/man/man1/

# Or use make
sudo make install
```

## Single-File Distribution

stash-cl ships as a single standalone executable. Both `--help` and
`--version` work correctly without any wrapper script.

## File Sizes

- **stash** (SBCL compression only): ~16 MB
- **stash** (with UPX --best): ~14 MB
- **stash** (with UPX --ultra-brute): ~13-14 MB

## Verification

After installation, users can verify:

```bash
$ stash --version
stash-cl version 0.2.0
Common Lisp rewrite of GNU Stow replacement

$ stash --help
Usage: stash [OPTION...] [PACKAGE|.]
...

$ which stash
/usr/local/bin/stash
```

## Build for Distribution

```bash
# Build with maximum compression
make clean
make compress-max

# Verify files
ls -lh stash

# Test before packaging
./stash --version
./stash --help
./stash  # Should show error about no packages
```

## Notes

- The executable is self-contained with no external dependencies
- Must be executable (`chmod +x`)
