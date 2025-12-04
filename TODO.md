# Stash-CL TODO List

## Project Status
**Current Version**: 0.1.0  
**Core Features**: ✅ Complete  
**Production Ready**: Yes (for most use cases)

---

## Priority 1: Critical Testing (Week 1-2)

### Testing Coverage - PARTIALLY COMPLETE

#### ✅ Already Tested
- [x] **Basic stash operation** - PASS
- [x] **Unstash operation** - PASS  
- [x] **Restash operation** - PASS
- [x] **Verbosity levels (0-2)** - PASS
- [x] **Simulation mode** - PASS
- [x] **Tree folding** - PASS
- [x] **Task planning system** - PASS

#### ⚠️ Still Need Testing
- [x] **Test conflict handling** (HIGH) ✅ COMPLETE
  - [x] Test existing files in target
  - [x] Test existing directories in target
  - [x] Test broken symlinks
  - [x] Test circular symlinks
  - [x] Verify error messages are clear
  - [x] Test idempotent operations (re-stash)
  - [x] Test simulation mode with conflicts
  - **Time spent**: 3 hours
  - **Result**: All 9 tests passing

- [x] **Test multiple packages** (HIGH) ✅ COMPLETE
  - [x] Test stashing 2 packages with overlapping directories
  - [x] Test stashing 3+ packages
  - [x] Test unfolding preserves original package content
  - [x] Verify all packages' files present after unfolding
  - **Time spent**: 2 hours
  - **Result**: All packages work correctly, content preserved

- [x] **Test ignore patterns** (HIGH) ✅ COMPLETE
  - [x] Integrated ignore pattern reading into folding
  - [x] Added glob-to-regex conversion
  - [x] Basic patterns work (*.log, *.tmp)
  - [x] Complex glob patterns (test-*, *-backup, .git*)
  - [x] Directory patterns (temp/*, node_modules/*)
  - [x] Prevent folding directories with ignored files
  - **Status**: All 7 tests passing
  - **Time spent**: 3 hours
  - **Result**: Fully functional ignore system

- [x] **Test no-folding mode** (MEDIUM) ✅ COMPLETE
  - [x] Verify individual file symlinks created
  - [x] Compare with folding mode behavior
  - [x] Test unstash compatibility
  - [x] Verify file accessibility in both modes
  - **Time spent**: 15 minutes
  - **Result**: All 5 tests passing

- [x] **Test deploy mode** (LOW) ✅ COMPLETE
  - [x] Deploy all packages in directory
  - [x] Verify overlapping directory handling
  - [x] Test simulation mode compatibility
  - [x] Compare with individual stashing
  - **Time spent**: 20 minutes
  - **Result**: All 6 tests passing

- [ ] **Expand automated test suite** (HIGH)
  - Convert remaining manual tests to automated scripts
  - Add CI/CD integration (GitHub Actions)
  - Test on multiple SBCL versions
  - **Estimated**: 4-6 hours

- [x] **Executable compression** (MEDIUM) ✅ COMPLETE
  - [x] Implement SBCL compression (level 9)
  - [x] Add UPX compression support
  - [x] Create wrapper script for --version/--help
  - [x] Multi-platform package manager support
  - **Result**: 88% size reduction (121MB → 14MB)
  - **Time spent**: 2 hours

**Remaining Time**: 4-6 hours (1 week part-time)  
**Progress**: ~85% complete (all core features tested, ignore patterns working, compression implemented)

---

## Priority 2: Missing Core Features (Week 3-4)

### High Priority Features

- [x] **Implement `--adopt` flag** (HIGH) ✅ COMPLETE
  - [x] Move existing files into package directory
  - [x] Useful for managing existing dotfiles
  - [x] Replace file with symlink to adopted version
  - [x] Handles subdirectories recursively
  - [x] Works in simulation mode
  - **Time spent**: 2 hours
  - **Result**: Fully functional adoption system

- [x] **Implement `--ignore=REGEX` CLI flag** (MEDIUM) ✅ COMPLETE
  - [x] Add command-line regex ignore patterns
  - [x] Combine with ignore file patterns
  - [x] Support multiple `--ignore` flags
  - [x] Integrated with folding system
  - **Time spent**: 30 minutes
  - **Result**: CLI patterns merge with file-based patterns

- [x] **Improve error messages** (MEDIUM) ✅ COMPLETE
  - [x] Add color coding for errors/warnings
  - [x] Provide actionable suggestions
  - [x] Better formatting with format-error helper
  - **Time spent**: 1 hour
  - **Result**: Colored, helpful error messages

**Total Time Spent**: 3.5 hours
**Status**: ✅ ALL COMPLETE

---

## Priority 3: Documentation (Week 5)

### Documentation Tasks

- [x] **Create man page** (HIGH) ✅ COMPLETE
  - [x] Write comprehensive man page
  - [x] Document all flags and options
  - [x] Add examples section
  - [x] Install to system man path
  - [x] Add install/uninstall Makefile targets
  - **Time spent**: 1 hour
  - **Format**: troff/groff
  - **Result**: Complete man page with all features documented

- [ ] **Expand README** (MEDIUM)
  - Add more usage examples
  - Add troubleshooting section
  - Add FAQ section
  - Add comparison with GNU Stow
  - **Estimated**: 2-3 hours

- [ ] **Create CONTRIBUTING.md** (LOW)
  - Coding standards
  - How to submit PRs
  - Development setup
  - **Estimated**: 1-2 hours

- [ ] **Add code documentation** (MEDIUM)
  - Document all exported functions
  - Add docstrings to internal functions
  - Create API documentation
  - **Estimated**: 3-4 hours

**Total Estimated Time**: 10-15 hours (1 week part-time)

---

## Priority 4: Advanced Features (Week 6-8)

### Low Priority Features

- [ ] **Implement `--defer=REGEX` flag** (LOW)
  - Defer certain patterns during stowing
  - Useful for complex package management
  - **Estimated**: 2-3 hours
  - **Complexity**: Low
  - **User Impact**: Low (niche use case)

- [ ] **Implement `--override=REGEX` flag** (LOW)
  - Override ignore patterns
  - Allow forcing certain files
  - **Estimated**: 2-3 hours
  - **Complexity**: Low
  - **User Impact**: Low (niche use case)

- [ ] **Add `--paranoid` mode** (LOW)
  - Extra safety checks
  - Confirm every operation
  - Useful for critical systems
  - **Estimated**: 2-3 hours
  - **Complexity**: Low
  - **User Impact**: Low-Medium

- [ ] **Add `--conflicts` flag** (MEDIUM)
  - List all conflicts without taking action
  - Useful for debugging
  - **Estimated**: 2 hours
  - **Complexity**: Low
  - **User Impact**: Medium

**Total Estimated Time**: 8-11 hours (1-2 weeks part-time)

---

## Priority 5: Polish & Optimization (Week 9-10)

### Quality Improvements

- [ ] **Performance optimization** (LOW)
  - Profile code for bottlenecks
  - Optimize directory traversal
  - Cache file system queries
  - **Estimated**: 4-6 hours

- [ ] **Better progress indicators** (LOW)
  - Add progress bar for large operations
  - Show current operation status
  - **Estimated**: 2-3 hours

- [ ] **Add shell completion** (LOW)
  - Bash completion script
  - Zsh completion script
  - Fish completion script
  - **Estimated**: 3-4 hours

- [ ] **Package for distributions** (MEDIUM)
  - Create Debian package
  - Create RPM package
  - Create AUR package (Arch)
  - Submit to Quicklisp
  - **Estimated**: 6-8 hours

**Total Estimated Time**: 15-21 hours (2 weeks part-time)

---

## Priority 6: Future Enhancements (Beyond Week 10)

### Nice-to-Have Features

- [ ] **Backup mode**
  - Create backups before operations
  - Restore from backups
  - **Estimated**: 4-6 hours

- [ ] **Undo/Redo functionality**
  - Track all operations
  - Undo last operation
  - Redo undone operations
  - **Estimated**: 6-8 hours

- [ ] **Interactive mode**
  - Prompt for conflicts
  - Show diffs before adopting
  - **Estimated**: 4-6 hours

- [ ] **Configuration file support**
  - `~/.stashrc` or `~/.config/stash/config`
  - Default options
  - Package-specific settings
  - **Estimated**: 3-4 hours

- [ ] **Hooks system**
  - Pre-stash hooks
  - Post-stash hooks
  - Pre-unstash hooks
  - Post-unstash hooks
  - **Estimated**: 4-6 hours

- [ ] **Info manual**
  - Comprehensive Texinfo documentation
  - Install to system info path
  - **Estimated**: 8-10 hours

**Total Estimated Time**: 29-40 hours (4-5 weeks part-time)

---

## Bug Fixes & Known Issues

### Current Known Issues
- [x] ~~Bug #7: Verbosity mode crashes with `-vv`~~ ✅ FIXED
- [ ] Need to verify symlink handling on edge cases
- [ ] Test behavior with permission errors
- [ ] Test behavior with read-only filesystems

### Reported Issues
*(None yet - project just launched)*

---

## Timeline Summary

### Short Term (Weeks 1-5) - **RECOMMENDED FOCUS**
1. **Week 1-2**: Complete remaining testing (conflict handling, ignore patterns, etc.)
   - Status: ~40% complete (core operations already tested ✅)
2. **Week 3-4**: Implement `--adopt` and `--ignore=REGEX`
3. **Week 5**: Create man page and improve documentation

**Total**: ~31-45 hours (5 weeks part-time)  
**Outcome**: Production-ready with all essential features

### Medium Term (Weeks 6-10)
4. **Week 6-8**: Add advanced features (`--defer`, `--override`, etc.)
5. **Week 9-10**: Polish, optimize, and package

**Total**: ~23-32 hours (5 weeks part-time)  
**Outcome**: Feature-complete with excellent UX

### Long Term (Beyond Week 10)
6. **Ongoing**: Future enhancements, community features, maintenance

**Total**: ~29-40 hours (4-5 weeks part-time)  
**Outcome**: Advanced features and ecosystem integration

---

## Recommended Implementation Order

### Phase 1: Stabilization (CRITICAL)
1. ✅ Complete automated test suite
2. ✅ Test all existing features thoroughly
3. ✅ Fix any bugs discovered
4. ✅ Improve error messages

**Why First**: Ensures current features are rock-solid before adding more.

### Phase 2: Essential Features (HIGH VALUE)
1. ✅ Implement `--adopt` flag
2. ✅ Create man page
3. ✅ Implement `--ignore=REGEX`

**Why Second**: These are the most requested features from GNU Stow users.

### Phase 3: Polish (MEDIUM VALUE)
1. ✅ Add shell completion
2. ✅ Package for distributions
3. ✅ Expand documentation

**Why Third**: Makes the tool easier to use and distribute.

### Phase 4: Advanced Features (NICE TO HAVE)
1. ✅ Implement `--defer` and `--override`
2. ✅ Add interactive mode
3. ✅ Add hooks system

**Why Last**: These are niche features that most users won't need.

---

## Success Metrics

### Version 0.2.0 Goals (End of Phase 1)
- [ ] 100% test coverage of existing features
- [ ] Zero known bugs
- [ ] All core features tested and documented

### Version 0.3.0 Goals (End of Phase 2)
- [ ] `--adopt` flag implemented and tested
- [ ] Man page complete
- [ ] `--ignore=REGEX` implemented

### Version 1.0.0 Goals (End of Phase 3)
- [ ] Feature parity with GNU Stow (all essential features)
- [ ] Comprehensive documentation
- [ ] Available in package managers
- [ ] Active user community

---

## Contributing

Want to help? Pick an item from Priority 1 or 2 and submit a PR!

**Good First Issues**:
- Improve error messages
- Add shell completion scripts
- Expand README examples
- Test ignore patterns

**Medium Difficulty**:
- Implement `--ignore=REGEX`
- Create man page
- Add automated tests

**Advanced**:
- Implement `--adopt` flag
- Add hooks system
- Performance optimization

---

## Notes

- All time estimates assume part-time development (2-3 hours per session)
- Estimates include testing and documentation time
- Priority levels may change based on user feedback
- Community contributions can accelerate timeline significantly

**Last Updated**: December 4, 2025
