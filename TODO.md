# Stash-CL TODO List

## Project Status
**Current Version**: 0.3.0  
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

- [x] **Expand automated test suite** (HIGH) ✅ COMPLETE
  - [x] Unit tests (test-unit.lisp) - 26 tests, 35 assertions
  - [x] Integration tests (test-integration.sh) - 50 assertions
  - [x] Tests cover: config parsing, paths, file ops, task planner, CLI features
  - [x] Tests cover: stash, unstash, deploy, list, conflicts, config, completion
  - **Result**: All tests passing (26 unit + 50 integration)

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

- [x] **Expand README** (MEDIUM) ✅ COMPLETE
  - [x] Add more usage examples
  - [x] Add common workflows section
  - [x] Add troubleshooting section
  - [x] Add FAQ section (9 questions)
  - [x] Add comparison table with GNU Stow
  - **Time spent**: 1.5 hours
  - **Result**: Comprehensive README with examples and guides

- [x] **Create CONTRIBUTING.md** (LOW) ✅ COMPLETE
  - [x] Coding standards
  - [x] How to submit PRs
  - [x] Development setup
  - [x] Testing guidelines
  - [x] Good first issues
  - **Time spent**: 1 hour
  - **Result**: Complete contribution guidelines

- [x] **Add code documentation** (MEDIUM) ✅ COMPLETE
  - [x] Document all exported functions
  - [x] Add docstrings to key functions
  - [x] Existing functions already well-documented
  - **Time spent**: 30 minutes (verification)
  - **Result**: All key functions have docstrings

**Total Time Spent**: 4 hours
**Status**: ✅ ALL COMPLETE

---

## Priority 4: Advanced Features (Week 6-8)

### Low Priority Features

- [x] **Implement `--defer=REGEX` flag** (LOW) ✅ COMPLETE
  - Defer certain patterns during stowing
  - Useful for complex package management

- [x] **Implement `--override=REGEX` flag** (LOW) ✅ COMPLETE
  - Override ignore patterns
  - Allow forcing certain files

- [ ] **Add `--paranoid` mode** (LOW)
  - Extra safety checks
  - Confirm every operation
  - Useful for critical systems
  - **Estimated**: 2-3 hours
  - **Complexity**: Low
  - **User Impact**: Low-Medium

- [x] **Add `--conflicts` flag** (MEDIUM) ✅ COMPLETE
  - [x] List all conflicts without taking action
  - [x] Useful for debugging
  - [x] Reports conflicts with file paths and messages
  - **Result**: Fully functional conflict detection mode

**Total Estimated Time**: 8-11 hours (1-2 weeks part-time)

---

## Priority 5: Polish & Optimization (Week 9-10)

### Quality Improvements

- [x] **Performance optimization** (LOW) ✅ COMPLETE
  - [x] Replace process-spawning file operations with direct syscalls
  - [x] file-is-symlink-p: sb-posix:lstat instead of `test -L` process spawn
  - [x] create-symlink: sb-posix:symlink instead of `ln -s` process spawn
  - [x] delete-directory: uiop:delete-directory-tree instead of `rm -rf` process spawn
  - [x] read-symlink: sb-posix:readlink instead of `readlink` process spawn
  - [x] Consolidated duplicate read-symlink functions into file-ops.lisp
  - **Result**: Eliminates thousands of process spawns per stash operation

- [x] **Better progress indicators** (LOW) ✅ COMPLETE
  - [x] Add progress tracking with progress-start/progress-tick
  - [x] Show current operation status with percentage
  - [x] Integrated with folding verbosity levels
  - **Result**: Progress indicators show [N/Total] with percentage

- [x] **Add shell completion** (LOW) ✅ COMPLETE
  - [x] Bash completion script (--completion=bash)
  - [x] Zsh completion script (--completion=zsh)
  - [x] Fish completion script (--completion=fish)
  - [x] Package name completion from stash directory
  - **Result**: All three shells supported via --completion=SHELL flag

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

- [x] **Interactive mode** ✅ COMPLETE
  - [x] Prompt for conflicts with --interactive/-I flag
  - [x] Show diffs before adopting files
  - [x] Interactive adoption mode (adopt/skip/abort per file)
  - [x] Conflict resolution with skip/simulate/abort options
  - **Result**: Fully functional interactive mode

- [x] **Configuration file support** ✅ COMPLETE
  - [x] `~/.config/stash/config` (XDG) and `~/.stashrc` support
  - [x] Default options: dir, target, source, verbose, no-folding
  - [x] Default ignore, defer, override patterns
  - [x] CLI options override config file values
  - **Result**: Config file loaded automatically, CLI takes precedence

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
- [x] ~~**Bug #8: Restash with overlapping directories**~~ ✅ FIXED - Execute unstash tasks before planning stash tasks so filesystem state is correct.
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

### Version 0.3.0 Goals (Current Release)
- [x] Configuration file support (~/.stashrc, ~/.config/stash/config)
- [x] Shell completion (bash/zsh/fish)
- [x] --conflicts flag (list conflicts without action)
- [x] Interactive mode (prompt for conflicts, show diffs)
- [x] Better progress indicators
- [x] Expanded automated test suite (unit + integration)
- [x] All 0.2.0 features retained and tested

### Version 1.0.0 Goals
- [ ] Feature parity with GNU Stow (all essential features)
- [ ] Comprehensive documentation
- [ ] Available in package managers
- [ ] Active user community

---

## Contributing

Want to help? Pick an item from Priority 1 or 2 and submit a PR!

**Good First Issues**:
- Add more integration tests
- Improve error messages
- Expand README examples

**Medium Difficulty**:
- Add CI/CD integration (GitHub Actions)
- Create info manual (Texinfo)
- Performance optimization

**Advanced**:
- Add hooks system
- Implement backup mode
- Undo/Redo functionality

---

## Notes

- All time estimates assume part-time development (2-3 hours per session)
- Estimates include testing and documentation time
- Priority levels may change based on user feedback
- Community contributions can accelerate timeline significantly

**Last Updated**: August 2026
