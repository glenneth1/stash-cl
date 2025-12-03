# GNU Stow vs Stash-CL Feature Comparison

## Overview

This document compares the feature set of GNU Stow (the reference implementation) with stash-cl (our Common Lisp rewrite).

## Core Features

| Feature | GNU Stow | Stash-CL | Status | Notes |
|---------|----------|----------|--------|-------|
| **Basic Operations** |
| Stow packages | ✅ `-S, --stow` | ✅ (default) | ✅ IMPLEMENTED | Default action |
| Unstow packages | ✅ `-D, --delete` | ✅ `-D, --delete` | ✅ IMPLEMENTED | |
| Restow packages | ✅ `-R, --restow` | ✅ `-R, --restash` | ✅ IMPLEMENTED | Renamed to "restash" |
| **Directory Management** |
| Set stow directory | ✅ `-d, --dir=DIR` | ✅ `--dir=DIR` | ✅ IMPLEMENTED | |
| Set target directory | ✅ `-t, --target=DIR` | ✅ `-t, --target=DIR` | ✅ IMPLEMENTED | |
| **Simulation & Verbosity** |
| Dry-run mode | ✅ `-n, --no, --simulate` | ✅ `-n, --simulate` | ✅ IMPLEMENTED | |
| Verbose output | ✅ `-v, --verbose[=N]` | ✅ `-v, --verbose` | ✅ IMPLEMENTED | Levels 0-3 |
| **Tree Folding** |
| Basic tree folding | ✅ Yes | ✅ Yes | ✅ IMPLEMENTED | |
| Disable folding | ✅ `--no-folding` | ✅ `--no-folding` | ✅ IMPLEMENTED | |
| Partial folding | ❌ No | ✅ Yes | ✅ ENHANCEMENT | Keeps subdirs folded when possible |
| Automatic refolding | ❌ No | ✅ Yes | ✅ ENHANCEMENT | Refolds after unstow |
| Folding statistics | ❌ No | ✅ Yes | ✅ ENHANCEMENT | Shows detailed folding report |
| **Conflict Handling** |
| Detect conflicts | ✅ Yes | ✅ Yes | ✅ IMPLEMENTED | |
| Adopt files | ✅ `--adopt` | ❌ No | ⚠️ NOT IMPLEMENTED | Planned for future |
| **Ignore Patterns** |
| Global ignore file | ✅ `.stow-global-ignore` | ✅ `.stash-global-ignore` | ✅ IMPLEMENTED | Renamed |
| Local ignore file | ✅ `.stow-local-ignore` | ✅ `.stash-local-ignore` | ✅ IMPLEMENTED | Renamed |
| Regex ignore | ✅ `--ignore=REGEX` | ❌ No | ⚠️ NOT IMPLEMENTED | Uses ignore files only |
| Defer patterns | ✅ `--defer=REGEX` | ❌ No | ⚠️ NOT IMPLEMENTED | Planned for future |
| Override patterns | ✅ `--override=REGEX` | ❌ No | ⚠️ NOT IMPLEMENTED | Planned for future |
| **Advanced Features** |
| Deploy mode | ❌ No | ✅ `-d, --deploy` | ✅ ENHANCEMENT | Stow all packages |
| Task planning | ❌ No | ✅ Yes | ✅ ENHANCEMENT | Validates before executing |
| Task summary | ❌ No | ✅ Yes | ✅ ENHANCEMENT | Shows planned operations |
| **Help & Documentation** |
| Help message | ✅ `-h, --help` | ✅ `-h, --help` | ✅ IMPLEMENTED | |
| Version info | ✅ `--version` | ✅ `--version` | ✅ IMPLEMENTED | |
| Man page | ✅ Yes | ❌ No | ⚠️ TODO | Need to create |
| Info manual | ✅ Yes | ❌ No | ⚠️ TODO | Need to create |

## Implementation Status Summary

### ✅ Fully Implemented (Core Features)
- Basic stow/unstow/restow operations
- Directory management (stow dir, target dir)
- Simulation mode (dry-run)
- Verbose output with multiple levels
- Tree folding with no-folding option
- Conflict detection
- Ignore file support (.stash-global-ignore, .stash-local-ignore)
- Help and version information

### ✅ Enhanced Beyond GNU Stow
- **Smart Partial Folding**: Keeps subdirectories folded when possible during unfolding
- **Automatic Refolding**: Refolds directories after unstowing when appropriate
- **Folding Statistics**: Detailed reports showing what was folded/unfolded
- **Task Planning System**: Validates all operations before executing
- **Task Summary**: Shows planned operations before execution
- **Deploy Mode**: Stow all packages in one command

### ⚠️ Not Yet Implemented
- `--adopt` flag (move existing files into package)
- `--ignore=REGEX` CLI flag (currently only uses ignore files)
- `--defer=REGEX` flag
- `--override=REGEX` flag
- Man page documentation
- Info manual documentation

### 🔄 Differences from GNU Stow
- **Renamed operations**: "restow" → "restash" (for consistency with "stash")
- **Renamed ignore files**: `.stow-*-ignore` → `.stash-*-ignore`
- **Different implementation**: Common Lisp vs Perl
- **Task planning**: All operations validated before execution (safer)
- **Enhanced folding**: More intelligent than GNU Stow

## Testing Status

### ✅ Tested Features
- Basic stow operation
- Tree folding
- Simulation mode
- Help/version output

### ⚠️ Needs Testing
- Unstow operation
- Restash operation
- Conflict handling
- Ignore patterns
- No-folding mode
- Multiple packages
- Deploy mode
- Verbose levels

## Priority for Future Development

### High Priority
1. ✅ Complete core stow/unstow/restow (DONE)
2. ✅ Tree folding (DONE)
3. ⚠️ Comprehensive testing suite
4. ⚠️ Bug fixes from testing

### Medium Priority
1. `--adopt` flag implementation
2. `--ignore=REGEX` CLI flag
3. Better error messages
4. Man page documentation

### Low Priority
1. `--defer` and `--override` flags
2. Info manual
3. Performance optimizations
4. Additional enhancements

## Compatibility Notes

### Breaking Changes from GNU Stow
- Ignore files renamed (`.stow-*` → `.stash-*`)
- Command renamed (`stow` → `stash`)
- Restow renamed to restash

### Migration Path
For users migrating from GNU Stow:
1. Rename `.stow-global-ignore` → `.stash-global-ignore`
2. Rename `.stow-local-ignore` → `.stash-local-ignore`
3. Replace `stow` commands with `stash`
4. Replace `-R` (restow) with `-R` (restash) - same flag, new name

## Conclusion

Stash-CL implements all core GNU Stow features plus several enhancements. The main missing features are advanced ignore patterns via CLI flags and the `--adopt` option. The enhanced folding system and task planning make stash-cl safer and more intelligent than GNU Stow in many scenarios.
