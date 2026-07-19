# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is ivan4th's personal Emacs configuration, tested with Emacs 29.2+. It uses straight.el for package management with ~95 packages organized into ~40 feature modules.

## Architecture

### Initialization Flow

1. `early-init.el` - Disables package.el, increases GC threshold for faster startup
2. `init.el` - Bootstraps straight.el, then sequentially requires all `my-*.el` modules from `lisp/`
3. `custom/HOSTNAME.el` - Per-machine customizations (symlinked, not in git)
4. `site-HOSTNAME.el` - Site-specific functions and settings (not in git)

### Module Pattern

All modules in `lisp/my-*.el` follow this structure:
```elisp
;; -*- lexical-binding: t -*-
(require 'dependencies)        ; if needed
(use-package package-name
  :hook (mode . feature)
  :bind (("C-c key" . command))
  :init (setq var val)
  :config (setup))
(defun custom-function () ...)  ; custom functions use i4- or my/ prefix
(provide 'my-module-name)
```

### Key Patterns

- **Custom variables**: Use `defcustom` with `:group 'i4-config` for user-configurable settings
- **Platform detection**: `*is-a-mac*` constant for macOS-specific behavior
- **Function prefixes**: `i4-` for interactive commands, `my/` for internal helpers
- **Package management**: `use-package` with straight.el (`:straight t` is default)
- **Keybinding convention**: Custom commands use `C-c i` prefix (e.g., `C-c i t` for tests, `C-c i b` for compile, `C-c i r` for recompile)

### Module Load Order

Modules are loaded sequentially in init.el. Dependencies must be loaded earlier:
- `my-base` and `my-tools` - Foundational settings
- `my-completion` - Vertico, company, consult
- Language modes (`my-go`, `my-lisp`, `my-slime`, etc.)
- Domain-specific (`my-org`, `my-git`, `my-kubernetes`)

Some modules have internal sub-requires (e.g., `my-go` requires `my-go-test`, `my-go` requires `my-copilot`) that are not listed in init.el.

## Adding New Modules

1. Create `lisp/my-feature.el` following the module pattern above
2. Add `(require 'my-feature)` to init.el in appropriate position
3. Use `defcustom` with `:group 'i4-config` for any user-configurable options
4. Follow the `C-c i` keybinding convention for new interactive commands

## Test Running Patterns

Both Go and Common Lisp have test-running functions with consistent keybindings:

| Keybinding | Go (`my-go-test.el`) | Common Lisp (`my-slime.el`) |
|------------|---------------------|---------------------------|
| `C-c i t` | `i4-go-test` - Run tests in current file/test at point | `i4-vtf-compile-run-test` - Compile and run test |
| `C-c i l` | `i4-go-last-test` - Re-run last test | `i4-vtf-run-last-test` - Re-run last test |

Go tests: Use prefix arg (`C-u C-c i t`) to run only the test at point vs. all tests in file.

## Key Customization Variables

Customize via `M-x customize-group RET i4-config`:
- `i4-org-dir` - Org files directory
- `i4-github-handle` - GitHub username for authentication
- `i4-go-spacemesh-dir` - Go project path
- `i4-slime-dir` - SLIME configuration path

## Useful Utility Functions

Buffer management (defined in `my-tools.el`):
- `killemall REGEXP` - Kill all buffers with paths matching regexp
- `revemall REGEXP` - Revert all buffers with paths matching regexp
- `savemall REGEXP` - Save all buffers with paths matching regexp

Go stack trace analysis (defined in `my-go.el`):
- `rmstacks SUBSTRING` - Remove goroutine stacks containing substring
- `keepstacks SUBSTRING` - Keep only goroutine stacks containing substring
- `sortstacks` - Sort goroutine stacks by age

## External Dependencies

Some features require external tools:
- Go: gopls, goimports
- Common Lisp: SBCL, quicklisp
- Jsonnet: jsonnetfmt
- Authentication: 1Password CLI (op)
