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
- `i4-slack-op-item` - 1Password item holding the Slack credentials
- `i4-slack-team-name` - Slack team registered by `i4-slack-connect`
- `i4-slack-client-url` - Slack web client used by the renewal flow

## Useful Utility Functions

Buffer management (defined in `my-tools.el`):
- `killemall REGEXP` - Kill all buffers with paths matching regexp
- `revemall REGEXP` - Revert all buffers with paths matching regexp
- `savemall REGEXP` - Save all buffers with paths matching regexp

Go stack trace analysis (defined in `my-go.el`):
- `rmstacks SUBSTRING` - Remove goroutine stacks containing substring
- `keepstacks SUBSTRING` - Keep only goroutine stacks containing substring
- `sortstacks` - Sort goroutine stacks by age

## Renewing Slack Credentials

`lisp/my-slack.el` reads the Slack token and cookie from 1Password at
connect time; they are never stored in the repo. The item is
`op://Personal/emacs-slack-netgate` with concealed `token` and `cookie`
fields.

`xoxc-` tokens are browser session tokens and expire on their own, after
which everything fails with `invalid_auth`. Check without involving
Emacs (prints no secrets):

```bash
curl -s https://slack.com/api/auth.test \
  -H "Authorization: Bearer $(op read -n 'op://Personal/emacs-slack-netgate/token')" \
  -H "Cookie: d=$(op read -n 'op://Personal/emacs-slack-netgate/cookie')" \
  | jq '{ok, error, team, user}'
```

To renew: `C-c S R` (`i4-slack-refresh-credentials`) opens the client and
displays these steps; `C-c S P` (`i4-slack-store-credentials`) stores the
result and reconnects.

1. **Token** — in the devtools console of a signed-in Slack tab, run the
   snippet `C-c S R` leaves at the head of the kill ring
   (`my/slack-token-snippet`). It copies the token to the clipboard
   without displaying it. Chrome ignores pasted input until `allow
   pasting` is typed once.
2. **Cookie** — Network tab, reload the page, click any request to the
   Slack host, and copy the entire value of the `cookie:` request header
   (use the Raw toggle if it is truncated).
3. `C-c S P`, token at the first prompt, the whole cookie header at the
   second. Both are read with `read-passwd`, written to 1Password via
   `op item edit`, and the team is re-registered immediately.

Things that have gone wrong before, all still true:

- This workspace's client is `netgate.slack.com`, not `app.slack.com`.
  `my.slack.com/customize`, which upstream's `slack-refresh-token`
  instructions send you to, is restricted to full members here.
- The cookie has to be `<d value>; d-s=...; lc=...` with the `d` value
  bare and first, because `slack-team-d-cookie` takes everything before
  the first semicolon as its value. `my/slack-normalize-cookie` builds
  this from a raw header; never hand-assemble it.
- The `d` value must stay percent-encoded. Devtools' parsed cookie view
  and a HAR's `.request.cookies[]` both show it URL-decoded, which does
  not authenticate. Only the raw `Cookie:` header preserves the escapes.
- When taking credentials from a HAR, read `.request.headers[]` (name
  `cookie`), not `.request.cookies[]`, and delete the HAR afterwards — it
  contains a live session.
- Never echo these values. Pass them through shell variables or straight
  into `op`; `my/slack-redact` scrubs anything `xox*` out of error text.

This is an Enterprise Grid workspace. If `auth.test` succeeds but some
endpoints return 401, also grab `TS.boot_data.enterprise_api_token` and
pass it to `slack-register-team` as `:enterprise-token`; Grid orgs may
additionally require matching the browser's user agent through
`slack-request-curl-options`.

## External Dependencies

Some features require external tools:
- Go: gopls, goimports
- Common Lisp: SBCL, quicklisp
- Jsonnet: jsonnetfmt
- Authentication: 1Password CLI (op)
