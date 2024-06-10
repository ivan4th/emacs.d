;; -*- lexical-binding: t -*-

(require 'my-auth-source)

(defcustom i4-github-handle ""
  "GitHub handle to use"
  :type 'string
  :group 'i4-config)

(defcustom i4-1pw-github-api-token-ref ""
  "1password reference for the GitHub API token"
  :type 'string
  :group 'i4-config)

(define-auth-source-1pw-mapping
 "api.github.com"
 #'(lambda () (concat i4-github-handle "^ghub"))
 'i4-1pw-github-api-token-ref)

(define-auth-source-1pw-mapping
 "api.github.com"
 #'(lambda () (concat i4-github-handle "^forge"))
 'i4-1pw-github-api-token-ref)

(use-package magit
  :defines
  magit-stage-all-confirm
  magit-unstage-all-confirm
  :config
  (setq magit-stage-all-confirm nil
	magit-unstage-all-confirm nil)
  (defalias 'ms 'magit-status))

(use-package forge
  :after (magit))

(use-package git-link
  :bind (("\C-cgl" . git-link))
  :config
  (setq git-link-default-remote "origin"
	git-link-default-branch nil
	git-link-open-in-browser t
	git-link-use-commit t
	git-link-use-single-line-number t))

(use-package git-gutter
  :delight
  :commands (global-git-gutter-mode)
  :config
  (setq git-gutter:diff-option "--patience")
  (global-git-gutter-mode t))

;; avoid questions about git-controlled source files
(setq vc-follow-symlinks t)

(provide 'my-git)
