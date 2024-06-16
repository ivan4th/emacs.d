;; -*- lexical-binding: t -*-

(require 'dabbrev)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(setq-default show-trailing-whitespace t)
(setq dabbrev-case-replace nil
      dabbrev-case-fold-search nil
      show-paren-style 'expression
      make-backup-files t
      version-control t
      backup-directory-alist (quote ((".*" . "~/.emacs_backups/")))
      delete-old-versions t
      ;; do not load outdated .elc files
      load-prefer-newer t)

(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(provide 'my-base)
