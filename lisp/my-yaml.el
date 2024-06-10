;; -*- lexical-binding: t -*-

(when *is-a-mac*
  (setq treesit-extra-load-path '("/opt/homebrew/lib")))

(add-to-list 'auto-mode-alist
             '("\\.ya?ml$" . yaml-ts-mode))

(provide 'my-yaml)
