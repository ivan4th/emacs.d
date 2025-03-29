;; -*- lexical-binding: t -*-

(use-package mermaid-mode
  :straight (mermaid-mode :type git :host github :repo "abrochard/mermaid-mode")
  :init
  (set-default 'mermaid-indentation-level 2))

(provide 'my-mermaid)
