;; -*- lexical-binding: t -*-

(use-package dired
  :straight (:type built-in)
  :config
  (set-default 'auto-revert-verbose nil)
  (set-default 'dired-dwim-target t)
  :hook (dired-mode . auto-revert-mode)
  :bind (:map dired-mode-map
	      ("r" . wdired-change-to-wdired-mode)))

(require 'dired-x)

(provide 'my-dired)
