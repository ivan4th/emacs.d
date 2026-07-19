;; -*- lexical-binding: t -*-

(defun my-yang-mode-hook ()
  "Configuration for YANG Mode.  Add this to `yang-mode-hook'."
  (c-set-style "BSD")
  (setq indent-tabs-mode t)
  (setq c-basic-offset 4)
  (setq font-lock-maximum-decoration t)
  (font-lock-mode t))

(use-package yang-mode
  :straight (yang-mode :type git :host github :repo "mbj4668/yang-mode")
  :init
  (add-hook 'yang-mode-hook 'my-yang-mode-hook))

(provide 'my-yang)
