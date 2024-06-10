;; -*- lexical-binding: t -*-

(use-package win-switch
  :config
  (win-switch-setup-keys-ijkl "\C-xo")
  (set-default 'win-switch-idle-time 3))

(provide 'my-win-switch)
