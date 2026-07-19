;; -*- lexical-binding: t -*-

(use-package elmacro
  :straight (elmacro :type git :host github :repo "Silex/elmacro")
  :init (elmacro-mode 1))

(provide 'my-elmacro)
