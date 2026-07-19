;; -*- lexical-binding: t -*-

(use-package vterm
  :bind (("C-c i v" . vterm))
  :init
  (set-default 'vterm-max-scrollback 100000))


(provide 'my-vterm)
