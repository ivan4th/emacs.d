;; -*- lexical-binding: t -*-

(use-package kubernetes
  :commands (kubernetes-overview)
  :init
  (set-default 'kubernetes-poll-frequency 3600)
  (set-default 'kubernetes-redraw-frequency 3600))

(provide 'my-kubernetes)
