;; -*- lexical-binding: t -*-

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style (if *is-a-mac* 'osx-notifier 'notifier)))

(provide 'my-alert)
