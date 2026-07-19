;; -*- lexical-binding: t -*-

(use-package telega
  :straight (telega :type git :host github :repo "zevlg/telega.el")
  :bind (("C-c i /" . telega)
         ("C-c i m" . telega-chat-with))
  :commands (telega)
  :defer t
  :init
  (setf telega-chat-input-markups '("markdown2" nil "org")))

(provide 'my-telega)
