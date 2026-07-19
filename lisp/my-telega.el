;; -*- lexical-binding: t -*-

;; used by telega for telega-use-tracking-for; provides C-c C-SPC
;; (tracking-next-buffer) once tracking-mode is enabled by telega
(use-package tracking)

(use-package telega
  :straight (telega :type git :host github :repo "zevlg/telega.el")
  :bind (("C-c i /" . telega)
         ("C-c i m" . telega-chat-with))
  :commands (telega)
  :defer t
  :init
  (setf telega-chat-input-markups '("markdown2" nil "org")
        ;; list chats with new messages in the mode line (tracking.el),
        ;; cycle through them with C-c C-SPC
        telega-use-tracking-for '(or unmuted mentions))
  :config
  ;; unread/mention counts in the mode line
  (telega-mode-line-mode 1))

(provide 'my-telega)
