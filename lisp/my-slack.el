;; -*- lexical-binding: t -*-
(use-package ts)
(use-package websocket)
(use-package circe)
(use-package emojify)
(use-package request)
(use-package alert)
(use-package oauth2)

(defcustom i4-slack-op-item "op://Personal/emacs-slack-netgate"
  "1Password item reference holding the Slack token and cookie."
  :group 'i4-config
  :type 'string)

(defun my/slack-op-read (field)
  "Read FIELD of `i4-slack-op-item' from 1Password via the op CLI."
  (with-temp-buffer
    (let ((status (call-process "op" nil t nil "read" "-n"
                                (concat i4-slack-op-item "/" field))))
      (unless (eq status 0)
        (error "op read failed for %s: %s" field (string-trim (buffer-string))))
      (string-trim (buffer-string)))))

(use-package emacs-slack
  :straight (emacs-slack :type git :host github :repo "emacs-slack/emacs-slack")
  :bind (("C-c S S" . slack-start)
         ("C-c S K" . slack-stop)
         ("C-c S c" . slack-select-rooms)
         ("C-c S u" . slack-select-unread-rooms)
         ("C-c S U" . slack-user-select)
         ("C-c S s" . slack-search-from-messages)
         ("C-c S J" . slack-jump-to-browser)
         ("C-c S j" . slack-jump-to-app)
         ("C-c S e" . slack-insert-emoji)
         ("C-c S E" . slack-message-edit)
         ("C-c S r" . slack-message-add-reaction)
         ("C-c S t" . slack-thread-show-or-create)
         ("C-c S g" . slack-message-redisplay)
         ("C-c S G" . slack-conversations-list-update-quick)
         ("C-c S q" . slack-quote-and-reply)
         ("C-c S Q" . slack-quote-and-reply-with-link)
         (:map slack-mode-map
               (("@" . slack-message-embed-mention)
                ("#" . slack-message-embed-channel)))
         (:map slack-thread-message-buffer-mode-map
               (("C-c '" . slack-message-write-another-buffer)
                ("@" . slack-message-embed-mention)
                ("#" . slack-message-embed-channel)))
         (:map slack-message-buffer-mode-map
               (("C-c '" . slack-message-write-another-buffer)))
         (:map slack-message-compose-buffer-mode-map
               (("C-c '" . slack-message-send-from-buffer)))
         )
  :custom
  (slack-extra-subscribed-channels (mapcar 'intern (list "some-channel")))
  :config
  (slack-register-team
   :name "Netgate"
   :token (my/slack-op-read "token")
   :cookie (my/slack-op-read "cookie")
   :full-and-display-names t
   :default t
   :subscribed-channels nil ;; using slack-extra-subscribed-channels because I can change it dynamically
   ))

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

(provide 'my-slack)
