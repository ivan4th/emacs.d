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
  (telega-mode-line-mode 1)
  ;; clicking internal pseudo-messages (unread bar, date breaks) sends
  ;; openMessageContent with :id -1 => "Telega error 400: Message not found"
  (advice-add 'telega-msg-open-content :around
              (lambda (orig msg &optional clicked-p)
                (unless (telega-msg-internal-p msg)
                  (funcall orig msg clicked-p))))
  ;; TDLib master (since 2026-06-05, > v1.8.64/5c77c4692 that telega is
  ;; tested against) sends draftMessage with :content
  ;; (draftMessageContentText) instead of :input_message_text; telega then
  ;; crashes rendering such chats in the root buffer with
  ;; (wrong-type-argument stringp nil) from (intern nil).  Back-fill the
  ;; old field until telega catches up.  NOTE: saving drafts from Emacs
  ;; may still misbehave, this only fixes the reading direction.
  (defun my/telega-draft-compat (chat-or-topic &rest _)
    (when-let* ((draft (plist-get chat-or-topic :draft_message))
                (content (plist-get draft :content)))
      (unless (plist-get draft :input_message_text)
        (plist-put draft :input_message_text
                   (list :@type "inputMessageText"
                         :text (or (plist-get content :text)
                                   '(:@type "formattedText" :text ""))))))
    chat-or-topic)
  (advice-add 'telega-ins--chat-status :before
              (lambda (chat &optional topic)
                (my/telega-draft-compat chat)
                (when topic (my/telega-draft-compat topic))))
  (defun my/telega-draft-compat-chatbuf (&rest _)
    (when telega-chatbuf--chat
      (my/telega-draft-compat telega-chatbuf--chat)))
  (advice-add 'telega-chatbuf--input-draft-update :before
              #'my/telega-draft-compat-chatbuf))

(provide 'my-telega)
