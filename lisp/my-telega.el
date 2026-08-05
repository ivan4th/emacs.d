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
              #'my/telega-draft-compat-chatbuf)
  ;; Same TDLib-newer-than-telega problem, outgoing direction: TDLib
  ;; master (2026-06-08) moved media InputFile + metadata fields of
  ;; inputMessage* into wrapper objects (inputDocument, inputPhoto, ...),
  ;; so sending attachments failed with "InputFile is not specified" and
  ;; the typed message was dropped.  Rewrite requests to the new schema
  ;; on their way to telega-server.  Remove once telega catches up with
  ;; TDLib (tested against v1.8.64/5c77c4692 as of fc6a4d1).
  (defconst my/telega--imc-compat-alist
    ;; imc @type, wrapper @type, file key, metadata keys moved inside
    '(("inputMessageDocument" "inputDocument" :document
       (:thumbnail :disable_content_type_detection))
      ("inputMessagePhoto" "inputPhoto" :photo
       (:thumbnail :added_sticker_file_ids :width :height))
      ("inputMessageVideo" "inputVideo" :video
       (:thumbnail :cover :start_timestamp :added_sticker_file_ids
        :duration :width :height :supports_streaming))
      ("inputMessageAudio" "inputAudio" :audio
       (:album_cover_thumbnail :duration :title :performer))
      ("inputMessageAnimation" "inputAnimation" :animation
       (:thumbnail :added_sticker_file_ids :duration :width :height))
      ("inputMessageVideoNote" "inputVideoNote" :video_note
       (:thumbnail :duration :length))
      ("inputMessageVoiceNote" "inputVoiceNote" :voice_note
       (:duration :waveform))))
  (defun my/telega-imc-compat (imc)
    (let* ((spec (assoc (plist-get imc :@type) my/telega--imc-compat-alist))
           (file-key (nth 2 spec))
           (file (and spec (plist-get imc file-key))))
      (if (or (null file)
              ;; already new-style
              (equal (plist-get file :@type) (nth 1 spec)))
          imc
        (let ((wrapper (list :@type (nth 1 spec) file-key file))
              (moved-keys (cons file-key (nth 3 spec)))
              (rest nil))
          (dolist (k (nth 3 spec))
            (when-let* ((v (plist-get imc k)))
              (setq wrapper (nconc wrapper (list k v)))))
          (cl-loop for (k v) on imc by #'cddr
                   unless (memq k moved-keys)
                   do (setq rest (nconc rest (list k v))))
          (nconc rest (list file-key wrapper))))))
  (defun my/telega-draft-compat-out (draft)
    (let ((imt (plist-get draft :input_message_text)))
      (if (not (equal (plist-get imt :@type) "inputMessageText"))
          draft
        (let ((rest (cl-loop for (k v) on draft by #'cddr
                             unless (eq k :input_message_text)
                             nconc (list k v))))
          (nconc rest
                 (list :content
                       (nconc (list :@type "draftMessageContentText"
                                    :text (plist-get imt :text))
                              (when-let* ((lpo (plist-get imt :link_preview_options)))
                                (list :link_preview_options lpo)))))))))
  (defun my/telega-server-send-compat (args)
    (let ((sexp (car args)))
      (when (listp sexp)
        (pcase (plist-get sexp :@type)
          ((or "sendMessage" "editMessageMedia")
           (when-let* ((imc (plist-get sexp :input_message_content)))
             (plist-put sexp :input_message_content (my/telega-imc-compat imc))))
          ("sendMessageAlbum"
           (when-let* ((imcs (plist-get sexp :input_message_contents)))
             (plist-put sexp :input_message_contents
                        (vconcat (mapcar #'my/telega-imc-compat
                                         (append imcs nil))))))
          ("setChatDraftMessage"
           (when-let* ((draft (plist-get sexp :draft_message)))
             (plist-put sexp :draft_message
                        (my/telega-draft-compat-out draft)))))))
    args)
  (advice-add 'telega-server--send :filter-args
              #'my/telega-server-send-compat)
  ;; Telegram sometimes leaves a phantom unread-mentions counter on a
  ;; chat when the mentioning messages are deleted before being read
  ;; (a server-side quirk); telega then shows @N in the mode line
  ;; forever, since it can only clear mentions by viewing the actual
  ;; messages.  Reset such counters via readAllChatMentions.
  (defun i4-telega-clear-stale-mentions ()
    "Clear unread-mention counters of chats whose mention messages are gone."
    (interactive)
    (unless (telega-server-live-p)
      (user-error "telega is not running"))
    (let ((cleared nil))
      (maphash
       (lambda (_id chat)
         (when (> (or (plist-get chat :unread_mention_count) 0) 0)
           (let ((found (telega-server--call
                         (list :@type "searchChatMessages"
                               :chat_id (plist-get chat :id)
                               :query ""
                               :from_message_id 0
                               :offset 0
                               :limit 1
                               :filter '(:@type "searchMessagesFilterUnreadMention")))))
             (when (seq-empty-p (plist-get found :messages))
               (telega-server--send
                (list :@type "readAllChatMentions"
                      :chat_id (plist-get chat :id)))
               (push (telega-chat-title chat) cleared)))))
       telega--chats)
      (if cleared
          (message "Cleared stale mentions in: %s"
                   (mapconcat #'identity cleared ", "))
        (message "No stale mention counters found"))))
  ;; telega replaced telega-company with capf-based completion (not
  ;; enabled by default); wire it up and let company pop up right after
  ;; typing "@" (default company-minimum-prefix-length of 3 delays it)
  (defun my/telega-chat-completion-setup ()
    (telega-completions-setup-capf)
    (setq-local company-minimum-prefix-length 1))
  (add-hook 'telega-chat-mode-hook #'my/telega-chat-completion-setup))

(provide 'my-telega)
