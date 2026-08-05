;; -*- lexical-binding: t -*-
(use-package ts)
(use-package websocket)
(use-package circe)
(use-package emojify)
(use-package request)
(use-package alert)
(use-package oauth2)

(defcustom i4-slack-op-item "op://Personal/emacs-slack-netgate"
  "1Password item reference holding the Slack token and cookie.
Must be of the form op://VAULT/ITEM; the item is expected to have
concealed fields named token and cookie."
  :group 'i4-config
  :type 'string)

(defcustom i4-slack-team-name "Netgate"
  "Name of the Slack team registered by `i4-slack-connect'."
  :group 'i4-config
  :type 'string)

(defun my/slack-op-parts ()
  "Return (VAULT . ITEM) parsed from `i4-slack-op-item'."
  (if (string-match "\\`op://\\([^/]+\\)/\\([^/]+\\)\\'" i4-slack-op-item)
      (cons (match-string 1 i4-slack-op-item)
            (match-string 2 i4-slack-op-item))
    (error "`i4-slack-op-item' must look like op://VAULT/ITEM: %s"
           i4-slack-op-item)))

(defun my/slack-redact (string)
  "Remove anything that looks like a Slack secret from STRING."
  (replace-regexp-in-string "xox[a-z]-[^ \t\n\"]*" "xox<redacted>" string))

(defun my/slack-op-credentials ()
  "Return (TOKEN . COOKIE) read from `i4-slack-op-item' via the op CLI.
Both fields are fetched in a single op invocation so that a locked
1Password session asks to unlock at most once."
  (let ((parts (my/slack-op-parts)))
    (with-temp-buffer
      (let ((status (call-process "op" nil t nil
                                  "item" "get" (cdr parts)
                                  "--vault" (car parts)
                                  "--format" "json"
                                  "--fields" "label=token,label=cookie"
                                  "--reveal")))
        (unless (eq status 0)
          (error "op item get failed: %s"
                 (my/slack-redact (string-trim (buffer-string)))))
        (goto-char (point-min))
        (let* ((parsed (json-parse-buffer :object-type 'alist :array-type 'list))
               ;; op returns a bare object when only one field comes back
               (fields (if (alist-get 'label parsed) (list parsed) parsed))
               (field (lambda (label)
                        (or (alist-get 'value
                                       (seq-find (lambda (f)
                                                   (equal (alist-get 'label f) label))
                                                 fields))
                            (error "No %s field in 1Password item %s"
                                   label (cdr parts))))))
          (cons (funcall field "token") (funcall field "cookie")))))))

(defun my/slack-forget-team (name)
  "Disconnect and unregister every registered team called NAME.
Teams are keyed by token, so a credential refresh would otherwise
leave the old team behind with a dead token."
  (when (boundp 'slack-teams-by-token)
    (let (stale)
      (maphash (lambda (token team)
                 (when (equal (slack-team-name team) name)
                   (push (cons token team) stale)))
               slack-teams-by-token)
      (dolist (entry stale)
        (ignore-errors (slack-team-disconnect (cdr entry)))
        (remhash (car entry) slack-teams-by-token)
        (when (eq slack-current-team (cdr entry))
          (setq slack-current-team nil))))))

(defun my/slack-register-team ()
  "Register `i4-slack-team-name' with credentials from 1Password."
  (my/slack-forget-team i4-slack-team-name)
  (let ((creds (my/slack-op-credentials)))
    (slack-register-team
     :name i4-slack-team-name
     :token (car creds)
     :cookie (cdr creds)
     :full-and-display-names t
     :default t
     ;; using slack-extra-subscribed-channels because I can change it dynamically
     :subscribed-channels nil)))

(defun i4-slack-connect ()
  "Register the Slack team from 1Password credentials and connect."
  (interactive)
  (require 'slack)
  (my/slack-register-team)
  (slack-start))

(defun my/slack-normalize-cookie (raw)
  "Normalize RAW into the cookie format emacs-slack expects.
RAW may be a whole Cookie: request header copied from the browser's
network tab; everything but d, d-s and lc is dropped.  The d value must
come first and unnamed because `slack-team-d-cookie' takes everything
before the first semicolon as its value."
  (let ((values (make-hash-table :test 'equal))
        (raw (replace-regexp-in-string "\\`[Cc]ookie:[ \t]*" "" (string-trim raw))))
    (dolist (part (split-string raw ";" t "[ \t\n]+"))
      (cond
       ;; already in emacs-slack's format: bare, unnamed d value
       ((string-prefix-p "xoxd-" part) (puthash "d" part values))
       ((string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" part)
        (puthash (string-trim (match-string 1 part))
                 (string-trim (match-string 2 part))
                 values))))
    (let ((d (gethash "d" values))
          (d-s (gethash "d-s" values))
          (lc (gethash "lc" values)))
      (unless d
        (error "No d cookie found in the pasted value"))
      (unless (and d-s lc)
        (message "Warning: no %s cookie found; websocket auth may fail"
                 (if d-s "lc" "d-s")))
      (concat d
              (when d-s (concat "; d-s=" d-s))
              (when lc (concat "; lc=" lc))))))

(defun i4-slack-store-credentials (token cookie)
  "Store Slack TOKEN and COOKIE in 1Password, then reconnect.
COOKIE may be pasted as a whole Cookie: header; see
`my/slack-normalize-cookie'.  Interactively both are read without
echoing; they are never written to disk or to the kill ring."
  (interactive
   (list (string-trim (read-passwd "Slack token (xoxc-...): "))
         (my/slack-normalize-cookie
          (read-passwd "Slack cookie (whole Cookie: header, or xoxd-...; d-s=...; lc=...): "))))
  (let ((parts (my/slack-op-parts)))
    (unwind-protect
        (with-temp-buffer
          (let ((status (call-process "op" nil t nil
                                      "item" "edit" (cdr parts)
                                      "--vault" (car parts)
                                      (concat "token[concealed]=" token)
                                      (concat "cookie[concealed]=" cookie))))
            (unless (eq status 0)
              (error "op item edit failed: %s"
                     (my/slack-redact (string-trim (buffer-string)))))))
      (clear-string token)
      (clear-string cookie)))
  (message "Slack credentials updated in 1Password")
  (i4-slack-connect))

(defun i4-slack-refresh-credentials ()
  "Open the browser flow for fresh Slack credentials.
Copies the token-grabbing snippet to the kill ring and displays
upstream's instructions; run `i4-slack-store-credentials' afterwards."
  (interactive)
  (require 'slack)
  (kill-new "window.prompt(\"your api token is: \", TS.boot_data.api_token)")
  (browse-url "https://my.slack.com/customize")
  (with-current-buffer (get-buffer-create "*slack refresh instructions*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (funcall slack-edit-refresh-token-instructions
                       slack-refresh-token-instructions)))
    (goto-char (point-min))
    (view-mode 1))
  (pop-to-buffer "*slack refresh instructions*")
  (message "Run M-x i4-slack-store-credentials once you have the token and cookie"))

(use-package emacs-slack
  :straight (emacs-slack :type git :host github :repo "emacs-slack/emacs-slack")
  :bind (("C-c S S" . i4-slack-connect)
         ("C-c S R" . i4-slack-refresh-credentials)
         ("C-c S P" . i4-slack-store-credentials)
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
  (setq slack-edit-refresh-token-instructions
        (lambda (instructions)
          (concat instructions
                  "\nThen run M-x i4-slack-store-credentials to save them in 1Password.\n")))
  ;; Registering the team needs 1Password, which may prompt; do it when
  ;; the package is actually loaded rather than at startup, and let it
  ;; fail softly so the rest of the package stays usable.
  (condition-case err
      (my/slack-register-team)
    (error (message "my-slack: could not register team: %s (use C-c S S to retry)"
                    (error-message-string err)))))

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

(provide 'my-slack)
