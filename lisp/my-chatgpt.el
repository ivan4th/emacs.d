;; -*- lexical-binding: t -*-
(require 'my-auth-source)

(defcustom i4-1pw-openai-api-token-ref ""
  "1password reference for the OpenAI API token"
  :type 'string
  :group 'i4-config)

(define-auth-source-1pw-mapping
 "api.openai.com" nil 'i4-1pw-openai-api-token-ref)

(defvar openai-base-url)

(defun openai-key-auth-source-fixme (&optional base-url)
  "Retrieve the OpenAI API key from auth-source given a BASE-URL.
If BASE-URL is not specified, it defaults to `openai-base-url'."
  (if-let ((auth-info (auth-source-search :max 1
                                          :host (url-host (url-generic-parse-url (or base-url openai-base-url)))
                                          :require '(:user :secret))))
      ;; original version has an invalid funcall
      ;; (funcall (plist-get (car auth-info) :secret))
      (plist-get (car auth-info) :secret)
    (error "OpenAI API key not found in auth-source")))

(use-package openai
  :straight (openai :type git :host github :repo "emacs-openai/openai")
  :config
  ;; TODO: openai-user
  (setq openai-key #'openai-key-auth-source-fixme))

(use-package chatgpt
  :straight (chatgpt :type git :host github :repo "emacs-openai/chatgpt")
  :bind (("C-c i a" . chatgpt))
  :after (openai)
  :config
  (setq chatgpt-window-prompt ""))

(provide 'my-chatgpt)
