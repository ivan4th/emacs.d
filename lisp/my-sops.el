;; -*- lexical-binding: t -*-

;; (defun sops-setup-env ()
;;   "Set environment variable for SOPS"
;;   (when (string-match "arn:aws:kms.*:\\([[:digit:]]+\\):" (buffer-string))
;;     (pcase (match-string-no-properties 1 (buffer-string))
;;       ("111111111111" (setenv "AWS_PROFILE" "dev"))
;;       ("222222222222" (setenv "AWS_PROFILE" "stage"))
;;       ("333333333333" (setenv "AWS_PROFILE" "prod"))
;;       (x (message "No matching AWS Profile for %s" x)))))

(use-package sops
  :straight (sops :type git :host github :repo "djgoku/sops")
  :bind (("C-c C-c" . sops-save-file)
         ("C-c C-k" . sops-cancel)
         ("C-c C-d" . sops-edit-file))
  :init
  ;; (setq sops-before-encrypt-decrypt-hook 'sops-setup-env)
  (global-sops-mode 1))

(provide 'my-sops)

;; FIXME: appears to be broken?
