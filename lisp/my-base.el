;; -*- lexical-binding: t -*-

(require 'dabbrev)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(setq-default show-trailing-whitespace t)
(setq dabbrev-case-replace nil
      dabbrev-case-fold-search nil
      show-paren-style 'expression
      make-backup-files t
      version-control t
      backup-directory-alist (quote ((".*" . "~/.emacs_backups/")))
      delete-old-versions t
      ;; do not load outdated .elc files
      load-prefer-newer t
      confirm-kill-emacs 'yes-or-no-p)

(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(global-set-key (kbd "M-o") 'overwrite-mode)

(defun my/replace-buffer-with-clipboard ()
  "Replace the current buffer content with the clipboard/kill-ring content."
  (interactive)
  (delete-region (point-min) (point-max))
  (yank))

;; Bind it to a key (example: F8)
(global-set-key "\C-ciy" 'my/replace-buffer-with-clipboard)

(defun i4-yank-downcase ()
  "Yank from kill ring and downcase the yanked text."
  (interactive)
  (let ((start (point)))
    (yank)
    (downcase-region start (point))))

(global-set-key (kbd "C-c y") 'i4-yank-downcase)

(defun i4-kill-ring-save-invert-case (beg end)
  "Copy region to kill ring with case inverted."
  (interactive "r")
  (let ((text (buffer-substring beg end)))
    (kill-new
     (mapconcat (lambda (c)
                  (char-to-string
                   (cond ((char-equal c (upcase c)) (downcase c))
                         (t (upcase c)))))
                text ""))))

(global-set-key (kbd "C-c w") 'i4-kill-ring-save-invert-case)

(provide 'my-base)
