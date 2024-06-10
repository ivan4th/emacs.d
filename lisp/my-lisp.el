;; -*- lexical-binding: t -*-

(require 'eldoc)
(require 'my-flycheck)
(require 'my-delight)
(require 'my-tools)

(defun my-indent-and-complete-symbol ()
  "Indentation helper for emacs lisp based on slime-indent-and-complete-symbol"
  (interactive)
  (let ((pos (point)))
    (lisp-indent-line)
    (when (= pos (point))
      (when (save-excursion (re-search-backward "[^() \n\t\r]+\\=" nil t))
	(completion-at-point)))))

(defun i4-switch-to-ielm ()
  (interactive)
  (let ((buffer (get-buffer "*ielm*")))
    (cond ((null buffer)
	   (ielm))
	  (t
	   ;; from slime -- slime-pop-to-buffer
	   (set-buffer buffer)
	   (let ((old-frame (selected-frame))
		 (window (display-buffer buffer)))
	     (select-window window)
	     (when (not (eq old-frame (selected-frame)))
	       (select-frame-set-input-focus (window-frame window)))
	     (goto-char (point-max)))))))

(defun mark-sexp-up ()
  (interactive)
  (backward-up-list)
  (mark-sexp))

(use-package paredit
  :delight
  :bind (:map paredit-mode-map
	      ([C-left] . nil)
	      ([C-right] . nil)
	      ("C-j" . nil)))

(use-package elisp-slime-nav
  :delight
  :hook
  (emacs-lisp-mode . elisp-slime-nav-mode))

(use-package eldoc
  :straight (:type built-in)
  :delight eldoc-mode)

(use-package emacs-lisp-mode
  :straight (:type built-in)
  :hook
  (emacs-lisp-mode . paredit-mode)
  (emacs-lisp-mode . eldoc-mode)
  (emacs-lisp-mode . flycheck-mode)
  (emacs-lisp-mode . i-dont-like-trailing-whitespace)
  :bind (:map emacs-lisp-mode-map
	      ([tab] . my-indent-and-complete-symbol)
	      ("\C-c\C-z" . i4-switch-to-ielm))
  :after (paredit delight flycheck))

;; the following applies to both lisp-mode and emacs-lisp-mode

(define-key lisp-mode-shared-map [C-left] 'backward-sexp)
(define-key lisp-mode-shared-map [C-right] 'forward-sexp)
(define-key lisp-mode-shared-map [C-up] 'backward-up-list)
(define-key lisp-mode-shared-map [C-down] 'down-list)
(define-key lisp-mode-shared-map [C-backspace] 'backward-kill-sexp)
(define-key lisp-mode-shared-map "\C-t" 'transpose-sexps)
(define-key lisp-mode-shared-map "\C-k" 'kill-sexp)
(define-key lisp-mode-shared-map [C-M-up] 'mark-sexp-up)

(provide 'my-lisp)
