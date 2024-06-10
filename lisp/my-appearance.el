;; -*- lexical-binding: t -*-

(defvar *is-a-mac*)

(use-package zenburn-theme
  :init
  (load-theme 'zenburn t)
  (set-face-background 'error "DarkRed")
  (set-face-background 'warning "DarkBlue"))

(setq initial-frame-alist
      (append '((cursor-type . bar) (cursor-color . "#dfdfdf"))
	      (copy-alist initial-frame-alist)))

(setq default-frame-alist
      (append '((cursor-type . bar) (cursor-color . "#dfdfdf"))
	      (copy-alist default-frame-alist)))

(blink-cursor-mode t)

;; https://www.lonecpluspluscoder.com/2015/09/14/using-the-hack-2-0-font-in-emacs-on-os-x/
(when *is-a-mac*
  (set-frame-font "-*-Source Code Pro-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
  (set-face-attribute
   'mode-line nil
   :font "-*-Source Code Pro-normal-normal-normal-*-10-*-*-*-m-0-iso10646-1")
  (set-face-attribute
   'mode-line-inactive nil
   :font "-*-Source Code Pro-normal-normal-normal-*-10-*-*-*-m-0-iso10646-1"))

(provide 'my-appearance)
