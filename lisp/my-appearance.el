;; -*- lexical-binding: t -*-

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

;; initially:
;; https://www.lonecpluspluscoder.com/2015/09/14/using-the-hack-2-0-font-in-emacs-on-os-x/
;; later:
;; https://superuser.com/questions/692173/emacs-default-font-does-not-work-with-new-frames
(when *is-a-mac*
  (set-face-attribute
   'default nil
   :font "-*-Source Code Pro-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
  (set-face-attribute
   'mode-line nil
   :font "-*-Source Code Pro-normal-normal-normal-*-10-*-*-*-m-0-iso10646-1")
  (set-face-attribute
   'mode-line-inactive nil
   :font "-*-Source Code Pro-normal-normal-normal-*-10-*-*-*-m-0-iso10646-1"))

(provide 'my-appearance)
