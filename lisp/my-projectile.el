;; -*- lexical-binding: t -*-

(use-package projectile
  :delight projectile-mode
  :bind (:map projectile-mode-map
	      (("\C-cph" . projectile-find-file)
	       ("\C-cp" . projectile-command-map)))
  :init
  (setq projectile-keymap-prefix (kbd "C-c p")
	projectile-require-project-root nil)
  (projectile-mode t))

;; https://sideshowcoder.com/2017/10/24/projectile-and-tramp/
(defadvice projectile-on (around exlude-tramp activate)
  "This should disable projectile when visiting a remote file"
  (unless  (--any? (and it (file-remote-p it))
                   (list
                    (buffer-file-name)
                    list-buffers-directory
                    default-directory
                    dired-directory))
    ad-do-it))

(provide 'my-projectile)
