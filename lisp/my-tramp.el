;; -*- lexical-binding: t -*-

;; (setq tramp-ssh-controlmaster-options "")

(setf tramp-auto-save-directory "~/.tramp-autosave"
      remote-file-name-inhibit-cache nil
      vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp)
      tramp-verbose 1)

;; (setq tramp-verbose 6)

(provide 'my-tramp)
