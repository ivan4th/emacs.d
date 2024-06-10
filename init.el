;; -*- lexical-binding: t -*-

(setq straight-use-package-by-default t
      ;; straight-base-dir (concat user-emacs-directory "straight")
      read-process-output-max (* 4 1024 1024)
      process-adaptive-read-buffering nil
      custom-file (format "%scustom/%s.el" user-emacs-directory (system-name)))

(defconst *is-a-mac* (eq 'darwin system-type))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'my-base)
(require 'my-tools)
(require 'my-dired)
(require 'my-flycheck)
(require 'my-lisp)
(require 'my-completion)
(require 'my-appearance)
(require 'my-auth-source)
(require 'my-goto-last-change)
(require 'my-projectile)
(require 'my-win-switch)
(require 'my-git)
(require 'my-org)
(require 'my-go)
(require 'my-multiple-cursors)
(require 'my-isearch)
(require 'my-makefile)
(require 'my-shellscript)
(require 'my-kubernetes)
(require 'my-vterm)
(require 'my-yaml)
(require 'my-chatgpt)

(defcustom i4-slime-dir "" "SLIME directory"
  :type 'string
  :group 'i4-config)

;; TODO: for now SLIME has special handling
(when (and (not (string= i4-slime-dir ""))
	   (file-exists-p i4-slime-dir))
  (require 'my-slime))

;; FIXME: this should not be needed
(load custom-file)

(let ((site-config (format "%ssite-%s.el" user-emacs-directory (system-name))))
  (if (file-exists-p site-config)
      (load site-config)
    (warn "site config not found: %s" site-config)))

;; TBD: yasnippet or other snippets
;; TBD: ediff
;; TBD: jsonnet
