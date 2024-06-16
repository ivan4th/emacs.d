;; -*- lexical-binding: t -*-

(require 'my-auth-source)

(defcustom i4-github-handle ""
  "GitHub handle to use"
  :type 'string
  :group 'i4-config)

(defcustom i4-1pw-github-api-token-ref ""
  "1password reference for the GitHub API token"
  :type 'string
  :group 'i4-config)

(define-auth-source-1pw-mapping
 "api.github.com"
 #'(lambda () (concat i4-github-handle "^ghub"))
 'i4-1pw-github-api-token-ref)

(define-auth-source-1pw-mapping
 "api.github.com"
 #'(lambda () (concat i4-github-handle "^forge"))
 'i4-1pw-github-api-token-ref)

(use-package magit
  :defines
  magit-stage-all-confirm
  magit-unstage-all-confirm
  :config
  (setq magit-stage-all-confirm nil
	magit-unstage-all-confirm nil)
  (defalias 'ms 'magit-status))

(use-package forge
  :after (magit))

(use-package git-link
  :bind (("\C-cgl" . git-link))
  :config
  (setq git-link-default-remote "origin"
	git-link-default-branch nil
	git-link-open-in-browser t
	git-link-use-commit t
	git-link-use-single-line-number t))

(use-package git-gutter
  :delight
  :commands (global-git-gutter-mode)
  :config
  (setq git-gutter:diff-option "--patience")
  (global-git-gutter-mode t))

;; avoid questions about git-controlled source files
(setq vc-follow-symlinks t)

;; https://gist.github.com/offby1/1240799

;; There's something similar (but fancier) in vc-git.el: vc-git-grep

;; -I means don't search through binary files

;; --no-color, oddly enough, is required to allow emacs to colorize the output

(defcustom git-grep-switches "--extended-regexp -I -n --no-color"
  "Switches to pass to `git grep'."
  :type 'string
  :group 'i4-config)

(defcustom git-grep-default-work-tree ""
  "Top of your favorite git working tree.
  \\[git-grep] will search from here if it cannot figure out where else to look."
  :type 'directory
  :group 'i4-config)

(require 'vc-git)
(require 'grep)

;; Uncomment this to try out the built-in-to-Emacs function.
;;(defalias 'git-grep 'vc-git-grep)

(defun git-grep (command-args)
  (interactive
   (let ((root (vc-git-root default-directory)))
     (when (not root)
       (setq root git-grep-default-work-tree)
       (message "git-grep: %s doesn't look like a git working tree; searching from %s instead" default-directory root))
     (list (read-shell-command
	    "Run git-grep (like this): "
            (format (concat
                     "cd %s && "
                     "git --no-pager grep %s -e %s")
                    root
                    git-grep-switches
                    (let ((thing (and
                                  ; don't snarf stuff from the
                                  ; buffer if we're not looking
                                  ; at a file.  Perhaps we
                                  ; should also check to see if
                                  ; the file is part of a git
                                  ; repo.
				  buffer-file-name
				  (thing-at-point 'symbol))))
                      (or (and thing (progn
                                       (set-text-properties 0 (length thing) nil thing)
                                       (shell-quote-argument (regexp-quote thing))))
			  "")))
            'git-grep-history))))
  (let ((grep-use-null-device nil))
    (grep command-args)))

(provide 'my-git)
