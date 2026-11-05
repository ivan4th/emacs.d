;; -*- lexical-binding: t -*-

(require 'my-auth-source)

(when *is-a-mac*
  (setq magit-git-executable "/opt/homebrew/bin/git"))

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

;; Fix forge-create-pullreq--read-args defaults:
;; 1. Source: also fall back to tracking branch (not just push branch)
;; 2. Target: default to repo's default branch, not the source's upstream
;;    (the upstream-following logic usually resolves back to the source branch,
;;    which is deleted from candidates, leaving no default)
(with-eval-after-load 'forge-commands
  (defun my/forge-create-pullreq--read-args ()
    (let* ((source (magit-completing-read
                    "Source branch"
                    (magit-list-remote-branch-names)
                    nil t nil 'magit-revision-history
                    (or (and-let* ((d (magit-branch-at-point)))
                          (if (magit-remote-branch-p d)
                              d
                            (or (magit-get-push-branch d)
                                (magit-get-upstream-branch d))))
                        (and-let* ((d (magit-get-current-branch)))
                          (if (magit-remote-branch-p d)
                              d
                            (or (magit-get-push-branch d)
                                (magit-get-upstream-branch d)))))))
           (repo    (forge-get-repository :tracked))
           (remote  (oref repo remote))
           (targets (delete source (magit-list-remote-branch-names remote)))
           (target  (magit-completing-read
                     "Target branch" targets nil t nil 'magit-revision-history
                     (car (member (concat remote "/"
                                          (or (oref repo default-branch) "master"))
                                  targets)))))
      (list source target)))
  (advice-add 'forge-create-pullreq--read-args :override
              #'my/forge-create-pullreq--read-args))

;; GitLab configuration for gitlab.k.sctech.ru
(defcustom i4-gitlab-sctech-handle "akadmin"
  "GitLab handle to use for gitlab.k.sctech.ru"
  :type 'string
  :group 'i4-config)

(defcustom i4-1pw-gitlab-sctech-api-token-ref "op://Personal/gitlab.k.sctech.ru auth token/credential"
  "1password reference for the gitlab.k.sctech.ru API token"
  :type 'string
  :group 'i4-config)

;; Register the self-hosted GitLab in forge-alist
(with-eval-after-load 'forge
  (add-to-list 'forge-alist
               '("gitlab.k.sctech.ru"
                 "gitlab.k.sctech.ru/api/v4"
                 "gitlab.k.sctech.ru"
                 forge-gitlab-repository)))

;; Auth mapping for forge access to gitlab.k.sctech.ru
(define-auth-source-1pw-mapping
 "gitlab.k.sctech.ru/api/v4"
 #'(lambda () (concat i4-gitlab-sctech-handle "^forge"))
 'i4-1pw-gitlab-sctech-api-token-ref)

;; GitLab configuration for gitlab.netgate.com
(defcustom i4-gitlab-netgate-handle "ishvedunov"
  "GitLab handle to use for gitlab.netgate.com"
  :type 'string
  :group 'i4-config)

(defcustom i4-1pw-gitlab-netgate-api-token-ref "op://Personal/gitlab.netgate.com token/credential"
  "1password reference for the gitlab.netgate.com API token"
  :type 'string
  :group 'i4-config)

;; Register gitlab.netgate.com in forge-alist
(with-eval-after-load 'forge
  (add-to-list 'forge-alist
               '("gitlab.netgate.com"
                 "gitlab.netgate.com/api/v4"
                 "gitlab.netgate.com"
                 forge-gitlab-repository)))

;; Auth mapping for forge access to gitlab.netgate.com
(define-auth-source-1pw-mapping
 "gitlab.netgate.com/api/v4"
 #'(lambda () (concat i4-gitlab-netgate-handle "^forge"))
 'i4-1pw-gitlab-netgate-api-token-ref)

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

(defun my-filename-wo-tramp (path)
  (if (tramp-tramp-file-p path)
      (let ((localname (tramp-file-name-localname
                        (tramp-dissect-file-name path))))
        localname)
    path))

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
                    (my-filename-wo-tramp root)
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
