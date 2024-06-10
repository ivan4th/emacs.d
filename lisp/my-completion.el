;; -*- lexical-binding: t -*-

;; some parts from:
;; https://github.com/EndlessPeak/.emacs.d/blob/474bbbcdfc7f08037a03a06c2e69f1dd0fb4a873/config/etc/init-vertico.org#L149
(use-package vertico
  :commands
  vertico-mode
  :config
  (vertico-mode)
  (setq read-file-name-completion-ignore-case t
	read-buffer-completion-ignore-case t
	completion-ignore-case t
	vertico-count 20
	vertico-resize nil
	;; completion-styles '(basic partial-completion emacs22)
	;; completion-category-overrides '((file (styles substring)))
	;; completion-category-defaults nil
        ;; completion-category-overrides '((file (styles partial-completion)))
	))

;; needed due to :commands
(require 'vertico)

(use-package savehist
  :config
  (savehist-mode))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config
  (marginalia-mode))

;; FIXME: use-package with :config doesn't appear to work for recentf (?)
(require 'recentf)

(setf recentf-max-saved-items 500
      recentf-max-menu-items 60
      recentf-exclude '("/.*:"))
(run-at-time nil 60 'recentf-save-list)
(recentf-mode 1)

(use-package consult
  :bind (("\C-x\C-r" . consult-recent-file)
	 ("\C-cig"   . consult-git-grep))
  :after (vertico))

(use-package orderless
  :init
  ;; & is for company because space will break completion (FIXME: check)
  (setq completion-styles '(orderless partial-completion basic)
        orderless-component-separator "[ &]"
	completion-category-defaults nil
	completion-category-overrides '((file (styles basic partial-completion))
					(symbol (styles basic partial-completion emacs22)))))

(use-package company
  :commands
  company-indent-or-complete-common
  global-company-mode
  :bind (:map company-active-map ("<tab>" . company-complete-selection))
  :init
  ;; https://github.com/company-mode/company-mode/issues/94#issuecomment-365701801
  (global-set-key (kbd "TAB") #'company-indent-or-complete-common)
  (global-set-key [M-tab] 'company-complete-common)
  ;; FROM: https://github.com/mdempsky/gocode/tree/master/emacs-company
  (setq company-tooltip-limit 20 ;; bigger popup window
	;; decrease delay before autocompletion popup shows
	company-idle-delay .3
	;; remove annoying blinking
	company-echo-delay 0
	;; start autocompletion only after typing
	company-begin-commands '(self-insert-command))
  (global-company-mode))


(provide 'my-completion)
