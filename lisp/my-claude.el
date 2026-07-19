;; -*- lexical-binding: t -*-

;; install required inheritenv dependency:
(use-package inheritenv
  :straight (:type git :host github :repo "purcell/inheritenv"))

;; for eat terminal backend:
;; (use-package eat
;;   :straight (:type git
;;                    :host codeberg
;;                    :repo "akib/emacs-eat"
;;                    :files ("*.el" ("term" "term/*.el") "*.texi"
;;                            "*.ti" ("terminfo/e" "terminfo/e/*")
;;                            ("terminfo/65" "terminfo/65/*")
;;                            ("integration" "integration/*")
;;                            (:exclude ".dir-locals.el" "*-tests.el"))))

;; for vterm terminal backend:
;; (use-package vterm :straight t)

;; install claude-code.el, using :depth 1 to reduce download size:
(use-package claude-code
  :straight (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main" :depth 1
                   :files ("*.el" (:exclude "images/*")))
  :bind-keymap
  ("C-c c" . claude-code-command-map) ;; or your preferred key
  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode))
  :config
  (setq claude-code-terminal-backend 'vterm)
  ;; optional IDE integration with Monet
  ;; (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  ;; (monet-mode 1)

  (claude-code-mode)

  ;; claude-code.el calls (buffer-face-set :inherit 'claude-code-repl-face)
  ;; which remaps the default face. This interacts badly with vterm's
  ;; :inverse-video rendering, producing pink backgrounds instead of the
  ;; intended colors. Undo the remapping after each buffer is created.
  (add-hook 'claude-code-start-hook
            (lambda () (buffer-face-mode -1))))

(provide 'my-claude)
