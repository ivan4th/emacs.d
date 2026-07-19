;; -*- lexical-binding: t -*-

(when *is-a-mac*
  (setq treesit-extra-load-path '("/opt/homebrew/lib")))

(add-to-list 'auto-mode-alist
             '("\\.ya?ml$" . yaml-ts-mode))


;; knu/yaml-imenu.el sadly works only with yaml-mode,
;; but it's useful with large values.yaml files
;; Need to enable which-function-mode
(use-package yaml-imenu
  :straight (yaml-imenu :type git :host github :repo "knu/yaml-imenu.el"))

(provide 'my-yaml)
