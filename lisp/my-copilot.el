(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :bind (:map copilot-mode-map
	      ([C-return] . copilot-accept-completion)))

(provide 'my-copilot)
