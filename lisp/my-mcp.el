;; -*- lexical-binding: t -*-

(use-package mcp-server
  :straight (:type git :host github :repo "rhblind/emacs-mcp-server"
             :files ("*.el" "tools/*.el" "mcp-wrapper.py" "mcp-wrapper.sh"))
  :config
  (add-hook 'emacs-startup-hook #'mcp-server-start-unix))

(provide 'my-mcp)
