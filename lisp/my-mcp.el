;; -*- lexical-binding: t -*-

(use-package mcp-server
  :straight (:type git :host github :repo "rhblind/emacs-mcp-server"
             :files ("*.el" "tools/*.el" "mcp-wrapper.py" "mcp-wrapper.sh"))
  :custom
  ;; Allow reloading modules over MCP; the blocklist is an anti-footgun
  ;; measure, not a boundary (setq is not on it), so keeping require and
  ;; load out of it buys nothing.
  (mcp-server-security-allowed-dangerous-functions '(require load))
  :config
  (add-hook 'emacs-startup-hook #'mcp-server-start-unix))

(provide 'my-mcp)
