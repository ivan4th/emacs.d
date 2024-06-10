;; -*- lexical-binding: t -*-

(use-package go-mode
  :hook
  (go-mode . lsp)
  (go-mode . flycheck-mode)
  :init
  (add-hook 'go-mode-hook
	    #'(lambda ()
		(add-hook 'before-save-hook 'gofmt-before-save)))
  :config
  (setq fill-column 90)
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package lsp-mode
  :config
  (setq lsp-go-analyses '((loopclosure . :json-false))))

(use-package treemacs
  :config
  (setq treemacs-no-delete-other-windows nil))

(use-package lsp-ui)
(use-package lsp-treemacs)

(defun linux-go ()
  (interactive)
  (setenv "CGO_ENABLED" "0")
  (setenv "GOOS" "linux")
  (setenv "GOARCH" "amd64"))

(defcustom i4-go-spacemesh-dir "" "go-spacemesh source directory"
  :type 'string
  :group 'i4-config)

(defun sm-path (path)
  (replace-regexp-in-string "SMDIR" (expand-file-name i4-go-spacemesh-dir) path t))

(defun sm-go ()
  (interactive)
  ;; TBD: use 'make *-env'
  (setenv "CGO_CFLAGS" (sm-path "-ISMDIR/build/"))
  (setenv "CGO_LDFLAGS" (sm-path "-LSMDIR/build/ -Wl,-rpath,@loader_path -Wl,-rpath,SMDIR/build/"))
  (setenv "TEST_LOG_LEVEL" "debug"))

;; TODO: running tests (check existing options)
;; TODO: DAP (debugger)

(provide 'my-go)
