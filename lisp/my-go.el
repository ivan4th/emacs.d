;; -*- lexical-binding: t -*-

(require 'my-copilot)

(use-package go-mode
  :hook
  (go-mode . lsp)
  (go-mode . flycheck-mode)
  ;; (go-mode . copilot-mode)
  :init
  (add-hook 'go-mode-hook
	    #'(lambda ()
		(setf fill-column 90)
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

;; FIXME: this needs some getting used to
;; (use-package gotest
;;   :bind (("C-c i t" . my-go-test)
;; 	 ("C-c i l" . go-test-current-test-cache))
;;   :init
;;   (defun my-go-test (&optional arg)
;;     (interactive "P")
;;     (if arg
;; 	(go-test-current-test)
;;       (go-test-current-file))))

(defun rmstacks (substring)
  "Remove all goroutine stack traces containing SUBSTRING from the current buffer.
Goroutine stack traces are assumed to be separated by two newlines."
  (interactive "sEnter substring to remove: ")
  (save-excursion
    ;; Read the entire buffer into a string
    (let* ((buf (buffer-substring-no-properties (point-min) (point-max)))
           ;; Split on two consecutive newlines
           (goroutine-list (split-string buf "\n\n"))
           ;; Filter out any goroutine blocks that match the substring
           (filtered (cl-remove-if (lambda (block)
                                     (string-match-p (regexp-quote substring) block))
                                   goroutine-list)))
      ;; Clear the buffer, then insert back only the goroutines that didn't match
      (erase-buffer)
      (insert (string-join filtered "\n\n")))))

(defun keepstacks (substring)
  "Remove all goroutine stack traces NOT containing SUBSTRING from the current buffer.
Goroutine stack traces are assumed to be separated by two newlines."
  (interactive "sEnter substring to remove: ")
  (save-excursion
    ;; Read the entire buffer into a string
    (let* ((buf (buffer-substring-no-properties (point-min) (point-max)))
           ;; Split on two consecutive newlines
           (goroutine-list (split-string buf "\n\n"))
           ;; Filter out any goroutine blocks that do not match the substring
           (filtered (cl-remove-if-not (lambda (block)
					 (string-match-p (regexp-quote substring) block))
                                       goroutine-list)))
      ;; Clear the buffer, then insert back only the goroutines that didn't match
      (erase-buffer)
      (insert (string-join filtered "\n\n")))))

(defun go-extract-age (str)
  "Return the age from STR in minutes as an integer, or 0 if not found/unknown.
For example, STR might look like: \"running, 10 minutes\"
or \"locked to thread, 2 hours\" etc.
You can add more patterns as needed."
  (let ((minutes 0))
    (cond
     ((string-match "\\([0-9]+\\) ?minutes?" str)
      (setq minutes (string-to-number (match-string 1 str))))
     ;; ((string-match "\\([0-9]+\\) ?hours?" str)
     ;;  (setq minutes (* 60 (string-to-number (match-string 1 str)))))
     ;; Add more patterns as needed:
     ;; (string-match "\\([0-9]+\\) ?days?" str) => multiply by (24 * 60)
     ;; (string-match "\\([0-9]+\\)m\\([0-9]+\\)s" str) => parse "10m15s" style
     )
    minutes))

(defun sortstacks ()
  (interactive)
  (save-excursion
    (let* ((buf (buffer-substring-no-properties (point-min) (point-max)))
           (goroutine-list (split-string buf "\n\n")))
      (erase-buffer)
      (insert (string-join (cl-sort goroutine-list #'< :key #'go-extract-age) "\n\n")))))

(use-package go-playground)

(require 'my-go-test)

;; TODO: running tests (check existing options)
;; TODO: DAP (debugger)
;; TODO: go-ts-mode

(provide 'my-go)
