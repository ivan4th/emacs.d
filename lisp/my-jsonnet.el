;; -*- lexical-binding: t -*-

(use-package jsonnet-mode)

;; Based on: https://github.com/grafana/jsonnet-language-server/blob/main/editor/emacs/jsonnet-language-server.el

;;; jsonnet-language-server -- LSP registration for Emacs lsp-mode.
;;; Commentary:
;;; Code:
(require 'jsonnet-mode)
(require 'lsp-mode)

(defcustom lsp-jsonnet-executable "jsonnet-language-server"
  "Command to start the Jsonnet language server."
  :group 'lsp-jsonnet
  :risky t
  :type 'file)

;; Configure lsp-mode language identifiers.
(add-to-list 'lsp-language-id-configuration '(jsonnet-mode . "jsonnet"))

;; Register jsonnet-language-server with the LSP client.
(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () lsp-jsonnet-executable))
  :activation-fn (lsp-activate-on "jsonnet")
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       ;; TODO: jsonnet-language-server settings should use a prefix
                       (ht-get (lsp-configuration-section "jsonnet") "jsonnet"))))
  :server-id 'jsonnet))

;; Start the language server whenever jsonnet-mode is used.
(add-hook 'jsonnet-mode-hook #'lsp-deferred)

(require 'flycheck)

(defcustom jsonnetfmt-command "jsonnetfmt"
  "The 'jsonnetfmt' command.
Some users may replace this with 'goimports'
from https://golang.org/x/tools/cmd/goimports."
  :type 'string
  :group 'go)

(defcustom jsonnetfmt-args '("--string-style" "d")
  "Additional arguments to pass to jsonnetfmt."
  :type '(repeat string)
  :group 'go)

(defun jsonnet-fmt ()
  "Format the current buffer according to the formatting tool.

The tool used can be set via ‘jsonnetfmt-command` (default: jsonnetfmt) and additional
arguments can be set as a list via ‘jsonnetfmt-args`."
  (interactive)
  (let ((tmpfile (make-nearby-temp-file "jsonnetfmt" nil ".jsonnet"))
        (patchbuf (get-buffer-create "*Jsonnetfmt patch*"))
        (errbuf nil ;;(if jsonnetfmt-show-errors (get-buffer-create "*Jsonnetfmt Errors*"))
                )
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        our-jsonnetfmt-args)

    (unwind-protect
        (save-restriction
          (widen)
          (if errbuf
              (with-current-buffer errbuf
                (setq buffer-read-only nil)
                (erase-buffer)))
          (with-current-buffer patchbuf
            (erase-buffer))

          (write-region nil nil tmpfile)

          ;; (when (and (jsonnetfmt--is-goimports-p) buffer-file-name)
          ;;   (setq our-jsonnetfmt-args
          ;;         (append our-jsonnetfmt-args
          ;;                 ;; srcdir, despite its name, supports
          ;;                 ;; accepting a full path, and some features
          ;;                 ;; of goimports rely on knowing the full
          ;;                 ;; name.
          ;;                 (list "-srcdir" (go--file-local-name
          ;;                                  (file-truename buffer-file-name))))))
          (setq our-jsonnetfmt-args
                (append our-jsonnetfmt-args jsonnetfmt-args
                        (list "-i" (file-local-name tmpfile))))
          (message "Calling jsonnetfmt: %s %s" jsonnetfmt-command our-jsonnetfmt-args)
          ;; We're using errbuf for the mixed stdout and stderr output. This
          ;; is not an issue because jsonnetfmt -w does not produce any stdout
          ;; output in case of success.
          (if (zerop (apply #'process-file jsonnetfmt-command nil errbuf nil our-jsonnetfmt-args))
              (progn
                ;; There is no remote variant of ‘call-process-region’, but we
                ;; can invoke diff locally, and the results should be the same.
                (if (zerop (let ((local-copy (file-local-copy tmpfile)))
                             (unwind-protect
                                 (call-process-region
                                  (point-min) (point-max) "diff" nil patchbuf
                                  nil "-n" "-" (or local-copy tmpfile))
                               (when local-copy (delete-file local-copy)))))
                    (message "Buffer is already jsonnetfmted")
                  (go--apply-rcs-patch patchbuf)
                  (message "Applied jsonnetfmt"))
                (if errbuf (jsonnetfmt--kill-error-buffer errbuf)))
            (message "Could not apply jsonnetfmt")
            ;; (if errbuf (jsonnetfmt--process-errors (buffer-file-name) tmpfile errbuf))
            ))

      (kill-buffer patchbuf)
      ;; (delete-file tmpfile)
      )))

;; FIXME
(defun jsonnet-reformat-buffer ()
  "Reformat entire buffer using the Jsonnet format utility."
  (interactive)
  (when (eq 'jsonnet-mode major-mode)
    (jsonnet-fmt)))

(defun i4-jsonnet-hook ()
  (flycheck-mode 1)
  (add-hook 'before-save-hook 'jsonnet-reformat-buffer)
  ;; FIXME
  (when (string-match-p "^/Users/ivan4th/work/lea/leacluster/" (buffer-file-name))
    (flycheck-select-checker 'jsonnet-test)))

(add-hook 'jsonnet-mode-hook #'i4-jsonnet-hook)

(flycheck-define-checker jsonnet-test
  "A Jsonnet syntax checker using the jsonnet binary.

See URL `https://jsonnet.org'."
  :command ("/Users/ivan4th/work/lea/leacluster/test.sh" "--eval" source-original)
  ;; ("jsonnet"
  ;;  ;; FIXME: add script
  ;;  ;; "jsonnet-qbec"
  ;;  ;; "--jpath" "/Users/ivan4th/work/armcluster/vendor"
  ;;  source-inplace
  ;;  "--ext-str" "qbec.io/env=default"
  ;;  "--ext-code" "argocdSync=false"
  ;;  "--ext-str" "secrets=\"\"")
  :error-patterns
  ((error line-start "Error: evaluating jsonnet: RUNTIME ERROR: "
          (file-name) ":" line ":" column
          (zero-or-one (group "-" (one-or-more digit)))
          (one-or-more space)
          (message) line-end)
   (error line-start "Error: evaluating jsonnet: RUNTIME ERROR: "
          (message) "\n\t"
          (file-name) ":" line ":" column (optional "-" end-column)
          (zero-or-more not-newline) line-end)
   (error line-start "Error: evaluating jsonnet: RUNTIME ERROR: "
          (message) "\n\t"
          (file-name) ":(" line ":" column
          ")-(" end-line ":" end-column ")"
          (zero-or-more not-newline) line-end))
  :modes jsonnet-mode)

;; (add-to-list 'lsp-diagnostics-disabled-modes 'jsonnet-mode)

(flycheck-define-checker jsonnet
  "A Jsonnet syntax checker using the jsonnet binary.

See URL `https://jsonnet.org'."
  :command
  ("jsonnet"
   ;; FIXME: add script
   ;; "jsonnet-qbec"
   ;; "--jpath" "/Users/ivan4th/work/armcluster/vendor"
   source-inplace
   "--ext-str" "qbec.io/env=default"
   "--ext-code" "argocdSync=false"
   "--ext-str" "secrets=\"\"")
  :error-patterns
  (;; qbec
   (error line-start "Error: RUNTIME ERROR: " (message) "\n\t"
          (file-name) ":" (zero-or-one "(")
          line ":" column (zero-or-more not-newline) line-end)
   (error line-start "Error: " (file-name) ":" line ":" column
          (zero-or-one (group "-" (one-or-more digit)))
          (one-or-more space)
          (message) line-end)
   ;; plain jsonnet
   (error line-start "RUNTIME ERROR: " (message) "\n\t"
          (file-name) ":" (zero-or-one "(")
          line ":" column (zero-or-more not-newline) line-end)
   (error line-start "STATIC ERROR: "
	  (file-name) ":" line ":" column (zero-or-one (group "-" (one-or-more digit))) ":"
          (one-or-more space) (message) line-end)
   (error line-start (file-name) ":" line ":" column
          (zero-or-one (group "-" (one-or-more digit)))
          (one-or-more space)
          (message) line-end))
  :modes jsonnet-mode)

(setf jsonnet-path-prefix "/Users/ivan4th/work/armcluster/")
(setf jsonnet-path-items
      '("lib" "environments/default" "vendor" "vendor/1.20" "vendor/doc-util" "vendor/ksonnet-util"))

(defun clipboard-yaml-to-jsonnet ()
  "Convert YAML from the clipboard to nicely indented JSON, then format as Jsonnet, and insert at point."
  (interactive)
  (let ((original-buffer (current-buffer)))
    (with-temp-buffer
      (insert (or (gui-get-selection 'CLIPBOARD) ""))
      (shell-command-on-region
       (point-min) (point-max)
       "yaml2json --json-indent 2 | jsonnetfmt -n 2 -"
       nil t)
      (let ((result (buffer-string)))
        (with-current-buffer original-buffer
          (insert result))))))

(setenv "JSONNET_PATH"
        (string-join (cl-loop for path-item in jsonnet-path-items
                              collect (concat jsonnet-path-prefix path-item))
                     ":"))

(provide 'my-jsonnet)
