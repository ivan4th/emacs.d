;; -*- lexical-binding: t -*-

(require 'my-copilot)

(defvar my-tnsr-pkgs-root
  (file-truename (expand-file-name "~/work/netgate/tnsr-pkgs"))
  "Canonical path to the tnsr-pkgs project root.")

(defun my-tnsr-pkgs-ensure-single-lsp-workspace ()
  "Ensure a single LSP workspace for all tnsr-pkgs sub-repos.
Without this, each sub-repo gets its own clangd process, wasting
resources and preventing cross-project go-to-definition."
  (let* ((session (lsp-session))
         (root my-tnsr-pkgs-root)
         (root/ (concat root "/"))
         (old-folders (lsp-session-folders session))
         (new-folders
          (delete-dups
           (cons root
                 (seq-remove
                  (lambda (f)
                    (let ((resolved (file-truename f)))
                      (or (string= resolved root)
                          (string-prefix-p root/ resolved))))
                  old-folders)))))
    (unless (equal old-folders new-folders)
      (setf (lsp-session-folders session) new-folders)
      (lsp--persist-session session))))

(defun my-c-mode-hook ()
  (when (string-match ".*/tnsr-pkgs/.*" (or (buffer-file-name) ""))
    (setf c-basic-offset 8
          indent-tabs-mode t)
    ;; Resolve symlinks (~/work -> /Volumes/work) so clangd inside the
    ;; container sees paths that exist on its filesystem.  Without this,
    ;; relative #includes fail and .clangd config isn't found.
    (let ((truename (file-truename (buffer-file-name))))
      (unless (string= truename (buffer-file-name))
        (setq buffer-file-name truename
              default-directory (file-name-directory truename))))
    (copilot-mode 1)
    (my-tnsr-pkgs-ensure-single-lsp-workspace)
    (lsp)))

(add-hook 'c-mode-hook
          #'(lambda ()
              (my-c-mode-hook)))

(setf lsp-clangd-binary-path "/Users/ivan4th/bin/chimera-clangd")

;; https://mail.gnu.org/archive/html/help-gnu-emacs/2024-12/msg00198.html

(defvar yk-compilation-filter-alist nil
  "List of (REGEXP REPLACEMENT) to apply to compilation messages.")

(defun yk-compilation-filter ()
  "Apply replacements to compilation messages."
  (save-excursion
    (pcase-dolist (`(,regexp ,replacement) yk-compilation-filter-alist)
      (goto-char compilation-filter-start)
      (beginning-of-line)
      (while (re-search-forward regexp nil t)
        (replace-match replacement nil nil)))))

(add-to-list 'compilation-filter-hook
             'yk-compilation-filter)

(provide 'my-c)
