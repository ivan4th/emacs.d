;; -*- lexical-binding: t -*-

(provide 'my-go-test)

(require 'go-mode)

(defvar i4-go-test-timeout-seconds 600)
(defvar i4-go-extra-test-args "");; "-args -v 10 -logtostderr true"
;; (defvar i4-go-test-name-regexp "^[ \t]*func[ \t]+\\(Test[^ \t(]+\\)")
(defvar i4-go-test-suite-name-regexp "^[ \t]*func[ \t]+\\(Test[^ \t(]+Suite\\)")
(defvar i4-go-test-name-regexp "^[ \t]*func[ \t]+\\(?:([^)]*)[ \t]+\\)?\\(Test[^ \t(]+\\)")
(defvar i4-go-test-name-regexp/run "^[ \t]*\\(?:===[ \t]+RUN\\|--- PASS:\\|--- FAIL:\\)[ \t]+\\(Test[^ \t\n]+\\)")
(defvar i4-go-last-test-location nil)
(defvar i4-go-test-command "go test")
(defvar i4-go-test-race nil)

(defun i4-go-test-name ()
  (save-excursion
    (beginning-of-line)
    (when (or (looking-at i4-go-test-name-regexp)
              (looking-at i4-go-test-name-regexp/run)
              (re-search-backward i4-go-test-name-regexp nil t)
              (re-search-backward i4-go-test-name-regexp/run nil t))
      (match-string-no-properties 1))))

(defun i4-go-test-suite-names ()
  (save-excursion
    (goto-char (point-min))
    (let ((result
           (with-output-to-string
             (let ((first-p t))
               (while (re-search-forward i4-go-test-suite-name-regexp nil t)
                 (unless (cl-shiftf first-p nil)
                   (princ "|"))
                 (princ (match-string-no-properties 1)))))))
      (unless (string= "" result)
        result))))

(defun i4-go-test-and-suite-name ()
  (let ((test-name (i4-go-test-name))
        (suite-names (i4-go-test-suite-names)))
    (cond ((equal test-name suite-names)
           test-name)
          ((and test-name suite-names)
           (concat test-name "|" suite-names))
          ((or test-name suite-names)))))

(defun i4-go-test-location (&optional with-test-name-p)
  (list (if (buffer-file-name)
            (file-name-directory (buffer-file-name))
          default-directory)
        (when with-test-name-p
          (i4-go-test-and-suite-name))))

(defun i4-run-go-test (location)
  (setf i4-go-last-test-location location)
  ;; -count=1 disables Go test cache as it can be confusing at times
  (compile (format "(cd '%s' && %s%s%s -count=1 -test.v -timeout %ds%s%s)"
                   ;; FIXME: the following is not compatible with // +build !linux
                   ;; "(cd '%s' && %s%s -count=1 -test.v -timeout %ds%s%s $(ls *.go | egrep -v flycheck_))"
                   (cl-first location)
                   ;; GOOS=linux setting may be in place for gopls
                   (if (eq system-type 'darwin) "GOOS=darwin " "")
                   i4-go-test-command
                   (if i4-go-test-race " -race" "")
                   i4-go-test-timeout-seconds
                   (if (cl-second location)
                       (format " -run '^%s$'" (cl-second location))
                     "")
                   (if i4-go-extra-test-args
                       (concat " " i4-go-extra-test-args)
                     ""))))

(defun i4-go-test (arg)
  (interactive "P")
  (i4-run-go-test (i4-go-test-location arg)))

(defun i4-go-last-test ()
  (interactive)
  (i4-run-go-test (or i4-go-last-test-location (i4-go-test-location))))

(define-key go-mode-map "\C-cit" 'i4-go-test)
(define-key go-mode-map "\C-cil" 'i4-go-last-test)

(define-key compilation-mode-map "\C-cit" 'i4-go-test)
(define-key compilation-mode-map "\C-cil" 'i4-go-last-test)

;; https://stackoverflow.com/a/13408008
(require 'ansi-color)

(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(provide 'my-go-test)
