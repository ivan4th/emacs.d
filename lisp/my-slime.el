;; -*- lexical-binding: t -*-

(use-package slime)
(use-package slime-company
  :after (slime company)
  :config (setq slime-company-completion 'fuzzy
                slime-company-after-completion 'slime-company-just-one-space))

(setq slime-net-coding-system 'utf-8-unix)

(add-hook 'slime-repl-mode-hook
          (lambda ()
	    (slime-company-maybe-enable) ;; FIXME: this shouldn't be needed
            (paredit-mode +1)
            (setf slime-complete-symbol-function
                  'slime-fuzzy-complete-symbol)))

(setf ;; slime-backend (concat (file-name-as-directory i4-slime-dir) "swank-loader.lisp")
      inhibit-splash-screen t)

(require 'slime)
(set-default 'slime-when-complete-filename-expand t)
(setq slime-lisp-implementations
      '((sbcl ("sbcl" "--dynamic-space-size" "16384"))))
(setq slime-use-autodoc-mode nil)
(slime-setup '(slime-repl
	       slime-tramp slime-asdf slime-scratch slime-autodoc
	       slime-editing-commands slime-references slime-fuzzy
	       slime-indentation slime-fancy-inspector slime-c-p-c
	       slime-presentations slime-xref-browser
	       slime-fontifying-fu
	       slime-company
               ;; slime-js
               )) ;; FIXME: slime-fancy broke due to slime-package-fu
(slime-require :swank-listener-hooks)

(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
;;(setq lisp-indent-function 'common-lisp-indent-function)
(setq common-lisp-hyperspec-root (concat (getenv "HOME") "/HyperSpec/"))

(define-key slime-repl-mode-map "\M-\C-r" 'slime-repl-previous-matching-input)
(define-key slime-repl-mode-map "\M-\C-s" 'slime-repl-next-matching-input)

(setf slime-use-autodoc-mode t
      slime-autodoc-use-multiline-p t
      slime-repl-history-size 2000
      slime-protocol-version 'ignore)

(defvar i4-vtf-last-test nil)
(defvar i4-vtf-test-rx "(deftest[ \n\t]+\\([^ \n\t]+\\)")

(defun i4-vtf-test-name ()
  (save-excursion
    (beginning-of-line)
    (unless (looking-at i4-vtf-test-rx)
      (beginning-of-defun))
    (unless (looking-at "(deftest[ \n\t]+\\([^ \n\t]+\\)")
      (error "cannot locate VTF test"))
    (cons (downcase (match-string 1))
          (slime-current-package))))

(defun i4-vtf-do-run-test (&optional last-p)
  (let ((test-name
         (if (or (not last-p) (not i4-vtf-last-test))
             (setf i4-vtf-last-test (i4-vtf-test-name))
           i4-vtf-last-test)))
    (message "Running test: %s" (car test-name))
    (slime-repl-write-string (format "\n*** running test: %s ***\n" (car test-name)))
    (slime-eval `(swank:interactive-eval ,(format "(progn (%s) (format t \"~&*** success ***\"))"
                                                  (car test-name)))
                (cdr test-name))))

(defun i4-vtf-run-test (&optional raw-prefix-arg)
  (interactive "P")
  (i4-vtf-do-run-test))

(defun i4-vtf-run-last-test ()
  (interactive)
  (i4-vtf-do-run-test t))

(defun my-slime-indent-and-complete-symbol ()
  "Similar to slime-indent-and-complete-symbol, but using company-mode"
  (interactive)
  (let ((pos (point)))
    (unless (get-text-property (line-beginning-position) 'slime-repl-prompt)
      (lisp-indent-line))
    (when (= pos (point))
      (cond ((save-excursion (re-search-backward "[^() \n\t\r]+\\=" nil t))
             (company-complete))
            ((memq (char-before) '(?\t ?\ ))
             (slime-echo-arglist))))))

(add-hook 'slime-mode-hook
          #'(lambda ()
	      (slime-company-maybe-enable) ;; FIXME: this shouldn't be needed
              (local-set-key "\C-cit" 'i4-vtf-run-test)
              (local-set-key "\C-cil" 'i4-vtf-run-last-test)
              (local-set-key [f7] 'i4-lisp-toggle-unit-test)
	      ;; (local-set-key [tab] 'slime-indent-and-complete-symbol)
              (local-set-key [tab] 'my-slime-indent-and-complete-symbol)))

(global-set-key "\C-cl" 'slime-selector)

;; my lisp keywords (indentation code lifted from cl-indent.el)

;; (let ((l '((defxtruct . defclass))))
;;   (dolist (el l)
;;     (put (car el) 'common-lisp-indent-function
;;          (if (symbolp (cdr el))
;;              (get (cdr el) 'common-lisp-indent-function)
;;              (car (cdr el))))))

(put 'iter 'common-lisp-indent-function '(&lambda))

  ;;; skeleton stuff (TBD: use yasnippet(?))
(define-skeleton %asd-skeleton
  "Insert ASDF system definition skeleton" nil
  ";;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-" \n \n
  "(defpackage #:" i4-asd-name ".system" \n
  > "(:use #:cl #:asdf))" \n \n
  "(in-package :" i4-asd-name ".system)" \n \n
  "(defsystem #:" i4-asd-name \n
  > ":name \"" i4-asd-name "\"" \n
  > ":author \"TODO\"" \n
  > ":version \"0.1\"" \n
  > ":description \"" _ "\"" \n
  > ":depends-on (:i4utils)" \n
  > ":components ((:module " i4-asd-name \n
  > ":pathname \"\"" \n
  > ":serial t" \n
  > ":components" \n
  > "((:file \"package\")" \n
  > "(:file \"" i4-asd-name "\" :depends-on (\"package\"))))))" \n \n)

(defun get-asd-name ()
  (let ((filename (buffer-file-name)))
    (unless (string-match "\\(\\w+\\)/[^/]+\\.\\(?:asd\\|lisp\\)$" filename)
      (error "invalid file name"))
    (match-string 1 filename)))

(defvar i4-asd-name nil)

(defun asd-skeleton ()
  (interactive)
  (let ((i4-asd-name (get-asd-name)))
    (lisp-mode)
    (%asd-skeleton)))

(define-skeleton %package-skeleton
  "Insert defpackage skeleton" nil
  "(in-package :cl-user)" \n \n
  "(defpackage :" i4-asd-name \n
  > "(:use :cl :i4utils " _ "))" \n \n)

(defun package-skeleton ()
  (interactive)
  (let ((i4-asd-name (get-asd-name)))
    (%package-skeleton)))

(define-abbrev lisp-mode-abbrev-table "pckg" "" 'package-skeleton)

(defun i4-lisp-toggle-unit-test ()
  (interactive)
  (find-file
   (let* ((path (buffer-file-name))
	  (dir (file-name-directory path)))
     (if (string-match "/tests/$" dir)
	 (concat (cl-subseq dir 0 (- (length dir) 6))
		 (replace-regexp-in-string
		  "-test\\.lisp$" ".lisp"
		  (file-name-nondirectory path)))
       (format "%stests/%s-test.lisp"
               dir
               (file-name-sans-extension
                (file-name-nondirectory path))))))
  ;;(slime-sync-package-and-default-directory)
  )

(cl-pushnew ".lx64fsl" completion-ignored-extensions :test #'equal)
(cl-pushnew ".lafsl" completion-ignored-extensions :test #'equal)

;; (set-default 'slime-filename-translations
;;              (list (list "^\\(rpibuilder\\)"
;;                          (lambda (emacs-filename)
;;                            (replace-regexp-in-string
;;                             "^/somedir/ccl-dev/"
;;                             "/opt/ccl-arm/"
;;                             (replace-regexp-in-string "^/Users/someuser/" "/root/" emacs-filename)))
;;                          (lambda (lisp-filename)
;;                            (replace-regexp-in-string
;;                             "^/opt/ccl-arm/"
;;                             "/somedir/ccl-dev/"
;;                             (replace-regexp-in-string "^/root/" "/Users/someuser/" lisp-filename))))))


(defun sbcl ()
  (interactive)
  (slime 'sbcl))

;; (when (file-exists-p "~/quicklisp/log4slime-setup.el")
;;   (load "~/quicklisp/log4slime-setup.el"))

;; (global-log4slime-mode 1)

(provide 'my-slime)
