;; -*- lexical-binding: t -*-

(use-package org)
(use-package orgit)
(use-package org-journal)

(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-clock)
(require 'org-journal)
(require 'org-macs)
(require 'org-refile)
(require 'org-table)
(require 'org-list)

(defcustom i4-org-dir "~/org"
  "Directory for the org files"
  :type 'string
  :group 'i4-config)

(defcustom i4-org-task-files '()
  "(key description path) pairs for the org files with tasks used to
  generate org-capture-templates"
  :type '(repeat (list (string :tag "Key")
		       (string :tag "Description")
		       (string :tag "Org file path")))
  :group 'i4-config)

(defun i4-org-path (filename)
  (concat (file-name-as-directory i4-org-dir) filename))

(defun i4-gen-org-template (entry)
  (cl-destructuring-bind (key description path) entry
    (list
     key description 'entry
     (list 'file+headline path "Tasks")
      "* TODO %?\n  %i\n  %a")))

(defun i4-gen-org-templates ()
  (append
   (mapcar #'i4-gen-org-template i4-org-task-files)
   `(("n" "Note" entry (file+headline ,(i4-org-path "notes.org") "Notes")
      "* %?\n  %i\n  %a")
     ("j" "Journal" entry (file+datetree ,(i4-org-path "journal.org"))
      "* %?\nEntered on %U\n  %i\n  %a"))))

(setf org-replace-disputed-keys t
      ;; FIXME
      ;; org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)
      org-todo-keywords '("TODO" "DONE")
      org-todo-keyword-faces '(("TODO" . (:foreground "orange" :weight bold :underline t)))
      org-log-done t
      org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s%l")
        (todo . " %i %-12:c")
        (tags . " %i %-12:c")
        (search . " %i %-12:c"))
      ;; https://www.reddit.com/r/emacs/comments/4366f9/how_do_orgrefiletargets_work/
      ;; org-refile-targets '((nil :maxlevel . 3)
      ;;                      (org-agenda-files :maxlevel . 3))
      org-refile-targets '((org-agenda-files :regexp . " *\\(Tasks\\|Notes\\) *$")
                           (org-agenda-files :tag . "refile"))
      ;; Refile in a single go
      org-outline-path-complete-in-steps nil
      ;; Show full paths for refiling
      org-refile-use-outline-path t

      org-src-tab-acts-natively t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)))

(defvar *i4-dt-replacements*
  '(("Mon" . "Пнд")
    ("Tue" . "Втр")
    ("Wed" . "Срд")
    ("Thu" . "Чтв")
    ("Fri" . "Птн")
    ("Sat" . "Сбт")
    ("Sun" . "Вск")))

(defadvice org-insert-time-stamp (around org-date-rus activate)
  "Insert russian dates in org mode"
  (let ((p-start (point))
	(result ad-do-it)
	(p-end (point)))
    (save-excursion
      (dolist (item *i4-dt-replacements*)
	(replace-string (car item) (cdr item) t p-start p-end)))
    (insert " ")
    (org-fix-tags-on-the-fly)
    result))

(add-hook 'org-mode-hook
          #'(lambda ()
              (define-key org-mode-map [(shift up)] nil)
              (define-key org-mode-map [(shift down)] nil)
              (define-key org-mode-map [(shift left)] nil)
              (define-key org-mode-map [(shift right)] nil)
              (define-key org-mode-map "\M--" nil)))

;; (define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(defun i4-org-clock-in-last (&optional arg)
  (interactive "P")
  (org-clock-in-last arg)
  (org-save-all-org-buffers))

(defun i4-org-clock-out (&optional arg)
  (interactive "P")
  (org-clock-out arg)
  (org-save-all-org-buffers))

(defun i4-org-capture ()
  (interactive)
  (setf org-capture-templates
	(i4-gen-org-templates))
  (org-capture))

(global-set-key "\C-ci1" 'i4-org-clock-in-last)
(global-set-key "\C-ci2" 'i4-org-clock-out)
(global-set-key "\C-cic" 'i4-org-capture)

;; based on https://emacs.stackexchange.com/a/59239
(defun org-dblock-write:weekly (params)
  (cl-flet ((fmttm (tm) (format-time-string (org-time-stamp-format nil nil) tm)))
    (let ((file (or (plist-get params :file) (buffer-file-name)))
          (start (seconds-to-time
                  (org-matcher-time (plist-get params :tstart))))
          (end (seconds-to-time (org-matcher-time (plist-get params :tend)))))
      (while (time-less-p start end)
        (let ((next-week (time-add start
                                   (date-to-time "1970-01-08T00:00Z")))
              (week-begin (line-beginning-position))
              (week-minutes 0))
          (insert "\nWeekly Table from " (fmttm start) "\n")
          (insert "| Day of Week | Time |\n|-\n")
          (while (time-less-p start next-week)
            (let* ((next-day (time-add start (date-to-time "1970-01-02T00:00Z")))
                   (minutes
                    (with-current-buffer (find-file-noselect file)
                      (cadr (org-clock-get-table-data
                             file
                             (list :maxlevel 0
                                   :tstart (fmttm start)
                                   :tend (fmttm next-day)))))))
              (insert "|" (format-time-string "%a %m.%d" start)
                      "|" (format "%d:%02d" (floor minutes 60) (mod minutes 60))
                      "|\n")
              (org-table-align)
              (cl-incf week-minutes minutes)
              (setq start next-day)))
          (when (equal week-minutes 0)
            (delete-region week-begin (line-beginning-position))))))))

(defun i4-org-toggle-checkbox ()
  (interactive)
  (let ((p (point)))
    (org-toggle-checkbox)
    (org-sort-list nil ?X)
    (goto-char p)))

(define-key org-mode-map (kbd "<s-return>") 'i4-org-toggle-checkbox)

;; https://emacs.stackexchange.com/questions/19446/is-it-possible-to-change-and-freeze-the-current-date-time-in-emacs

(defvar org-commands-with-current-time '(org-current-time org-read-date-analyze org-today org-current-effective-time org-todo-yesterday org-read-date-analyze org-time-stamp-to-now org-small-year-to-year org-closest-date org-goto-calendar org-get-cursor-date org-time-string-to-absolute org-time-stamp-inactive org-time-stamp org-sort-entries)
  "Functions to be adviced by `org-ad-freeze-time'.")

(defvar org-frozen-time nil
  "Frozen time for org-commands. It should have the same format as
  the return value of `current-time'.
  Time flows normal if set to `nil'.")

(defun org-freeze-time (time)
  "Freeze `current-time' at the given TIME in the org-functions from
  `org-commands-with-current-time'"
  (interactive (list (org-read-date nil nil nil "Input freeze time:")))
  (setq org-frozen-time (append (org-read-date nil t time) '(0 0))))

(defun org-release-time ()
  "Release the time frozen by `org-freeze-time'."
  (interactive)
  (setq org-frozen-time nil))

(defmacro org-ad-freeze-time (fun)
  "Advice FUN to use `org-frozen-time'."
  (let ((old-def (make-symbol "old-def")))
    `(defadvice ,fun (around freeze-time activate)
       (let ((,old-def (symbol-function 'current-time)))
         (unwind-protect
             (progn
               (when org-frozen-time
                 (fset 'current-time (lambda () org-frozen-time)))
               ad-do-it)
           (fset 'current-time ,old-def))))))

(dolist (fun org-commands-with-current-time)
  (eval (macroexpand `(org-ad-freeze-time ,fun))))

(defun i4-org-todo-at (&optional arg)
  (interactive "P")
  (org-freeze-time (org-read-date))
  (unwind-protect
      (if (eq major-mode 'org-agenda-mode)
          (org-agenda-todo arg)
        (org-todo arg))
    (org-release-time)))

(provide 'my-org)
