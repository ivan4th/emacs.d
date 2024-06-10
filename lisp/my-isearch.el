;; -*- lexical-binding: t -*-
;; go to the beginning of search term after exiting isearch

(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)

(defun my-goto-match-beginning ()
  (when (and isearch-forward isearch-other-end (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))

(defadvice isearch-exit (after my-goto-match-beginning activate)
  "Go to beginning of match."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(provide 'my-isearch)
