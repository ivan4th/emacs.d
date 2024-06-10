;; -*- lexical-binding: t -*-

(defun i4-unurl (start end)
  (interactive "r")
  (save-excursion
    (goto-char start)
    (let ((text (buffer-substring-no-properties start end)))
      (delete-region start end)
      (insert
       (decode-coding-string
        (url-unhex-string
         (replace-regexp-in-string
          "&amp;\\|&" "\n"
          (cl-substitute ?\s ?+ text)))
        'utf-8)))))

(defun i4-dtw-before-save ()
  ;; derived from js2-mode.el
  (let ((col (current-column)))
    (delete-trailing-whitespace)
    ;; don't change trailing whitespace on current line
    (unless (eq (current-column) col)
      (indent-to col))))

(defun i-dont-like-trailing-whitespace ()
  (interactive)
  (setf show-trailing-whitespace t)
  (set (make-local-variable 'before-save-hook)
       #'i4-dtw-before-save))

;; from stevey's .emacs

;; from http://blog.tuxicity.se/elisp/emacs/2010/03/26/rename-file-and-buffer-in-emacs.html
(defun rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (message "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file name new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)))))))

(global-set-key (kbd "C-c r") 'rename-file-and-buffer)

(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	  (if (string-match dir "\\(?:/\\|\\\\)$")
	      (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
	(progn
	  (copy-file filename newname 1)
	  (delete-file filename)
	  (set-visited-file-name newname)
	  (set-buffer-modified-p nil)
	  t))))

;; line deletion -- from http://homepages.inf.ed.ac.uk/s0243221/emacs/

;; First define a variable which will store the previous column position
(defvar previous-column nil "Save the column position")

;; Define the nuke-line function. The line is killed, then the newline
;; character is deleted. The column which the cursor was positioned at is then
;; restored. Because the kill-line function is used, the contents deleted can
;; be later restored by usibackward-delete-char-untabifyng the yank commands.
(defun nuke-line()
  "Kill an entire line, including the trailing newline character"
  (interactive)

  ;; Store the current column position, so it can later be restored for a more
  ;; natural feel to the deletion
  (setq previous-column (current-column))

  ;; Now move to the end of the current line
  (end-of-line)

  ;; Test the length of the line. If it is 0, there is no need for a
  ;; kill-line. All that happens in this case is that the new-line character
  ;; is deleted.
  (if (= (current-column) 0)
    (delete-char 1)

    ;; This is the 'else' clause. The current line being deleted is not zero
    ;; in length. First remove the line by moving to its start and then
    ;; killing, followed by deletion of the newline character, and then
    ;; finally restoration of the column position.
    (progn
      (beginning-of-line)
      (kill-line)
      (delete-char 1)
      (move-to-column previous-column))))

(global-set-key "\C-c\C-q" 'nuke-line)

(defun insert-random-string (num-times)
  "Insert a random alphanumerics string of length 6 repeated num-times times."
  (interactive "p")
  (let ((mycharset ["1" "2" "3" "4" "5" "6" "7" "8" "9" "0"
                    "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
                    "k" "l" "m" "n" "o" "p" "q" "r" "s" "t"
                    "u" "v" "w" "x" "y" "z"]))
    (cl-loop repeat (* num-times 6)
             do (insert (elt mycharset (random (length mycharset)))))))

(global-set-key "\C-ci-" 'insert-random-string)

(defalias 'dtw 'delete-trailing-whitespace)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(defun i4-insert-pw ()
  "Insert generated password at the point"
  (interactive)
  (shell-command "echo -n `pwgen -cn 16 1`" t)
  (goto-char (mark)))

(global-set-key [f9] 'browse-url-at-point)

(defun killemall (regexp)
  (interactive "sKill buffers with paths matching this regexp: ")
  (dolist (buffer (buffer-list))
    (let ((name (or (buffer-file-name buffer)
                    (with-current-buffer buffer
                      (when (boundp 'dired-directory)
                        (if (consp dired-directory)
                            (car dired-directory)
                          dired-directory))))))
      (when (and name (not (string-equal name ""))
                 (string-match regexp name))
        (kill-buffer buffer)))))

(defun savemall (regexp)
  (interactive "sSave buffers with paths matching this regexp: ")
  (dolist (buffer (buffer-list))
    (let ((name (or (buffer-file-name buffer)
                    (with-current-buffer buffer
                      (when (boundp 'dired-directory)
                        (if (consp dired-directory)
                            (car dired-directory)
                          dired-directory))))))
      (when (and name (not (string-equal name ""))
                 (string-match regexp name)
                 (not (string-match "/$" name)))
        (with-current-buffer buffer
          (set-buffer-modified-p t)
          (save-buffer))))))

(require 'ansi-color)

(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun i4-insert-uuid ()
  "Insert UUID at the point"
  (interactive)
  (let ((case-fold-search t))
    (when (looking-at "[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}")
      (delete-region (point) (+ (point) 36)))
    (shell-command "echo -n `uuid -v4`" t)
    (goto-char (mark))))

(provide 'my-tools)
