;; -*- lexical-binding: t -*-

(defvar auth-source-1pw-mappings-initialized nil)
(defvar auth-source-1pw-mappings '())
(defvar auth-source-1pw-mappings-src '())


(defun auth-source-1pw-mappings ()
  (if auth-source-1pw-mappings-initialized
      auth-source-1pw-mappings
    (cl-flet ((resolve (value)
		(cl-etypecase value
		  (string value)
		  (function (funcall value))
		  (symbol (symbol-value value)))))
      (setq auth-source-1pw-mappings
	    (cl-loop for ((host . user) . spec) in auth-source-1pw-mappings-src
		     collect (cons (cons (resolve host) (resolve user))
				   (resolve spec)))
	    auth-source-1pw-mappings-initialized
	    t)
      auth-source-1pw-mappings)))

(defun 1pw-normalize-secret-ref (ref)
  (replace-regexp-in-string "^op://" "" ref))

(use-package auth-source-1password
  :functions
  auth-source-1password-enable
  auth-source-1password--1password-construct-query-path
  :config
  (setf auth-source-1password-construct-secret-reference
	#'(lambda (backend type host user port)
	    (message "Auth Source: ref %S" (cons host user))
	    (let ((spec (cdr (assoc (cons host user) (auth-source-1pw-mappings)))))
              (cond ((null spec)
		     (auth-source-1password--1password-construct-query-path
		      backend type host user port))
		    ((stringp spec)
		     (1pw-normalize-secret-ref spec))
		    (t
		     (error "bad 1password secret spec: %S" spec))))))
  (auth-source-1password-enable))

;; TODO: must be lazy!!!
(defun define-auth-source-1pw-mapping (host user spec)
  "Add 1password auth-source mapping for the specified host and user.
  spec can be a string, in which case it's interpreted as a
  secret reference, or it can be a symbol in which case it is
  interpreted as a variable name. The variable is resolved when
  the secret is actually requested."
  (unless (or (stringp spec)
	      (symbolp spec))
    (error "bad 1password secret spec: %S" spec))
  (setf auth-source-1pw-mappings-initialized nil
	auth-source-1pw-mappings nil)
  (push (cons (cons host user) spec)
	auth-source-1pw-mappings-src))

;; (auth-source-forget-all-cached)

(provide 'my-auth-source)
