;; -*- lexical-binding: t -*-

(use-package flycheck
  :config
  ;; https://github.com/flycheck/flycheck/issues/1559#issuecomment-478569550
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (set-default 'flycheck-checkers
	       '(sh-bash
		 coffee-coffeelint
		 css-csslint
		 emacs-lisp
		 ;; emacs-lisp-checkdoc
		 ;; go-gofmt
		 go-build
		 go-test
		 haml
		 html-tidy
		 javascript-jshint
		 json-jsonlint
		 lua
		 perl
		 php
		 php-phpcs
		 ;; python-flake8
		 ;; python-pylint
		 python-flake8
		 ruby
		 sass
		 tex-chktex
		 tex-lacheck
		 xml-xmlstarlet
		 sh-zsh)))

(require 'flycheck)

(provide 'my-flycheck)
