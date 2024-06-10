;; -*- lexical-binding: t -*-
(add-hook 'makefile-mode-hook
          #'(lambda ()
              (whitespace-mode 1)))

(provide 'my-makefile)
