;; -*- lexical-binding: t -*-

(require 'my-flycheck)

(add-hook 'sh-mode-hook
	  '(lambda ()
             (flycheck-mode 1)
             (setf sh-basic-offset 2
                   sh-indentation 2
                   sh-first-lines-indent 0
                   sh-indent-after-case '+
                   sh-indent-after-do '+
                   sh-indent-after-done 0
                   sh-indent-after-else '+
                   sh-indent-after-if '+
                   sh-indent-after-loop-construct '+
                   sh-indent-after-open '+
                   sh-indent-comment t
                   sh-indent-for-case-alt '++
                   sh-indent-for-case-label '+
                   sh-indent-for-continuation '+
                   sh-indent-for-do 0
                   sh-indent-for-done 0
                   sh-indent-for-else 0
                   sh-indent-for-fi 0
                   sh-indent-for-then 0)))

(provide 'my-shellscript)
