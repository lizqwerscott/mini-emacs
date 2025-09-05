;;; init-helpful.el --- init helpful package         -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(global-set-keys
 '(("C-h f" . helpful-callable)
   ("C-h C-f" . helpful-callable)
   ("C-h k" . helpful-key)
   ("C-h v" . helpful-variable)
   ("C-h F" . helpful-function)
   ("C-h C" . helpful-command)))

(add-hooks '(help-mode helpful-mode)
           #'visual-line-mode)

(provide 'init-helpful)
;;; init-helpful.el ends here
