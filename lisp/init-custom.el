;;; init-custom.el --- define customizations         -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defcustom user/minibuffer 'fido
  "The minibuffer farmework."
  :group 'user
  :type '(choice (const :tag "fido" fido)
                 (const :tag "mct" mct)
                 (const :tag "vertico" vertico)))

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init-custom)
;;; init-custom.el ends here
