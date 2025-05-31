(when (file-exists-p custom-file)
  (load custom-file))

(require 'init-mode)
(require 'init-super-save)

(require 'init-ui)

(require 'init-key)
(require 'init-meow)

(require 'init-edit)

(require 'init-minibuffer)
;; Ibuffer filters
(setq ibuffer-saved-filter-groups
      '(("default"
         ("org" (or
                 (mode . org-mode)
                 (name . "^\\*Org Src")
                 (name . "^\\*Org Agenda\\*$")))
         ("tramp" (name . "^\\*tramp.*"))
         ("emacs" (or
                   (name . "^\\*scratch\\*$")
                   (name . "^\\*Messages\\*$")
                   (name . "^\\*Warnings\\*$")
                   (name . "^\\*Shell Command Output\\*$")
                   (name . "^\\*Async-native-compile-log\\*$")
                   (name . "^\\*straight-")))
         ("ediff" (or
                   (name . "^\\*ediff.*")
                   (name . "^\\*Ediff.*")))
         ("dired" (mode . dired-mode))
         ("terminal" (or
                      (mode . term-mode)
                      (mode . shell-mode)
                      (mode . eshell-mode)))
         ("help" (or
                  (name . "^\\*Help\\*$")
                  (name . "^\\*info\\*$")
                  (name . "^\\*helpful"))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))
(setq ibuffer-show-empty-filter-groups nil) ; don't show empty groups

(with-eval-after-load 'proced
  (setq proced-enable-color-flag t
        proced-tree-flag t
        proced-auto-update-flag 'visible
        proced-descend t
        proced-filter 'user))

(add-hook 'proced-mode-hook
          #'(lambda ()
              (proced-toggle-auto-update 1)))

(require 'init-corfu)

(require 'init-dired)
(require 'init-helpful)

(require 'init-org)

(require 'init-git)

(require 'init-program)

;;; init.el ends here.
