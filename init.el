(when (file-exists-p custom-file)
  (load custom-file))

(require 'init-mode)
(require 'init-super-save)

(require 'init-ui)

(require 'init-key)
(require 'init-meow)

(require 'init-edit)

;;; minibuffer
(fido-vertical-mode t)
(marginalia-mode t)

(keymap-set icomplete-fido-mode-map "s-j" #'icomplete-fido-exit)

(setq completion-styles '(fussy basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles basic partial-completion))))

(setq tab-always-indent 'complete
      icomplete-delay-completions-threshold 0
      icomplete-compute-delay 0
      icomplete-show-matches-on-no-input t
      icomplete-hide-common-prefix nil
      icomplete-prospects-height 9
      icomplete-separator " . "
      icomplete-with-completion-tables t
      icomplete-in-buffer t
      icomplete-max-delay-chars 0
      icomplete-scroll t
      resize-mini-windows 'grow-only
      icomplete-matches-format nil)
(bind-key "TAB" #'icomplete-force-complete icomplete-minibuffer-map)
(bind-key "RET" #'icomplete-force-complete-and-exit icomplete-minibuffer-map)
(add-hook 'icomplete-minibuffer-setup-hook
          (lambda ()
            (setq-local max-mini-window-height 0.5)))

(with-eval-after-load 'fussy
  (fussy-setup))

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


(require 'init-corfu)

(require 'init-dired)
(require 'init-helpful)

(require 'init-org)

(require 'init-magit)

(keymap-set mode-specific-map "p" project-prefix-map)

(with-eval-after-load 'project
  (keymap-sets project-prefix-map
               '(("v" . magit-status))))

;;; init.el ends here.
