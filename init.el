(when (file-exists-p custom-file)
  (load custom-file))

(require 'init-mode)
(require 'init-super-save)

(require 'init-ui)

(require 'init-key)
(require 'init-meow)

(require 'init-edit)

;;; minibuffer
(fido-vertical-mode +1)
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

(with-eval-after-load 'fussy
  (fussy-setup))

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
