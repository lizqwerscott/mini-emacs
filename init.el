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
