;;; init-minibuffer.el --- init minibuffer           -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(marginalia-mode t)

;;; fido
(when (equal user/minibuffer 'fido)
  (fido-vertical-mode t))

(require 'icomplete)

(setq icomplete-delay-completions-threshold 0
      icomplete-compute-delay 0
      icomplete-show-matches-on-no-input t
      icomplete-hide-common-prefix nil
      icomplete-prospects-height 10
      icomplete-separator " . "
      icomplete-with-completion-tables t
      icomplete-in-buffer t
      icomplete-max-delay-chars 0
      icomplete-scroll t)

(if icomplete-in-buffer
    (advice-add 'completion-at-point
                :after #'minibuffer-hide-completions))

;; keymap
(bind-key "TAB" #'icomplete-force-complete icomplete-minibuffer-map)
(bind-key "RET" #'icomplete-force-complete-and-exit icomplete-minibuffer-map)
(keymap-set icomplete-fido-mode-map "s-j" #'icomplete-fido-exit)

(add-hook 'icomplete-minibuffer-setup-hook
          (lambda ()
            (setq-local max-mini-window-height 0.5)))

;; minibuffer prefix
(when (and (>= emacs-major-version 31)
           (boundp 'icomplete-vertical-in-buffer-adjust-list))
  (setq icomplete-vertical-in-buffer-adjust-list t)
  (setq icomplete-vertical-render-prefix-indicator t))

(when (or (< emacs-major-version 31)
          (not (boundp 'icomplete-vertical-in-buffer-adjust-list)))
  (require 'minibuffer-prefix))

;;; mct
(require 'mct)
(keymap-sets mct-minibuffer-local-completion-map
             '(("TAB" . mct-edit-completion)))

(when (equal user/minibuffer 'mct)
  (mct-mode))

;;; minibuffer setting

;; Keep minibuffer lines unwrapped, long lines like on M-y will be truncated
(add-hook 'minibuffer-setup-hook
          (lambda () (setq truncate-lines t)))

;; (setq read-buffer-completion-ignore-case t)
;; (setq read-file-name-completion-ignore-case t)

;; (setq resize-mini-windows t)
(setq minibuffer-eldef-shorten-default t)

(file-name-shadow-mode 1)
(minibuffer-depth-indicate-mode 1)
(minibuffer-electric-default-mode 1)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here.
