;;; init-tools.el --- tools                          -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'eww
  (keymap-binds eww-mode-map
    ("M-n" . scroll-up-1/3)
    ("M-p" . scroll-down-1/3)))

;;; Ibuffer
;; Ibuffer group filters
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

(setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))

(require 'ibuffer-color)

(setq ibuffer-formats '((mark modified read-only locked
                              " " (name 18 18 :left :elide)
                              " " (size-h 9 -1 :right)
                              " " (mode+ 16 16 :left :elide)
                              " " (recency+ 9 9 :left :elide)
                              " " filename-and-process+)
                        (mark " " (name 16 -1)
                              " " filename)))

(setq ibuffer-show-empty-filter-groups nil ; don't show empty groups
      ibuffer-human-readable-size t)

(defun ibuffer-refersh ()
  "Open and refresh ibuffer."
  (interactive)
  (when-let* ((buffer (get-buffer "*Ibuffer*")))
    (kill-buffer buffer))
  (ibuffer))

(global-bind-keys
 ("C-x C-b" . ibuffer-refersh))

;;; proced
(setq proced-auto-update-flag 'visible
      proced-auto-update-interval 3
      proced-enable-color-flag t)

(provide 'init-tools)
;;; init-tools.el ends here
