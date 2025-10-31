;;; init-dired.el --- dired                          -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'lib-dired)

;;; dired
(require 'dired)
;; (setq trash-directory "~/.trashs/")
;; (setq delete-by-moving-to-trash t)
;; (setq dired-recursive-deletes 'always)
(setq dired-auto-revert-buffer t)
(setq dired-hide-details-hide-symlink-targets nil)
(setq dired-dwim-target t)
(setq dired-listing-switches "-AFhlv --group-directories-first")
;; (setq dired-listing-switches
;;       "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
(setq dired-kill-when-opening-new-dired-buffer t)

;; Dont prompt about killing buffer visiting delete file
(setq dired-clean-confirm-killing-deleted-buffers nil)
(setq dired-guess-shell-alist-user
      `((,(rx "."
              (or
               ;; Videos
               "mp4" "avi" "mkv" "flv" "ogv" "ogg" "mov"
               ;; Music
               "wav" "mp3" "flac"
               ;; Images
               "jpg" "jpeg" "png" "gif" "xpm" "svg" "bmp"
               ;; Docs
               "pdf" "md" "djvu" "ps" "eps" "doc" "docx" "xls" "xlsx" "ppt" "pptx")
              string-end)
         ,(cond ((eq system-type 'gnu/linux) "xdg-open")
                ((eq system-type 'darwin) "open")
                ((eq system-type 'windows-nt) "start")
                (t "")))))

(setq ls-lisp-dirs-first t)
(when sys/macp
  (setq insert-directory-program "/opt/homebrew/bin/gls"))

;; Enable the disabled dired commands
(put 'dired-find-alternate-file 'disabled nil)

(advice-add 'find-file :around 'find-file-auto)

;;; dired-aux
(setopt dired-isearch-filenames 'dwim
        dired-create-destination-dirs 'ask
        dired-vc-rename-file t)

;;; dired-x
(setopt dired-bind-vm nil
        dired-bind-man nil
        dired-bind-info nil
        dired-omit-verbose nil
        dired-omit-files (rx string-start
                             (or ".DS_Store"
                                 ".cache"
                                 ".vscode"
                                 ".ccls-cache" ".clangd")
                             string-end)
        dired-omit-files (concat dired-omit-files "\\|^\\..*$"))

;;; Hook
(add-hook 'dired-mode-hook
          #'(lambda ()
              (dired-hide-details-mode)
              (when (boundp 'dired-async-mode)
                (dired-async-mode))
              (diredfl-mode)
              (dired-omit-mode)))

(require 'lib-transient)
;;; Menu
(pretty-transient-define-prefix dired-dispatch ()
  "Dired dispatch menu."
  [["Directory"
    ("h" "Hide Details" dired-hide-details-mode :toggle t :transient t)
    ("o" "Omit Mode" dired-omit-mode :toggle t :transient t)]]
  [("q" "Quit" transient-quit-all)])

;;; Keymap
(keymap-binds dired-mode-map
  ("e" . dired-toggle-read-only)
  ("f" . (lambda ()
           (interactive)
           (consult-fd default-directory)))
  ("F" . consult-fd-dir)
  ("W" . dired-copy-path)
  ("C-c +" . dired-create-empty-file)
  ("C-+" . dired-create-empty-file)
  ("h" . dired-up-directory)
  ("C-c e" . dired-do-open-default)
  ("C-o" . dired-dispatch)

  ("/" . prot-dired-limit-regexp)
  ("C-c C-l" . prot-dired-limit-regexp)

  (("M-n" "s-n") . prot-dired-subdirectory-next)
  (("M-p" "s-p") . prot-dired-subdirectory-previous)
  ("C-c C-n" . prot-dired-subdirectory-next)
  ("C-c C-p" . prot-dired-subdirectory-previous)

  ("J" . dired-jump-first-file))

(global-bind-keys
 ("C-x J" . dired-jump-other-window)
 ("C-x x e" . dired-do-open-default))

(provide 'init-dired)
;;; init-dired.el ends here
