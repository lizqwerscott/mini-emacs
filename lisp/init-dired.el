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
(require 'dired-aux)
(setq dired-isearch-filenames 'dwim)
(setq dired-create-destination-dirs 'ask)
(setq dired-vc-rename-file t)

;;; dired-x
(require 'dired-x)
(setq dired-bind-vm nil)
(setq dired-bind-man nil)
(setq dired-bind-info nil)
(setq dired-omit-verbose nil)
(setq dired-omit-files (rx string-start
                           (or ".DS_Store"
                               ".cache"
                               ".vscode"
                               ".ccls-cache" ".clangd")
                           string-end))
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..*$"))

;;; Hook
(add-hook 'dired-mode-hook
          #'(lambda ()
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
(keymap-sets dired-mode-map
  '(("TAB" . dired-subtree-cycle)
    ("e" . dired-toggle-read-only)
    ("W" . dired-copy-path)
    ("C-c +" . dired-create-empty-file)
    ("M-n" . scroll-up-1/3)
    ("M-p" . scroll-down-1/3)
    ("h" . dired-up-directory)
    ("C-c e" . dired-do-open-default)
    ("C-o" . dired-dispatch)))

(global-set-keys
 '(("C-x J" . dired-jump-other-window)))

(provide 'init-dired)
;;; init-dired.el ends here
