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

;;; bookmark

(defun my-bookmark-bmenu--revert ()
  "Re-populate `tabulated-list-entries'."
  (let (entries)
    (dolist (full-record (bookmark-maybe-sort-alist))
      (let* ((name       (bookmark-name-from-full-record full-record))
             (annotation (bookmark-get-annotation full-record))
             (location   (bookmark-location full-record))
             (file       (file-name-nondirectory location))
             (type       (let ((fmt "%-8.8s"))
                           (cond ((null location)
                                  (propertize (format fmt "NOFILE") 'face 'warning))
                                 ((file-remote-p location)
                                  (propertize (format fmt "REMOTE") 'face 'mode-line-buffer-id))
                                 ((not (file-exists-p location))
                                  (propertize (format fmt "NOTFOUND") 'face 'error))
                                 ((file-directory-p location)
                                  (propertize (format fmt "DIRED") 'face 'warning))
                                 (t (propertize (format fmt "FILE") 'face 'success))))))
        (push (list
               full-record
               `[,(if (and annotation (not (string-equal annotation "")))
                      "*" "")
                 ,(if (display-mouse-p)
                      (propertize name
                                  'font-lock-face 'bookmark-menu-bookmark
                                  'mouse-face 'highlight
                                  'follow-link t
                                  'help-echo "mouse-2: go to this bookmark in other window")
                    name)
                 ,type
                 ,@(if bookmark-bmenu-toggle-filenames
                       (list (propertize location 'face 'completions-annotations)))])
              entries)))
    (tabulated-list-init-header)
    (setq tabulated-list-entries entries))
  (tabulated-list-print t))
(advice-add #'bookmark-bmenu--revert :override #'my-bookmark-bmenu--revert)

(defun my-bookmark-bmenu-list ()
  "Display a list of existing bookmarks.
The list is displayed in a buffer named `*Bookmark List*'.
The leftmost column displays a D if the bookmark is flagged for
deletion, or > if it is flagged for displaying."
  (interactive)
  (bookmark-maybe-load-default-file)
  (let ((buf (get-buffer-create bookmark-bmenu-buffer)))
    (if (called-interactively-p 'interactive)
        (pop-to-buffer buf)
      (set-buffer buf)))
  (bookmark-bmenu-mode)
  (bookmark-bmenu--revert))
(advice-add #'bookmark-bmenu-list :override #'my-bookmark-bmenu-list)

(define-derived-mode bookmark-bmenu-mode tabulated-list-mode "Bookmark Menu"
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq tabulated-list-format
        `[("" 1) ;; Space to add "*" for bookmark with annotation
          ("Bookmark" ,bookmark-bmenu-file-column bookmark-bmenu--name-predicate)
          ("Type" 9)
          ,@(if bookmark-bmenu-toggle-filenames
                '(("File" 0 bookmark-bmenu--file-predicate)))])
  (setq tabulated-list-padding bookmark-bmenu-marks-width)
  (setq tabulated-list-sort-key '("Bookmark" . nil))
  (add-hook 'tabulated-list-revert-hook #'bookmark-bmenu--revert nil t)'
  (setq revert-buffer-function #'bookmark-bmenu--revert)
  (tabulated-list-init-header))

(provide 'init-tools)
;;; init-tools.el ends here
