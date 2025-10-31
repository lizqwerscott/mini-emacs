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
(keymap-set icomplete-minibuffer-map "C-j" #'icomplete-fido-exit)

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
(when (equal user/minibuffer 'mct)
  (require 'mct)
  (mct-mode))

;;; vertico
(when (equal user/minibuffer 'vertico)
  (require 'vertico)

  (setq vertico-count 15)

  (defun my/vertico-insert ()
    (interactive)
    (let* ((mb (minibuffer-contents-no-properties))
           (lc (if (string= mb "") mb (substring mb -1))))
      (cond ((string-match-p "^[/~:]" lc) (self-insert-command 1 ?/))
            ((file-directory-p (vertico--candidate)) (vertico-insert))
            (t (self-insert-command 1 ?/)))))

  ;; Configure the display per command.
  ;; Use a buffer with indices for imenu
  ;; and a flat (Ido-like) menu for M-x.
  (setopt vertico-multiform-commands
          '(;; (consult-imenu buffer indexed)
            ))

  ;; Configure the display per completion category.
  ;; Use the grid display for files and a buffer
  ;; for the consult-grep commands.
  (setq vertico-multiform-categories
        '(;; (file grid)
          ;; (project-file grid)
          (consult-grep buffer)))

  ;; highlight
  (defvar +vertico-transform-functions nil)

  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
    (dolist (fun (ensure-list +vertico-transform-functions))
      (setq cand (funcall fun cand)))
    (cl-call-next-method cand prefix suffix index start))

  (defun +vertico-highlight-directory (file)
    "If FILE ends with a slash, highlight it as a directory."
    (if (string-suffix-p "/" file)
        (propertize file 'face 'marginalia-file-priv-dir) ; or face 'dired-directory
      file))

  ;; function to highlight enabled modes similar to counsel-M-x
  (defun +vertico-highlight-enabled-mode (cmd)
    "If MODE is enabled, highlight it as font-lock-constant-face."
    (let ((sym (intern cmd)))
      (if (or (eq sym major-mode)
              (and
               (memq sym minor-mode-list)
               (boundp sym)))
          (propertize cmd 'face 'font-lock-constant-face)
        cmd)))

  ;; add-to-list works if 'file isn't already in the alist
  ;; setq can be used but will overwrite all existing values
  ;; (add-to-list 'vertico-multiform-categories
  ;;              '(file
  ;;                ;; this is also defined in the wiki, uncomment if used
  ;;                ;; (vertico-sort-function . vertico-sort-directories-first)
  ;;                (+vertico-transform-functions . +vertico-highlight-directory)))
  (add-to-list 'vertico-multiform-commands
               '(execute-extended-command
                 (+vertico-transform-functions . +vertico-highlight-enabled-mode)))

  (defvar +vertico-current-arrow t)


  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((and +vertico-current-arrow
                                                   (not (bound-and-true-p vertico-flat-mode)))
                                              (eql t)))
    (setq cand (cl-call-next-method cand prefix suffix index start))
    (let ((arrow (propertize (if (char-displayable-p ?») "» " "> ") 'face 'font-lock-keyword-face)))
      (unless (bound-and-true-p vertico-grid-mode)
        (if (= vertico--index index)
           (concat arrow cand)
         (concat "  " cand)))))

  ;; Configure directory extension.
  (keymap-binds vertico-map
    ("RET" . vertico-directory-enter)
    ("DEL" . vertico-directory-delete-char)
    ("M-DEL" . vertico-directory-delete-word)
    ("C-<backspace>" . delete-backward-char)
    ("C-j" . vertico-exit-input)
    ("/" . my/vertico-insert))

  (add-hook #'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  (vertico-mode 1)

  ;; Enable vertico-multiform
  (vertico-multiform-mode))

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

;;; consult
(require 'consult)

;; use narrow
(setq consult-narrow-key "<")
;; not auto preview
(setq consult-preview-key "C-o")

(add-hook 'completion-list-mode-hook 'consult-preview-at-point-mode)

(defun buffer-list-filter ()
  "Get buffer list with filter."
  (let ((buffers (buffer-list))
        (res))
    (dolist (buffer buffers)
      (unless (string-match-p "*help" (buffer-name buffer))
        (push buffer res)))
    res))

(setq consult-buffer-list-function #'buffer-list-filter)

;;;###autoload
(defun consult-fd-dir ()
  (interactive)
  (let ((consult-fd-args (append consult-fd-args
                                 (list
                                  "--type directory"))))
    (consult-fd "~/")))

(defun consult-buffer-with-target (target &optional sources)
  "Enhanced `switch-to-buffer' command with support for virtual buffers.

TARGET is consult buffer target place.

The command supports recent files, bookmarks, views and project files as
virtual buffers.  Buffers are previewed.  Narrowing to buffers (b), files (f),
bookmarks (m) and project files (p) is supported via the corresponding
keys.  In order to determine the project-specific files and buffers, the
`consult-project-function' is used.  The virtual buffer SOURCES
default to `consult-buffer-sources'.  See `consult--multi' for the
configuration of the virtual buffer sources."
  (interactive)
  (let ((selected (consult--multi (or sources consult-buffer-sources)
                                  :require-match
                                  (confirm-nonexistent-file-or-buffer)
                                  :prompt (format "Switch to in other %s: "
                                                  (if target
                                                      (symbol-name target)
                                                    ""))
                                  :history 'consult--buffer-history
                                  :sort nil)))
    ;; For non-matching candidates, fall back to buffer creation.
    (unless (plist-get (cdr selected) :match)
      (consult--buffer-action (car selected)))))

(defun consult-buffer-other-window ()
  "Variant of `consult-buffer', switching to a buffer in another window."
  (interactive)
  (let ((consult--buffer-display #'switch-to-buffer-other-window))
    (consult-buffer-with-target 'window)))

;; meow while translate i into TAB
(keymap-unset goto-map "TAB")
(global-bind-keys
 ("C-x C-r" . consult-recent-file)

 ("M-y" . consult-yank-pop)

 ("C-c b" . consult-buffer)
 ("C-c B" . consult-buffer-other-window)

 ("M-g l" . consult-goto-line)
 ("M-g o" . consult-outline)
 ("M-g m" . consult-mark)
 ("M-g k" . consult-global-mark)
 ("M-g e" . consult-compile-error)
 ("M-g i" . consult-imenu)
 ("M-g I" . consult-imenu-multi)
 ("M-g b" . consult-bookmark)

 ("M-s l" . consult-line)
 ("M-s L" . consult-line-multi)
 ("M-s u" . consult-isearch-history)
 ("M-s f" . ("Search file" . consult-fd))
 ("M-s d" . ("Search dir" . consult-fd-dir)))

;; consult dir
(require 'consult-dir)
;; A function that returns a list of directories
(defun consult-dir--quick-dir ()
  "Return list of fasd dirs."
  (list "~" "~/Downloads/" "~/Documents/" "~/MyProject/" "~/github/"))

;; A consult source that calls this function
(defvar consult-dir--source-quick
  `(
    :name     "quick"
    :narrow   ?q
    :category file
    :face     consult-file
    :history  file-name-history
    ;; :enabled  t
    :items    ,#'consult-dir--quick-dir)
  "Fasd directory source for `consult-dir'.")

;; Adding to the list of consult-dir sources
(add-to-list 'consult-dir-sources 'consult-dir--source-quick)

(global-bind-keys
 ("C-x C-d" . consult-dir))

(keymap-binds minibuffer-local-map
  ("M-s" . consult-history)
  ("M-r" . consult-history)
  ("C-i" . (lambda ()
             "Insert the currunt symbol."
             (interactive)
             (insert (save-excursion
                       (set-buffer (window-buffer (minibuffer-selected-window)))
                       (or (thing-at-point 'symbol t) "")))))
  ("C-x C-d" . consult-dir)
  ("C-x C-j" . consult-dir-jump-file))

(global-bind-keys
 (("s-x" "M-x") . execute-extended-command))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here.
