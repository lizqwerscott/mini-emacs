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

;; meow while translate i into TAB
(keymap-unset goto-map "TAB")
(global-set-keys
 '(("C-x C-r" . consult-recent-file)
   
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
   ("M-s d" . ("Search dir" . consult-fd-dir))))

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

(global-set-keys
 '(("C-x C-d" . consult-dir)))

(keymap-sets minibuffer-local-map
  '(("M-s" . consult-history)
    ("M-r" . consult-history)
    ("C-i" . (lambda ()
               "Insert the currunt symbol."
               (interactive)
               (insert (save-excursion
                         (set-buffer (window-buffer (minibuffer-selected-window)))
                         (or (thing-at-point 'symbol t) "")))))
    ("C-x C-d" . consult-dir)
    ("C-x C-j" . consult-dir-jump-file)))

(global-set-keys
 '((("s-x" "M-x") . execute-extended-command)))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here.
