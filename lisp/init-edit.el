;;; init-edit.el --- init edit package               -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:


;;; Code:

(require 'lib-edit)

;;; can use superword-mode
(add-hook 'prog-mode-hook
          'superword-mode)

;;; electric pair
;; (add-hook 'prog-mode-hook 'electric-pair-local-mode)
;; (add-hook 'conf-mode-hook 'electric-pair-local-mode)
;; (add-hook 'sly-mrepl-hook 'electric-pair-local-mode)
(setq electric-pair-inhibit-predicate #'electric-pair-default-inhibit)
(with-eval-after-load 'elec-pair
  (add-to-list 'electric-pair-pairs '(?\{ . ?\})))

(electric-pair-mode t)

(advice-add #'meow-grab
            :before
            #'(lambda ()
                (call-interactively #'electric-pair-mode)
                ;; (call-interactively #'fingertip-mode)
                ))

;;; puni
(require 'init-puni)

;;; vundo
(require 'vundo)
(setq vundo-glyph-alist vundo-unicode-symbols)
(global-set-key (kbd "C-/") #'vundo)

;;; nxml
(with-eval-after-load 'nxml-mode
  (keymap-binds nxml-mode-map
    ("C-s-f" . nxml-down-element)
    ("C-s-n" . nxml-forward-element)
    ("C-s-p" . nxml-backward-element)
    ("C-s-b" . nxml-backward-up-element)))

;;; Visual Replace
(require 'visual-replace)
(setq visual-replace-min-length 1)

(with-eval-after-load 'transient
  (transient-define-prefix visual-replace-dispatch ()
    "Visual replace menu."
    ["Toggles"
     ("r" "Regexp" visual-replace-toggle-regexp)
     ("s" "Scope" visual-replace-toggle-scope)
     ("q" "Query" visual-replace-toggle-query)
     ("w" "Word" visual-replace-toggle-word)
     ("c" "Case Fold" visual-replace-toggle-case-fold)
     ("l" "Lax ws" visual-replace-toggle-lax-ws)]))

(keymap-set visual-replace-mode-map
            "C-o" #'visual-replace-dispatch)
(global-set-key (kbd "s-r") #'visual-replace)
(global-set-key (kbd "M-r") #'visual-replace)
(visual-replace-global-mode 1)

;;; isearch
(setq isearch-lazy-count t
      isearch-case-fold-search t
      lazy-count-prefix-format "%s/%s "
      search-whitespace-regexp ".*?"
      isearch-repeat-on-direction-change t
      isearch-wrap-pause nil)

;; use selection to search
(defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))

(defun my-occur-from-isearch ()
  "Generate occur from isearch."
  (interactive)
  (let ((query (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string))))
    (isearch-update-ring isearch-string isearch-regexp)
    (let (search-nonincremental-instead)
      (ignore-errors (isearch-done t t)))
    (occur query)))

(defun my-isearch-consult-line-from-isearch ()
  "Invoke `consult-line' from isearch."
  (interactive)
  (let ((query (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string))))
    (isearch-update-ring isearch-string isearch-regexp)
    (let (search-nonincremental-instead)
      (ignore-errors (isearch-done t t)))
    (consult-line query)))

(defun isearch-yank-thing-at-point ()
  "Isearch yank thing at point."
  (interactive)
  (if-let* ((word (thing-at-point 'word)))
      (isearch-yank-string word)
    (if-let* ((symbol (thing-at-point 'symbol)))
        (isearch-yank-string word))))

(defun isearch-yank-thing-at-point-and-forward ()
  "Isearch yank thing at point and isearch forward."
  (interactive)
  (when (isearch-yank-thing-at-point)
    (isearch-repeat-forward)))

(with-eval-after-load 'isearch
  (keymap-binds isearch-mode-map
    ("<escape>" . isearch-exit)
    ("s-r" . isearch-toggle-regexp)
    ("s-e" . isearch-edit-string)

    ("C-w" . isearch-yank-thing-at-point-and-forward)

    ("C-v" . visual-replace-from-isearch)
    ("C-o" . my-occur-from-isearch)
    ("C-j" . my-isearch-consult-line-from-isearch)))

;; repeat for isearch
(defvar-keymap isearch-repeat-map
  :repeat t
  "s" #'isearch-repeat-forward
  "r" #'isearch-repeat-backward)

;;; rg
(with-eval-after-load 'rg
  (keymap-binds rg-mode-map
    ("s-n" . compilation-next-error)
    ("s-p" . compilation-previous-error)
    ("s-N" . rg-next-file)
    ("s-P" . rg-prev-file)))

(global-bind-keys
 ("M-s s" . rg-dwim)
 ("M-s R" . rg-menu)
 ("M-s r" . rg)
 ("M-s t" . rg-literal)
 ("M-s p" . rg-project))

;;; grugru
(grugru-default-setup)

(grugru-define-multiple
  ((c-ts-mode c++-ts-mode)
   (non-alphabet "&&" "||")
   (non-alphabet "&" "|")
   (non-alphabet "+" "-")
   (non-alphabet "*" "/" "%")
   (non-alphabet ">>" "<<")
   (non-alphabet "+=" "-=")
   (non-alphabet "*=" "/=" "%=")
   (non-alphabet "&=" "|=" "^=")
   (non-alphabet ">>=" "<<=")
   (non-alphabet "++" "--")
   (non-alphabet "==" "!=")
   (non-alphabet "<" "<=" ">" ">=")
   (symbol "break" "continue")
   (symbol "signed" "unsigned"))
  (c++-ts-mode
   (symbol "true" "false")
   (symbol "vector" "array" "deque")
   (symbol "class" "struct")
   (symbol "float" "double")
   (symbol "private" "public" "protected")
   (symbol "pair" "tuple")
   (symbol "static_cast" "dynamic_cast" "reinterpret_cast" "const_cast"))
  ((python-mode python-ts-mode)
   (symbol "True" "False"))
  ((emacs-lisp-mode lisp-mode)
   (symbol "when-let" "if-let")))

;;; keymap
(keymap-unset ctl-x-map "C-k")
(keymap-unset ctl-x-map "C-x")

(global-bind-keys
 ("RET" . newline-and-indent)
 ("C-<return>" . comment-indent-new-line)
 (("s-n" "M-n") . scroll-up-1/3)
 (("s-p" "M-p") . scroll-down-1/3)
 (("M-N" "s-N") . scroll-other-window-up-1/3)
 (("M-P" "s-P") . scroll-other-window-down-1/3)
 ("C-s-f" . forward-sexp)
 ("C-s-b" . backward-sexp)

 ("C-x C-n" . next-buffer)
 ("C-x C-p" . previous-buffer)

 ("C-x k" . kill-current-buffer)
 ("C-x K" . kill-buffer)

 ("C-x x b" . browse-this-file)
 ("C-x x c" . clone-indirect-buffer-other-window)

 ("M-g P" . goto-percent))

;; repeat for scroll up
(defvar-keymap scroll-repeat-map
  :repeat t
  "n" #'scroll-up-1/3
  "p" #'scroll-down-1/3)

(defvar-keymap scroll-other-window-repeat-map
  :repeat t
  "n" #'scroll-other-window-up-1/3
  "p" #'scroll-other-window-down-1/3)

(keymap-binds buffer-navigation-repeat-map
  ("n" . next-buffer)
  ("p" . previous-buffer))

(provide 'init-edit)
;;; init-edit.el ends here.
