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
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode t)

(require 'hungry-delete)
(setq hungry-delete-chars-to-skip " \t\f\v"
      hungry-delete-except-modes
      '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode))
(global-hungry-delete-mode t)

;;; fingertip
(require 'init-fingertip)

;;; vundo
(require 'vundo)
(setq vundo-glyph-alist vundo-unicode-symbols)
(global-set-key (kbd "C-/") #'vundo)

;;; nxml
(with-eval-after-load 'nxml-mode
  (keymap-sets nxml-mode-map
    '(("C-s-f" . nxml-down-element)
      ("C-s-n" . nxml-forward-element)
      ("C-s-p" . nxml-backward-element)
      ("C-s-b" . nxml-backward-up-element))))

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
      search-whitespace-regexp ".*?")

(defun my-occur-from-isearch ()
  (interactive)
  (let ((query (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string))))
    (isearch-update-ring isearch-string isearch-regexp)
    (let (search-nonincremental-instead)
      (ignore-errors (isearch-done t t)))
    (occur query)))

(with-eval-after-load 'isearch
  (keymap-sets isearch-mode-map
    '(("<escape>" . isearch-exit)
      ("C-o" . my-occur-from-isearch))))

;;; rg
(with-eval-after-load 'rg
  (keymap-sets rg-mode-map
    '(("s-n" . compilation-next-error)
      ("s-p" . compilation-previous-error)
      ("s-N" . rg-next-file)
      ("s-P" . rg-prev-file))))

(global-set-keys
 '(("M-s s" . rg-dwim)
   ("M-s R" . rg-menu)
   ("M-s r" . rg)
   ("M-s t" . rg-literal)
   ("M-s p" . rg-project)))

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

(provide 'init-edit)
;;; init-edit.el ends here.
