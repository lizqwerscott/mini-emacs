;;; init-org.el --- init about org mode some config  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:

;;; Org config
(require 'org)

(setq org-default-notes-file "~/Documents/Org/index.org")

(setq org-tags-column -80
      org-log-done 'time
      org-catch-invisible-edits 'smart
      org-startup-indented t
      org-startup-folded 'show2levels
      org-pretty-entities nil
      org-hide-emphasis-markers t)

(setq org-enforce-todo-dependencies t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
        (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)")))

(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 4.0))

;;; Org babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C . t)
   (python . t)
   (latex . t)
   (gnuplot . t)
   (shell . t)))

;;; UI

;; 中文标记隐藏空格
(font-lock-add-keywords 'org-mode
                        '(("\\cc\\( \\)[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)?\\cc?"
                           (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) ""))))
                          ("\\cc?\\( \\)?[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)\\cc"
                           (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "")))))
                        'append)

(with-eval-after-load 'org-superstar
  (add-list-to-list 'org-superstar-todo-bullet-alist
                    '(("TODO"   . ?☐)
                      ("DOING"  . ?▶)
                      ("HANGUP" . ?⏸)
                      ("CANCEL" . ?✖))))
(setq org-superstar-special-todo-items t)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

(add-hook 'org-mode-hook
          #'visual-line-mode)

;;; keymap
(keymap-binds org-mode-map
  ("C-c TAB" . "C-c TAB")
  ("M-P" . org-metaup)
  ("M-N" . org-metadown)
  ("M-H" . org-metaleft)
  ("M-L" . org-metaright)

  ("s-<return>" . org-meta-return)
  ("s-P" . org-metaup)
  ("s-N" . org-metadown)
  ("s-H" . org-metaleft)
  ("s-L" . org-metaright)

  ("M-g o" . consult-org-heading))

(provide 'init-org)
;;; init-org.el ends here.
