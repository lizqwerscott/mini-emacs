;;; init-org.el --- init about org mode some config  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:

;;; Org config
(setopt org-default-notes-file "~/Documents/Org/index.org")

(setopt org-tags-column -80
        org-log-done 'time
        org-catch-invisible-edits 'smart
        org-startup-indented t
        org-startup-folded 'show2levels
        org-pretty-entities nil
        org-hide-emphasis-markers t
        org-link-keep-stored-after-insertion t)

(setopt org-enforce-todo-dependencies t)
(setopt org-todo-keywords
        '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
          (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)")))

(setq org-priority-lowest ?D)

(with-eval-after-load 'org
  (setopt org-format-latex-options
        (plist-put org-format-latex-options :scale 4.0)))

;; yank
(with-eval-after-load 'yank-media
  (add-to-list 'yank-media-preferred-types 'image/tiff))

;;; Org export
(setopt org-export-with-drawers nil
        org-export-with-todo-keywords nil
        org-export-with-toc nil
        org-export-with-smart-quotes t
        org-export-date-timestamp-format "%e %B %Y")

;;; Org babel
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)
     (python . t)
     (latex . t)
     (gnuplot . t)
     (shell . t))))

;;; UI

;; 中文标记隐藏空格
(unless sys/macp
  (font-lock-add-keywords 'org-mode
                          '(("\\cc\\( \\)[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)?\\cc?"
                             (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) ""))))
                            ("\\cc?\\( \\)?[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)\\cc"
                             (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "")))))
                          'append))

(with-eval-after-load 'org-superstar
  (add-list-to-list 'org-superstar-todo-bullet-alist
                    '(("TODO"   . ?☐)
                      ("DOING"  . ?▶)
                      ("HANGUP" . ?⏸)
                      ("CANCEL" . ?✖))))
(setopt org-superstar-special-todo-items t)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

(add-hook 'org-mode-hook
          #'visual-line-mode)

(add-hook 'org-mode-hook
          #'org-modern-indent-mode 90)

;;; menu
(autoload #'transient-org-template "lib-org" nil t)
(autoload #'transient-org-line-template "lib-org" nil t)
(autoload #'transient-org-toggles "lib-org" nil t)

;;; keymap
(with-eval-after-load 'org
  (keymap-binds org-mode-map
    ("C-c TAB" . org-insert-item-auto-checkbox)
    ("M-P" . org-metaup)
    ("M-N" . org-metadown)
    ("M-H" . org-metaleft)
    ("M-L" . org-metaright)

    (("M-RET" "s-<return>") . org-meta-return-auto)
    ("s-P" . org-metaup)
    ("s-N" . org-metadown)
    ("s-H" . org-metaleft)
    ("s-L" . org-metaright)

    ("C-c C-l" . ar/org-insert-link-dwim)

    ("M-g o" . consult-org-heading)

    ("C-c o" . transient-org-toggles)
    ("<" . (lambda ()
             "Insert org template."
             (interactive)
             (if (or (region-active-p) (looking-back "^\s*" (line-beginning-position)))
                 (transient-org-template)
               (self-insert-command 1))))
    (">" . transient-org-line-template)))

(global-bind-keys
 ("C-c c" . org-capture)
 ("C-c a" . org-agenda)

 ("C-c L" . org-store-link)
 ("C-c C-o" . org-open-at-point))

;;; org capture
(with-eval-after-load 'org-capture
  (setq org-capture-templates nil)
  (push '("i" "我的闪念" entry (file+headline "~/Documents/Org/idea.org" "闪念") "* %U - %^{标题} %^g\n  %?\n")
        org-capture-templates)
  (push '("s" "收藏名言" entry (file+headline "~/Documents/Org/quote.org" "名言") "* %U - %^{标题} %^g\n  %?\n")
        org-capture-templates)
  (push '("l" "LNKS" entry (file+headline "~/Documents/Org/lnks.org" "链接") "* [[%^{link-url}][%^{link-description}]] %^g\n:PROPERTIES:\n:LINK-CREATE-TIME: %T\n:END:\n  %?\n")
        org-capture-templates)
  (push '("t" "任务" entry (file+headline "~/Documents/Org/tasks.org" "任务") "* TODO %^{标题} %^g\nSCHEDULED: %^t DEADLINE: %^t \n  %?\n") org-capture-templates)
  (push '("w" "工作任务" entry (file+headline "~/Documents/Org/tasks.org" "工作任务") "* TODO %^{任务名} :work:\nSCHEDULED: %^t DEADLINE: %^t\n  %?\n" ) org-capture-templates))

;;; org agenda
(with-eval-after-load 'org
  (setq org-archive-location "~/Documents/Org/archive.org::* finish-tasks")
  (setq org-refile-targets '(("~/Documents/Org/archive.org" :maxlevel . 1)
                             ("~/Documents/Org/inbox.org" :maxlevel . 1)
                             ("~/Documents/Org/tasks.org" :maxlevel . 4)))
  (add-list-to-list 'org-agenda-files
                    '("~/Documents/Org/idea.org"
                      "~/Documents/Org/quote.org"
                      "~/Documents/Org/tasks.org"
                      "~/Documents/Org/archive.org"
                      "~/Documents/Org/inbox.org")))

(provide 'init-org)
;;; init-org.el ends here.
