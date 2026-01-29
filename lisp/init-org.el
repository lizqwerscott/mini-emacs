;;; init-org.el --- init about org mode some config  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:

;;; Org config
(require 'org)

(require 'lib-org)

(setq org-default-notes-file "~/Documents/Org/index.org")

(setq org-tags-column -80
      org-log-done 'time
      org-catch-invisible-edits 'smart
      org-startup-indented t
      org-startup-folded 'show2levels
      org-pretty-entities nil
      org-hide-emphasis-markers t
      org-link-keep-stored-after-insertion t)

(setq org-enforce-todo-dependencies t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
        (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)")))

(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 4.0))

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
(setq org-superstar-special-todo-items t)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

(add-hook 'org-mode-hook
          #'visual-line-mode)

(add-hook 'org-mode-hook
          #'org-modern-indent-mode 90)

;;; menu
(defun hot-expand (str &optional mod)
  "Expand org template.

STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
  (let (text)
    (when (region-active-p)
      (setq text (buffer-substring (region-beginning) (region-end)))
      (delete-region (region-beginning) (region-end)))
    (insert str)
    (if (fboundp 'org-try-structure-completion)
        (org-try-structure-completion) ; < org 9
      (progn
        ;; New template expansion since org 9
        (require 'org-tempo nil t)
        (org-tempo-complete-tag)))
    (when mod (insert mod) (forward-line))
    (when text (insert text))))

(require 'lib-transient)
(pretty-transient-define-prefix transient-org-template ()
  "Transient org template menu."
  [["Basic"
    ("e" "example" (hot-expand "<e"))
    ("l" "latex" (hot-expand "<l"))
    ("x" "quote" (hot-expand "<q"))
    ("v" "verse" (hot-expand "<v"))
    ("b" "bash" (hot-expand "<s" "bash"))]
   ["Head"
    ("i" "index" (hot-expand "<i"))
    ("I" "INCLUDE" (hot-expand "<I"))
    ("S" "Startup" (insert "#+STARTUP: "))
    ("L" "LaTeX" (hot-expand "<L"))
    ("P" "Latex Preview" (insert "#+STARTUP: latexpreview "))
    ("Mb" "Html Bigblow Theme" (insert "#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-bigblow.setup"))
    ("Mr" "Html Readtheorg Theme" (insert "#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-readtheorg.setup"))
    ("Mn" "Html Normal Css" (insert "#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"http://gongzhitaao.org/orgcss/org.css\"/>"))]
   ["Source"
    ("ss" "src" (hot-expand "<s"))
    ("se" "emacs-lisp" (hot-expand "<s" "emacs-lisp"))
    ("sp" "python" (hot-expand "<s" "python"))
    ("sP" "python" (hot-expand "<s" "python :results output"))
    ("sc" "c++" (hot-expand "<s" "c++"))
    ("sy" "yaml" (hot-expand "<s" "yaml-ts"))]
   ["Misc"
    ("u" "plantuml" (hot-expand "<s" "plantuml :file chart.png"))
    ("G" "gnuplot" (hot-expand "<s" "gnuplot :results output :file ./result.png"))
    ("<" "ins" self-insert-command)]]
  [("q" "Quit" transient-quit-one)])

(pretty-transient-define-prefix transient-org-toggles ()
  "Transient org menu."
  :transient-non-suffix 'transient--do-stay
  [["Display"
    ("l" "Display Link" org-toggle-link-display :toggle (not org-link-descriptive) :transient t)
    ("m" "Hide Emphasis Markers" org-toggle-display-emphasis-markers :toggle org-hide-emphasis-markers :transient t)
    ("e" "Display Pretty Entities" org-toggle-pretty-entities :toggle org-pretty-entities :transient t)
    ("i" "Display inline images" org-toggle-inline-images :toggle org-inline-image-overlays :transient t)]
   ["Org Management"
    ("p" "Set Property" org-set-property)
    ("E" "Export" org-export-dispatch)
    ("L" "List export file" org-list-export-file)]]
  [("q" "Quit" transient-quit-one)])

(defun org-insert-or-surround (open close)
  "Insert or surround text with LaTeX-style delimiters.

If the region is active, wrap the selected text with the delimiters specified by
OPEN and CLOSE. Otherwise, insert the delimiters with space for text in between."
  (if (use-region-p)
      (let ((begin (region-beginning))
            (end (region-end)))
        (save-excursion
          (goto-char begin)
          (insert (format "\\%s " open))
          (goto-char (+ end 3))
          (insert (format " \\%s" close))))
    (insert (format "\\%s  \\%s" open close))
    (backward-char 3)))

(pretty-transient-define-prefix transient-org-line-template ()
  "Transient org line menu."
  [["Link"
    ("l" "Normal" ar/org-insert-link-dwim)]
   ["Emphasize"
    ("=" "Verbatim" (org-emphasize ?=))
    ("~" "Code" (org-emphasize ?=))
    ("+" "Delete" (org-emphasize ?+))
    ("_" "Underline" (org-emphasize ?_))

    ("/" "Italic" (org-emphasize ?/))
    ("*" "Bold" (org-emphasize ?*))
    ("e" "Emphasize" org-emphasize)]
   ["Latex"
    ("i" "Inline math" (org-insert-or-surround "(" ")"))
    ("I" "Display math" (org-insert-or-surround "[" "]"))
    ("L" "Convert to latex" latex-math-from-calc :if region-active-p)]
   ["Misc"
    (">" "ins" self-insert-command)]]
  [("q" "Quit" transient-quit-one)])

;;; keymap
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
  (">" . transient-org-line-template))

(global-bind-keys
 ("C-c c" . org-capture)
 ("C-c a" . org-agenda)

 ("C-c L" . org-store-link)
 ("C-c C-o" . org-open-at-point))

;;; org capture
(setq org-capture-templates nil)
(push '("i" "我的闪念" entry (file+headline "~/Documents/Org/idea.org" "闪念") "* %U - %^{标题} %^g\n  %?\n")
      org-capture-templates)
(push '("s" "收藏名言" entry (file+headline "~/Documents/Org/quote.org" "名言") "* %U - %^{标题} %^g\n  %?\n")
      org-capture-templates)
(push '("l" "LNKS" entry (file+headline "~/Documents/Org/lnks.org" "链接") "* [[%^{link-url}][%^{link-description}]] %^g\n:PROPERTIES:\n:LINK-CREATE-TIME: %T\n:END:\n  %?\n")
      org-capture-templates)
(push '("t" "任务" entry (file+headline "~/Documents/Org/tasks.org" "任务") "* TODO %^{标题} %^g\nSCHEDULED: %^t DEADLINE: %^t \n  %?\n") org-capture-templates)
(push '("w" "工作任务" entry (file+headline "~/Documents/Org/tasks.org" "工作任务") "* TODO %^{任务名} :work:\nSCHEDULED: %^t DEADLINE: %^t\n  %?\n" ) org-capture-templates)

;;; org agenda
(setq org-archive-location "~/Documents/Org/archive.org::* finish-tasks")
(setq org-refile-targets '(("~/Documents/Org/archive.org" :maxlevel . 1)
                           ("~/Documents/Org/inbox.org" :maxlevel . 1)
                           ("~/Documents/Org/tasks.org" :maxlevel . 4)))
(add-list-to-list 'org-agenda-files
                  '("~/Documents/Org/idea.org"
                    "~/Documents/Org/quote.org"
                    "~/Documents/Org/tasks.org"
                    "~/Documents/Org/archive.org"
                    "~/Documents/Org/inbox.org"))

(provide 'init-org)
;;; init-org.el ends here.
