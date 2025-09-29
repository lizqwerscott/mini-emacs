;;; init-meow.el --- init meow package               -*- lexical-binding: t; -*-
;; Copyright (C) 2025  lizqwer scott
;;; Commentary:
;;; Code:

(require 'meow)
(setq meow-keypad-meta-prefix nil)
(setq meow-keypad-leader-dispatch "C-c")
(setq meow-mode-state-list
      '((fundamental-mode . normal)
        (text-mode . normal)
        (prog-mode . normal)
        (conf-mode . normal)
        (helpful-mode . normal)
        (help-mode . normal)
        (compilation-mode . normal)
        (messages-buffer-mode . normal)
        (eww-mode . normal)
        (rg-mode . insert)
        (vterm-mode . insert)
        (Info-mode-hook . motion)))
(setq meow-use-clipboard t)
(setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

(meow-thing-register 'url 'url 'url)
(meow-thing-register 'angle '(pair ("<") (">")) '(pair ("<") (">")))

(require 'mark-comment)
(meow-thing-register 'comment #'mark-comment-inner-of-comment #'mark-comment-inner-of-comment)

(defvar wrap-keymap
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (dolist (k '("(" "[" "{" "<"))
      (define-key map k #'insert-pair))
    map)
  "Keymap for wrap.")

(meow-normal-define-key (cons "\\" wrap-keymap))

(setq meow-char-thing-table '((?\( . round)
                              (?\) . round)
                              (?\g .  string)
                              (?\[ . square)
                              (?\] . square)
                              (?<  . angle)
                              (?>  . angle)
                              (?{  . curly)
                              (?}  . curly)
                              (?s  . symbol)
                              (?d  . defun)
                              (?w  . window)
                              (?l  . line)
                              (?b  . buffer)
                              (?p  . paragraph)
                              (?u . url)
                              (?c . comment)))
(meow-normal-define-key (cons "\\" wrap-keymap))

(keymap-binds goto-map
  ("f" . find-file-at-point))

(defvar-keymap find-map
  :doc "Keymap for find commands."
  "c" #'find-custom-file
  "l" #'find-library
  "v" #'find-variable)

(defalias 'find-map find-map)

(autoload #'project-dispatch "init-project" nil t)
(global-bind-keys
 ("C-c p" . project-dispatch))

(require 'lib-transient)
(pretty-transient-define-prefix transient-toggles ()
  "Toggles menu."
  :transient-non-suffix 'transient--do-stay
  [["Basic"
    ("w" "Sub or super word" toggle-sub-word-or-super-word
     :toggle (lambda () (bound-and-true-p subword-mode)) :transient t)
    ("e" "Electric pair" electric-pair-mode :toggle t :transient t)
    ("s" "Auto save" super-save-mode :toggle t :transient t)]

   ["Highlight"
    ("h l" "Line highlight" global-hl-line-mode :toggle t :transient t)
    ("h p" "Paren highlight" show-paren-mode :toggle t :transient t)
    ("h w" "Whitespace"
     (lambda ()
       (interactive)
       (setq-default show-trailing-whitespace
                     (not show-trailing-whitespace)))
     :toggle (lambda () show-trailing-whitespace) :transient t)
    ("h d" "Rainbow delimiters" rainbow-delimiters-mode :toggle t :transient t)]

   ["Ui"
    ("n" "Line number" display-line-numbers-mode :toggle t :transient t)
    ("d" "Dark theme" +lizqwer/toggle-dark-theme
     :toggle (lambda () (cl-find user/night-theme custom-enabled-themes)) :transient t)
    ("T" "Transparent" +lizqwer/toggle-transparent
     :toggle (lambda ()
               (not (eq (frame-parameter (selected-frame) 'alpha-background) 100)))
     :transient t)
    ("o" "Outline" outli-mode :toggle t :transient t)
    ("m t" "Modeline time" display-time-mode :toggle t :transient t)
    ("m b" "Modeline battery" display-battery-mode :toggle t :transient t)]

   ["Program"
    ("f" "Flymake" flymake-mode :toggle t :transient t)
    ("v" "Diff-hl gutter" global-diff-hl-mode :toggle t :transient t)
    ("M" "Margin gutter" diff-hl-margin-mode :toggle t :transient t)
    ("E" "Debug on error" toggle-debug-on-error
     :toggle (lambda () (default-value 'debug-on-error)) :transient t)
    ("Q" "Debug on quit" toggle-debug-on-quit
     :toggle (lambda () (default-value 'debug-on-quit)) :transient t)]]

  [("q" "Quit" transient-quit-one)])

(global-bind-keys
 ("C-c T" . transient-toggles)
 ("<f6>" . transient-toggles))

(defun meow-setup ()
  "Meow setup functions."
  (meow-leader-define-key
   '("/". meow-keypad-describe-key)
   '("?" . meow-cheatsheet))

  (meow-leader-define-key
   '("1" . delete-other-windows)
   '("2" . split-window-below)
   '("3" . split-window-horizontally)
   '("0" . delete-window))

  (meow-leader-define-key
   '("f" . find-file)
   '("F" . find-file-other-window))

  (meow-leader-define-key
   '("s" . "M-s"))

  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . comment-or-uncomment-region)
   '("s" . meow-delete)
   '("S" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("P" . meow-yank-pop)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("d" . meow-kill)
   '("D" . meow-kill-append)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-cancel-selection)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("y" . meow-save)
   '("Y" . meow-clipboard-save)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))

  (meow-normal-define-key
   '("g" . "M-g")
   (cons "F" find-map))

  (meow-normal-define-key
   '("C-;" . grugru)
   '("Q" . kill-buffer-and-window)
   '("?" . "C-h ?")
   '("/" . consult-ripgrep)))

(meow-setup)
(meow-global-mode 1)

(global-bind-keys
 ("C-y" . meow-clipboard-yank))

(provide 'init-meow)
;;; init-meow.el ends here
