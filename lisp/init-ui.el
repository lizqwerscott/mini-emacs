;;; init-ui.el --- init ui packages                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:

;;; theme
(require 'init-theme)

;;; Optimization
(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;;; Title
(setq frame-title-format '("Emacs - %b")
      icon-title-format frame-title-format)

(setq initial-frame-alist
      '((top . 0.5)
        (left . 0.5)
        (width . 0.9)
        (height . 0.9)))

(defun set-alpha-background (symbol value)
  "Set SYMBOL VALUE.
and update transparent."
  (set-default-toplevel-value symbol value)
  (setf (alist-get 'alpha-background default-frame-alist) value)
  (when-let* ((frame (selected-frame)))
    (set-frame-parameter frame 'alpha-background value)))

(defcustom user/alpha-background 100
  "Default alpha background."
  :group 'user
  :type 'number
  :set #'set-alpha-background)

(defun set-fullscreenp (symbol value)
  "Set SYMBOL VALUE."
  (set-default-toplevel-value symbol value)
  (when value
    (add-list-to-list 'initial-frame-alist
                      '((fullscreen . fullboth))))
  (let* ((frame (selected-frame))
         (fullscreen (frame-parameter frame 'fullscreen)))
    (if value
        (modify-frame-parameters frame `((fullscreen . fullboth) (fullscreen-restore . ,fullscreen)))
      (let ((fullscreen-restore (frame-parameter frame 'fullscreen-restore)))
	    (if (memq fullscreen-restore '(maximized fullheight fullwidth))
	        (set-frame-parameter frame 'fullscreen fullscreen-restore)
	      (set-frame-parameter frame 'fullscreen nil))))))

(defcustom user/start-fullscreenp t
  "Is fullscreen in start."
  :group 'user
  :type 'boolean
  :set #'set-fullscreenp)

;;; Header & mode lines
(require 'init-headerline)
(require 'init-modeline)

;;; Line number
(add-hooks '(prog-mode text-mode conf-mode)
           #'(lambda ()
               (setq display-line-numbers-type 'relative)
               (display-line-numbers-mode 1)))

;;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen nil
      inhibit-startup-message t
      initial-scratch-message "")

;;; Mouse & Smooth Scroll
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 0
      hscroll-step 2
      hscroll-margin 2
      scroll-conservatively 101
      auto-window-vscroll nil
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position 'always)

;; 平滑地进行半屏滚动，避免滚动后recenter操作
(if (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode t)
  (pixel-scroll-mode t))

;; generate from https://patorjk.com/software/taag/#p=display&f=ANSI%20Shadow&t=MiNI%20Emacs
(with-current-buffer (get-buffer-create "*scratch*")
  (insert (format ";;
;; ███╗   ███╗██╗███╗   ██╗██╗    ███████╗███╗   ███╗ █████╗  ██████╗███████╗
;; ████╗ ████║██║████╗  ██║██║    ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
;; ██╔████╔██║██║██╔██╗ ██║██║    █████╗  ██╔████╔██║███████║██║     ███████╗
;; ██║╚██╔╝██║██║██║╚██╗██║██║    ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
;; ██║ ╚═╝ ██║██║██║ ╚████║██║    ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
;; ╚═╝     ╚═╝╚═╝╚═╝  ╚═══╝╚═╝    ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
;;   Loading time : %s
;;   Packages     : %s
;;
"
                  (emacs-init-time)
                  (number-to-string (length package-activated-list)))))

;;; Highlight the current line
(global-hl-line-mode 1)
(add-hooks '(eshell-mode shell-mode term-mode)
           #'(lambda () (setq-local global-hl-line-mode nil)))

;;; Highlight matching parens
(setq show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery nil
      show-paren-delay 0.2)
(setq show-paren-style 'mixed
      show-paren-context-when-offscreen t)
(custom-set-faces
 '(show-paren-match ((t (:foreground "SpringGreen3" :underline t :weight bold)))))

(add-hook 'after-init-hook #'show-paren-mode)

;;; Highlight uncommitted changes using VC
(setopt diff-hl-draw-borders nil
        diff-hl-disable-on-remote t)

(custom-set-faces
 '(diff-hl-change ((t (:inherit custom-changed :foreground unspecified :background unspecified))))
 '(diff-hl-insert ((t (:inherit diff-added :background unspecified))))
 '(diff-hl-delete ((t (:inherit diff-removed :background unspecified)))))

(with-eval-after-load 'diff-hl
  (unless sys/macp
    (setopt diff-hl-update-async t))

  (keymap-set diff-hl-command-map "SPC" 'diff-hl-mark-hunk)
  (keymap-unset diff-hl-command-map "n")
  (setq-default fringes-outside-margins t)

  ;; (diff-hl-flydiff-mode)

  (with-no-warnings
    (defun my-diff-hl-fringe-bmp-function (_type _pos)
      "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
      (define-fringe-bitmap 'my-diff-hl-bmp
        (vector (if sys/linuxp #b11111100 #b11100000))
        1 8
        '(center t)))
    (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)

    (unless (display-graphic-p)
      ;; Fall back to the display margin since the fringe is unavailable in tty
      (diff-hl-margin-mode 1)
      ;; Avoid restoring `diff-hl-margin-mode'
      (with-eval-after-load 'desktop
        (add-to-list 'desktop-minor-mode-table
                     '(diff-hl-margin-mode nil))))

    ;; Integration with magit
    (with-eval-after-load 'magit
      (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))))

(global-diff-hl-mode)
(global-diff-hl-show-hunk-mouse-mode)
(add-hook 'dired-mode-hook
          #'diff-hl-dired-mode)

;;; Highlight brackets according to their depth
(add-hooks '(emacs-lisp-mode lisp-mode)
           #'rainbow-delimiters-mode)

;;; window
;;; ace window
(require 'ace-window)
(add-to-list 'aw-ignored-buffers "*Ilist*")
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(add-hook 'after-init-hook
          #'ace-window-posframe-mode)

(custom-set-faces
 '(aw-leading-char-face ((t (:inherit font-lock-keyword-face :foreground unspecified :bold t :height 3.0))))
 '(aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0))))
 '(aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t)))))

(require 'transient)
(transient-define-prefix window-dispatch ()
  "Window Management."
  :transient-non-suffix 'transient--do-stay
  [["Actions"
    ("TAB" "switch" other-window :transient t)
    ("x" "delete" ace-delete-window :transient t)
    ("X" "delete other" ace-delete-other-windows)
    ("s" "swap" ace-swap-window :transient t)
    ("a" "select" ace-select-window)
    ("m" "maximize" toggle-frame-maximized)
    ("u" "fullscreen" toggle-frame-fullscreen)]
   ["Resize"
    ("h" "←" shrink-window-horizontally :transient t)
    ("j" "↓" enlarge-window :transient t)
    ("k" "↑" shrink-window :transient t)
    ("l" "→" enlarge-window-horizontally :transient t)
    ("n" "balance" balance-windows :transient t)]
   ["Split"
    ("3" "horizontally" split-window-right :transient t)
    ("2" "vertically" split-window-below :transient t)
    ("w 3" "root horizontally" split-root-window-right :transient t)
    ("w 2" "root vertically" split-root-window-below :transient t)
    ("t" "Transpose Window Layout" window-layout-transpose :transient t)
    ("r" "Clockwise Rotate" rotate-windows :transient t)
    ("R" "Counterclockwise Rotate" rotate-windows-back :transient t)]
   ["Zoom"
    ("+" "in" text-scale-increase :transient t)
    ("=" "in" text-scale-increase :transient t)
    ("-" "out" text-scale-decrease :transient t)
    ("0" "reset" (lambda () (interactive) (text-scale-increase 0)) :transient t)]
   ["Misc"
    ("o" "frame font" set-frame-font :transient t)
    ("f" "new frame" make-frame-command :transient t)
    ("d" "delete frame" delete-frame :transient t)
    ("<left>" "winner undo" winner-undo :transient t)
    ("<right>" "winner redo" winner-redo :transient t)
    ("q" "Quit" transient-quit-one)]])

(global-bind-keys
 (("s-o" "M-o") . ace-window)
 ("C-c w" . window-dispatch))

;;; winner mode
(winner-mode 1)
(keymap-binds winner-mode-map
  ("C-c H" . winner-undo)
  ("C-c L" . winner-redo))

(defun my-window-select-fit-size (window)
  (select-window window)
  (fit-window-to-buffer window
                        (floor (frame-height) 3)
                        10))

(setq display-buffer-alist
      '(;; bottom side window
        ((or "\\*.*e?shell\\*" "*ielm*")
         (display-buffer-in-side-window)
         (window-height . 0.25)
         (side . bottom)
         (slot . -1))
        ("\\*\\(Flymake diagnostics\\|xref\\|Completions\\)"
         (display-buffer-in-side-window)
         (window-height . 0.25)
         (side . bottom)
         (slot . 1))
        ("\\*\\(grep\\|find\\)\\*"
         (display-buffer-in-side-window)
         (window-height . 0.25)
         (side . bottom)
         (slot . 2))
        ;; right side window
        ((or "\\*\\([Hh]elp\\)\\*" "\\*\\(helpful.*\\)\\*")
         (display-buffer-in-side-window)
         (window-width . 0.4)
         (side . right)
         (slot . 0))
        ("\\*\\(Ibuffer\\)\\*"
         (display-buffer-in-side-window)
         (window-width . 100)
         (side . right)
         (slot . 1))
        ;; bottom buffer (NOT side window)
        ((or (derived-mode . compilation-mode) (derived-mode . cargo-process-mode))
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-height . 0.3)
         (preserve-size . (t . t))
         (body-function . select-window))
        ((or "\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\|Bookmark List\\|Occur\\|eldoc\\)\\*"
             "\\*\\(Flymake diagnostics\\|xref\\|Completions\\)"
             "Output\\*$"
             "\\*ert\\*$"

             "\\*Async Shell Command\\*$"
             "\\*Apropos\\*$")
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (dedicated . t)
         (window-height . 0.25)
         (body-function . my-window-select-fit-size))))

;;; Another

(which-key-mode)
(global-so-long-mode 1)

;;; Click to browse URL or to send to e-mail address
(add-hook 'text-mode-hook
          'goto-address-mode)

(add-hook 'prog-mode-hook
          'goto-address-prog-mode)

(provide 'init-ui)
;;; init-ui.el ends here.
