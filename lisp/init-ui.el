;;; init-ui.el --- init ui packages                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:

;;; theme
(+lizqwer/load-theme user/night-theme)

;;; Optimization
(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;;; Title
(setq frame-title-format '("Emacs - %b")
      icon-title-format frame-title-format)

(setq default-frame-alist
      '((alpha-background . 100)
        ;; (fullscreen . maximized)
        ))

(setq initial-frame-alist
      '((top . 0.5)
        (left . 0.5)
        (width . 0.9)
        (height . 0.9)
        ;; (fullscreen . maximized)
        ))

(unless (or sys/macp sys/win32p)
  (toggle-frame-fullscreen))

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

;;; highlight
(global-hl-line-mode 1)

(require 'paren)
(setq show-paren-delay 0.2
      show-paren-style 'mixed
      show-paren-context-when-offscreen t)
(add-hook 'after-init-hook #'show-paren-mode)

;;; Highlight uncommitted changes using VC
(require 'diff-hl)
(setq diff-hl-draw-borders nil)
(setq diff-hl-disable-on-remote t)

(custom-set-faces
 '(diff-hl-change ((t (:inherit custom-changed :foreground unspecified :background unspecified))))
 '(diff-hl-insert ((t (:inherit diff-added :background unspecified))))
 '(diff-hl-delete ((t (:inherit diff-removed :background unspecified)))))

(keymap-set diff-hl-command-map "SPC" 'diff-hl-mark-hunk)

(setq-default fringes-outside-margins t)

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
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(unless sys/macp
  (global-diff-hl-mode)
  (global-diff-hl-show-hunk-mouse-mode)
  (add-hook 'dired-mode-hook
            'diff-hl-dired-mode)

  (diff-hl-flydiff-mode))

;;; window
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
