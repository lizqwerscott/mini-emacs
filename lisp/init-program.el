;;; init-program.el --- init program package         -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:


;;; Code:

;;; Xref
(setq xref-show-xrefs-function #'xref-show-definitions-completing-read)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

;;; flymake
(add-hook 'prog-mode-hook
          #'flymake-mode)

(setq flymake-show-diagnostics-at-end-of-line nil
      flymake-show-diagnostics-at-end-of-line 'short
      flymake-indicator-type 'margins
      flymake-margin-indicators-string
      `((error "!" compilation-error) ;; Alternatives: », E, W, i, !, ?)
        (warning "?" compilation-warning)
        (note "i" compilation-info)))

(setq flymake-no-changes-timeout nil
      flymake-fringe-indicator-position 'right-fringe)

;;; complile
(setq compilation-scroll-output nil)
(setq compilation-auto-jump-to-first-error nil)
(setq compilation-max-output-line-length nil)

(defun ar/compile-autoclose-or-jump-first-error (buffer string)
  "Hide successful builds window with BUFFER and STRING."
  (when (with-current-buffer buffer
          (eq major-mode 'compilation-mode))
    (if (and (string-match "finished" string)
           (not (string-match "^.*warning.*" string)))
        (progn
          (message "Build finished :)")
          (run-with-timer 1 nil
                          (lambda ()
                            (when-let* ((multi-window (> (count-windows) 1))
                                        (live (buffer-live-p buffer))
                                        (window (get-buffer-window buffer t)))
                              (delete-window window)))))
      (progn
        (message "Compilation %s" string)
        (call-interactively #'compilation-next-error)))))

(setq compilation-finish-functions (list #'ar/compile-autoclose-or-jump-first-error))
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

;;; lisp
(add-hook 'before-save-hook
          #'(lambda ()
              (when (and (equal major-mode 'elisp-mode)
                       (equal major-mode 'lisp-mode))
                (call-interactively #'check-parens))))

;;; snippet
(setq yas-snippet-dirs
      (list
       (expand-file-name "config/yasnippet/snippets/"
                         user-emacs-directory)))

(require 'yasnippet)
(yas-global-mode 1)

;;; citre
(require 'citre-config)
(require 'citre)
(setq citre-auto-enable-citre-mode-modes '(prog-mode))
(setq citre-ctags-program "/usr/bin/ctags")
(add-hook 'find-file-hook #'citre-auto-enable-citre-mode)

(add-hook #'eglot-managed-mode-hook
          #'(lambda ()
              (when citre-mode
                (setq-local xref-backend-functions
                            '(citre-xref-backend
                              t)))))

;;; eglot
(setq read-process-output-max (* 1024 1024)) ; 1MB
(setq eglot-autoshutdown t
      eglot-events-buffer-size 0
      eglot-send-changes-idle-time 0.5
      eglot-events-buffer-config '(:size 0 :format full)
      eglot-prefer-plaintext t
      jsonrpc-event-hook nil
      eglot-code-action-indications nil ;; EMACS-31 -- annoying as hell
      )

(fset #'jsonrpc--log-event #'ignore)

(require 'eglot)

(setq eglot-ignored-server-capabilities
      '(:inlayHintProvider
        :documentHighlightProvider
        :documentFormattingProvider
        :documentRangeFormattingProvider
        :documentOnTypeFormattingProvider
        :colorProvider
        :foldingRangeProvider
        ;; :hoverProvider
        ))

(setq eglot-stay-out-of
      '(imenu))

(add-hook 'prog-mode-hook
          #'(lambda ()
              (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode 'json-ts-mode)
                (eglot-ensure))))

;; Emacs LSP booster
(when (executable-find "emacs-lsp-booster")
  (eglot-booster-mode 1))

;;; format
(require 'apheleia)

(setf (alist-get 'isort apheleia-formatters)
      '("isort" "--stdout" "-"))

(setf (alist-get 'python-ts-mode apheleia-mode-alist)
      '(isort black))

(setf (alist-get 'rust-ts-mode apheleia-mode-alist)
      'cargo-fmt)

(setf (alist-get 'cargo-fmt apheleia-formatters)
      '("cargo" "fmt"))

;;; language
;; config c++ style
(setq c-default-style "linux"
      c-basic-offset 4)

(setf c-ts-mode-indent-offset 4)

(add-hook 'c-mode-hook
          #'(lambda ()
              (c-toggle-auto-hungry-state)))

(add-hook 'c++-mode-hook
          #'(lambda ()
              (c-toggle-auto-hungry-state)))


(provide 'init-program)
;;; init-program.el ends heres.
