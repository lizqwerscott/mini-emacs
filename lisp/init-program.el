;;; init-program.el --- init program package         -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:


;;; Code:

;; Tree-sitter
(require 'treesit)
(customize-set-variable 'treesit-font-lock-level 4)

(treesit-font-lock-recompute-features
 '(command string variable function operator bracket keyword))

;;; Xref
(setq xref-show-xrefs-function 'consult-xref)
(setq xref-show-definitions-function 'consult-xref)

;; Use faster search tool
(when (executable-find "rg")
  (setq xref-search-program 'ripgrep))

(global-set-keys
 '(("M-g r" . xref-find-references)
   ("M-g d" . xref-find-definitions)
   ("M-g D" . xref-find-definitions-other-window)

   ("C-o" . xref-go-back)))

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

(global-set-keys
 '(("C-c j d" . consult-flymake)))

;;; eldoc
(with-eval-after-load 'eldoc
  (when (childframe-workable-p)
    (require 'eldoc-box)
    (setq eldoc-box-lighter nil
          eldoc-box-only-multi-line t
          eldoc-box-clear-with-C-g t)

    (defface posframe-border
      `((t (:inherit region)))
      "Face used by the `posframe' border."
      :group 'posframe)

    (custom-set-faces
     '(eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
     '(eldoc-box-body ((t (:inherit tooltip)))))

    ;; (add-hook 'eglot-managed-mode-hook
    ;;           #'eldoc-box-hover-at-point-mode)

    (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
          (alist-get 'right-fringe eldoc-box-frame-parameters) 8)))

;;; complile
(setq compilation-scroll-output nil)
(setq compilation-auto-jump-to-first-error nil)
(setq compilation-max-output-line-length nil)

(defun get-first-compilation-error ()
  (when (compilation-buffer-p (current-buffer))
    (compilation--ensure-parse (point-min))
    (save-excursion
      (goto-char (point-min))
      (condition-case err
          (progn
            (compilation-next-error 1)
            (> (point)
               (point-min)))
        (error
         nil)))))

(defun ar/compile-autoclose-or-jump-first-error (buffer string)
  "Hide successful builds window with BUFFER and STRING."
  (with-current-buffer buffer
    (when (eq major-mode 'compilation-mode)
      (if (or (string-match "^.*warning.*" string)
              (get-first-compilation-error)
              (string-match ".*exited abnormally.*" string))
          (progn
            (message "Compilation %s" string)
            (goto-char (point-min))
            (call-interactively #'compilation-next-error))
        (message "Build finished :)")
        (run-with-timer 1 nil
                        (lambda ()
                          (when-let* ((multi-window (> (count-windows) 1))
                                      (live (buffer-live-p buffer))
                                      (window (get-buffer-window buffer t)))
                            (delete-window window))))))))

(setq compilation-finish-functions (list #'ar/compile-autoclose-or-jump-first-error))
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(defun not-split-window (orig-fn &rest args)
  "Let ORIG-FN not split window.
ARGS is ORIG-FN args."
  (let ((split-height-threshold nil)
        (split-width-threshold nil))
    (apply orig-fn args)))

(advice-add #'next-error-no-select :around #'not-split-window)
(advice-add #'previous-error-no-select :around #'not-split-window)
(advice-add #'compile-goto-error :around #'not-split-window)

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

;;; keymap
(defun eglot-restart ()
  "Restart eglot."
  (interactive)
  (call-interactively #'eglot-shutdown)
  (call-interactively #'eglot))

(keymap-sets eglot-mode-map
  '(("C-c j r" . eglot-rename)
    ("C-c j R" . eglot-restart)
    ("C-c j a" . eglot-code-actions)

    ("M-g u" . eglot-find-implementation)))

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

(global-set-keys
 '(("C-c j f" . apheleia-format-buffer)))

;;; language
(add-to-list 'auto-mode-alist '("\\.launch$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.urdf\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

(add-list-to-list 'major-mode-remap-alist
                  '((sh-mode . bash-ts-mode)
                    (rust-mode . rust-ts-mode)
                    (python-mode . python-ts-mode)
                    (c++-mode . c++-ts-mode)
                    (c-mode . c-ts-mode)
                    (go-mode . go-ts-mode)
                    (csharp-mode . csharp-ts-mode)
                    (conf-toml-mode . toml-ts-mode)
                    (js-json-mode . json-ts-mode)))

(add-hook 'json-mode-hook #'(lambda () (treesit-parser-create 'json)))
(add-hook 'ielm-mode-hook #'(lambda () (treesit-parser-create 'elisp)))

(add-hook 'lisp-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
(add-hook 'emacs-lisp-mode-hook #'(lambda () (treesit-parser-create 'elisp)))

(add-hook 'sh-mode-hook #'(lambda () (treesit-parser-create 'bash)))

;;; c++
;; config c++ style
(setq c-default-style "linux"
      c-basic-offset 4
      c-ts-mode-indent-offset 4)

(add-to-list 'auto-mode-alist '("\\CMakeLists.txt\\'" . cmake-ts-mode))

(add-hook 'c-mode-hook
          #'(lambda ()
              (c-toggle-auto-hungry-state)))

(add-hook 'c++-mode-hook
          #'(lambda ()
              (c-toggle-auto-hungry-state)))

;; elisp
(keymap-sets emacs-lisp-mode-map
  '(("C-c r" . eval-buffer)))

(keymap-sets (emacs-lisp-mode-map lisp-interaction-mode-map)
  '(("C-c C-p" . ielm)))

(provide 'init-program)
;;; init-program.el ends heres.
