;;; init-program.el --- init program package         -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:


;;; Code:

;;; outli
(setq outli-allow-indented-headlines t)
(add-hook 'prog-mode-hook
          #'outli-mode)

(with-hook outline-minor-mode
  (meow-normal-define-key
   '("@" . "C-c @")))

;;; Tree-sitter
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

(global-bind-keys
 ("M-g r" . xref-find-references)
 ("M-g d" . xref-find-definitions)
 ("M-g D" . xref-find-definitions-other-window)

 ("C-o" . xref-go-back))

;;; flymake
(defun my-elisp-flymake-byte-compile (fn &rest args)
  "Wrapper for `elisp-flymake-byte-compile'."
  (let ((elisp-flymake-byte-compile-load-path
         (append elisp-flymake-byte-compile-load-path load-path)))
    (apply fn args)))
(advice-add 'elisp-flymake-byte-compile :around #'my-elisp-flymake-byte-compile)

(setq flymake-no-changes-timeout nil
      flymake-fringe-indicator-position 'right-fringe
      flymake-margin-indicator-position 'right-margin)

(setq flymake-indicator-type 'margins
      flymake-margin-indicators-string
      `((error "!" compilation-error) ;; Alternatives: », E, W, i, !, ?)
        (warning "?" compilation-warning)
        (note "i" compilation-info)))

(add-hook 'prog-mode-hook
          #'flymake-mode)

(setq flymake-popon-width 80)

(custom-set-faces
 '(flymake-popon ((t :inherit default :height 0.85)))
 `(flymake-popon-posframe-border ((t :foreground ,(face-background 'posframe-border nil t)))))

(add-hook #'flymake-mode-hook
          #'flymake-popon-mode)

(global-bind-keys
 ("C-c j d" . consult-flymake))

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
          (alist-get 'right-fringe eldoc-box-frame-parameters) 8)

    (global-bind-keys
     ("C-h ?" . eldoc-box-help-at-point))))

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

(with-eval-after-load 'compile
  (keymap-binds compilation-mode-map
    ("s-n" . compilation-next-error)
    ("s-p" . compilation-previous-error)))

;;; eshell
(require 'init-eshell)

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
                         user-emacs-directory))
      yas-buffer-local-condition #'meow-insert-mode-p)

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

(keymap-binds eglot-mode-map
  ("C-c j r" . eglot-rename)
  ("C-c j R" . eglot-restart)
  ("C-c j a" . eglot-code-actions)

  ("M-g u" . eglot-find-implementation))

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

(defun format-code-buffer ()
  "Format now buffer."
  (interactive)
  (save-buffer)
  (call-interactively #'apheleia-format-buffer)
  (revert-buffer t t))

(global-bind-keys
 ("C-c j f" . format-code-buffer))

;;; language
(add-to-list 'auto-mode-alist '("\\.launch$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.urdf\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode))

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

(with-eval-after-load 'cc-mode
  (autoload #'insert-trailing-semi-and-indent "insert-trailing-semi" nil t)
  (keymap-binds (c-mode-map c++-mode-map)
    (";" . insert-trailing-semi-and-indent)))

(with-eval-after-load 'c-ts-mode
  (autoload #'insert-trailing-semi-and-indent "insert-trailing-semi" nil t)
  (keymap-binds (c-ts-mode-map c++-ts-mode-map)
    (";" . insert-trailing-semi-and-indent)))

;;; elisp
(defun eval-buffer-and-message ()
  "Eval elisp buffer and message finish."
  (interactive)
  (eval-buffer)
  (message "Eval buffer finish!"))

(setopt elisp-fontify-semantically t
        elisp-add-help-echo nil)

(keymap-binds (emacs-lisp-mode-map lisp-interaction-mode-map)
  ("C-c r" . eval-buffer-and-message)
  ("C-c C-p" . ielm)
  ("C-h ?" . helpful-at-point))

;;; python
(with-eval-after-load 'eglot
  (defun random-hex-string (n)
    "Generate random N len hex string."
    (let ((str ""))
      (dotimes (_ n str)
        (setq str (format "%s%02x" str (random 256))))))

  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode) . ,(lambda (_interactive _project)
                                                   (list "basedpyright-langserver"
                                                         "--stdio"
                                                         (format "--cancellationReceive=file:%s"
                                                                 (random-hex-string 21))))))

  (setq-default eglot-workspace-configuration
                '(:basedpyright (:typeCheckingMode "basic"))))

(when (executable-find "ty")
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((python-mode python-ts-mode) . ("ty" "server")))))

;;; bash
(add-hook 'sh-mode-hook #'(lambda () (treesit-parser-create 'bash)))

;;; json
(add-hook 'json-mode-hook #'(lambda () (treesit-parser-create 'json)))
(setq json-ts-mode-indent-offset 4)

;;; yaml
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))

;;; markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setopt markdown-fontify-code-blocks-natively t)

(provide 'init-program)
;;; init-program.el ends heres.
