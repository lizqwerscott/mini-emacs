(when (file-exists-p custom-file)
  (load custom-file))

(require 'init-mode)
(require 'init-super-save)

(require 'init-ui)

(require 'init-key)
(require 'init-meow)

(require 'init-edit)

;;; minibuffer
(fido-vertical-mode t)
(marginalia-mode t)

(keymap-set icomplete-fido-mode-map "s-j" #'icomplete-fido-exit)

(setq completion-styles '(fussy basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles basic partial-completion))))

(setq tab-always-indent 'complete
      icomplete-delay-completions-threshold 0
      icomplete-compute-delay 0
      icomplete-show-matches-on-no-input t
      icomplete-hide-common-prefix nil
      icomplete-prospects-height 9
      icomplete-separator " . "
      icomplete-with-completion-tables t
      icomplete-in-buffer t
      icomplete-max-delay-chars 0
      icomplete-scroll t
      resize-mini-windows 'grow-only
      icomplete-matches-format nil)
(bind-key "TAB" #'icomplete-force-complete icomplete-minibuffer-map)
(bind-key "RET" #'icomplete-force-complete-and-exit icomplete-minibuffer-map)
(add-hook 'icomplete-minibuffer-setup-hook
          (lambda ()
            (setq-local max-mini-window-height 0.5)))

(with-eval-after-load 'fussy
  (fussy-setup))

;; Ibuffer filters
(setq ibuffer-saved-filter-groups
      '(("default"
         ("org" (or
                 (mode . org-mode)
                 (name . "^\\*Org Src")
                 (name . "^\\*Org Agenda\\*$")))
         ("tramp" (name . "^\\*tramp.*"))
         ("emacs" (or
                   (name . "^\\*scratch\\*$")
                   (name . "^\\*Messages\\*$")
                   (name . "^\\*Warnings\\*$")
                   (name . "^\\*Shell Command Output\\*$")
                   (name . "^\\*Async-native-compile-log\\*$")
                   (name . "^\\*straight-")))
         ("ediff" (or
                   (name . "^\\*ediff.*")
                   (name . "^\\*Ediff.*")))
         ("dired" (mode . dired-mode))
         ("terminal" (or
                      (mode . term-mode)
                      (mode . shell-mode)
                      (mode . eshell-mode)))
         ("help" (or
                  (name . "^\\*Help\\*$")
                  (name . "^\\*info\\*$")
                  (name . "^\\*helpful"))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))
(setq ibuffer-show-empty-filter-groups nil) ; don't show empty groups

(require 'init-corfu)

(require 'init-dired)
(require 'init-helpful)

(require 'init-org)

(require 'init-magit)

;;; project
(defun my/project-files-in-directory (dir)
  "Use `fd' to list files in DIR."
  (let* ((default-directory dir)
         (localdir (file-local-name (expand-file-name dir)))
         (command (format "fd -H -t f -0 . %s" localdir)))
    (project--remote-file-names
     (sort (split-string (shell-command-to-string command) "\0" t)
           #'string<))))

(cl-defmethod project-files (project &optional dirs)
  (if (equal 'transient (car project))
      (progn
        (message "eglot transient single file")
        (when-let* ((server (eglot-current-server))
                    (buffers (eglot--managed-buffers server))
                    (paths (project--remote-file-names
                            (mapcar #'(lambda (buffer)
                                        (file-truename (buffer-file-name buffer)))
                                    buffers))))
          paths))
    (mapcan #'my/project-files-in-directory
            (or dirs (list (project-root project))))))

(require 'init-program)

;;; init.el ends here.
