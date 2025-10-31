;;; init-package.el --- init packages                -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>


;;; Commentary:

;;; Code:

(require 'package)
(require 'cl-lib)

(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(custom-set-variables
 '(package-vc-register-as-project nil))

(defun my/vc-git-clone (fn remote directory rev)
  (if (or (not (string-match-p "elpa" directory))
         (null rev))
      (funcall fn remote directory rev)
    (cond
     ((ignore-errors
        ;; First try if rev is a branch/tag name
        ;; https://stackoverflow.com/a/48748567/2163429
        (vc-git--out-ok "clone" "--depth" "1" "--single-branch" "--branch" rev remote directory)))
     ((vc-git--out-ok "clone" "--single-branch" remote directory)
      (let ((default-directory directory))
        (vc-git--out-ok "checkout" rev))))
    directory))

(advice-add 'vc-git-clone :around
            'my/vc-git-clone)

(cl-defun my/package-vc-install (name &key (fetcher 'github) repo url branch backend local-path)
  (unless (package-installed-p name)
    (if local-path
        (package-vc-install-from-checkout local-path
                                          (symbol-name name))
      (package-vc-install (if (equal 'git fetcher)
                              url
                            (format "https://%s%s"
                                    (pcase fetcher
                                      ('github "github.com/")
                                      ('sourcehut "git.sr.ht/~")
                                      ('codeberg "codeberg.org/"))
                                    repo))
                          branch
                          backend
                          name))))

(defun package! (package)
  (if (listp package)
      (apply #'my/package-vc-install
	         package)
    (unless (package-installed-p package)
      (unless (package-installed-p package)
      ;; from use-package-ensure
      (condition-case-unless-debug err
          (if (assoc package package-archive-contents)
              (package-install package)
            (package-refresh-contents)
            (package-install package))
        (error
         (display-warning 'package
                          (format "Failed to install %s: %s"
                                  package
                                  (error-message-string err))
                          :error)))))))

(defun packages! (packages)
  (dolist (package packages)
    (package! package)))

(defun emacs-update ()
  "Update Emacs all packages."
  (interactive)
  (call-interactively #'package-upgrade-all))

;;; install all package

(defvar *package-early-install-list*
  '(no-littering
    exec-path-from-shell

    mct
    marginalia
    fussy
    orderless
    helpful
    sudo-edit
    consult
    consult-dir

    markdown-mode
    csv-mode

    (meow :fetcher github :repo "meow-edit/meow")
    vundo
    puni
    hungry-delete
    super-save
    rg
    grugru
    visual-replace

    magit
    diff-hl

    corfu
    cape
    citre
    (eglot-booster :fetcher github :repo "jdtsmith/eglot-booster")
    apheleia
    yasnippet
    eldoc-box
    outli

    posframe
    rainbow-delimiters
    ace-window

    diredfl
    org-superstar))

(packages! *package-early-install-list*)

(provide 'init-package)
;;; init-package.el ends here
