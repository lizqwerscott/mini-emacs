;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; vc
(setq vc-handled-backends '(Git)
      vc-follow-symlinks t)

;;; ediff
(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(with-eval-after-load 'ediff
  (setq ediff-keep-variants nil
        ediff-make-buffers-readonly-at-startup nil
        ediff-merge-revisions-with-ancestor t
        ediff-show-clashes-only t))

(add-hook 'ediff-startup-hook
          (lambda ()
            (dolist (buffer (list ediff-buffer-A ediff-buffer-B ediff-buffer-C))
              (when buffer
                (with-current-buffer buffer
                  (outline-show-all))))))

;;; diff-mode
(with-eval-after-load 'diff-mode
  (setq diff-default-read-only t
        diff-font-lock-syntax 'hunk-also))

;;; magit
(with-eval-after-load 'magit
  (setq magit-diff-refine-hunk t
        magit-save-repository-buffers nil
        magit-revision-insert-related-refs nil)

  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(add-hook 'magit-mode-hook
          #'magit-wip-mode)

(provide 'init-git)
;;; init-git.el ends here
