;;; init-magit.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'magit
  (setq magit-diff-refine-hunk t
        magit-save-repository-buffers nil
        magit-revision-insert-related-refs nil)

  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(add-hook 'magit-mode-hook
          #'magit-wip-mode)

(provide 'init-magit)
;;; init-magit.el ends here
