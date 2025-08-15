;;; init-completion.el --- init completion           -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; minibuffer

(with-eval-after-load 'fussy
  (fussy-setup))

;;; completion
(setq completion-styles '(basic substring initials fussy partial-completion)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (basic partial-completion initials substring))))
      tab-always-indent 'complete)


;; (setq completion-styles '(basic substring initials flex partial-completion))
;; (setq completion-category-defaults nil)
;; (setq completion-category-overrides
;;       '((file (styles . (basic partial-completion initials substring)))))

(setq completion-cycle-threshold 2)
(setq completion-ignore-case t)
(setq completion-show-inline-help nil)

(setq completions-detailed t)

;;; corfu
(require 'init-corfu)

(provide 'init-completion)
;;; init-completion.el ends here.
