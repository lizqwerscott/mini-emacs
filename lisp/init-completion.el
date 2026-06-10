;;; init-completion.el --- init completion           -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Add prompt indicator to `completing-read-multiple'.
;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; Only list the commands of the current modes
(when (boundp 'read-extended-command-predicate)
  (setq read-extended-command-predicate
        #'command-completion-default-include-p))

(setq completion-cycle-threshold 4
      completions-detailed t
      completion-auto-help nil
      completion-styles '(basic))

;;; fussy

(setopt fussy-max-word-length-to-score 5000
        fussy-compare-same-score-fn 'fussy-histlen->strlen<)

(autoload #'fussy-orderless-setup "fussy-orderless" nil t)

(fussy-orderless-setup)
(fussy-eglot-setup)

(with-eval-after-load 'corfu
  ;; For cache functionality.
  (advice-add 'corfu--capf-wrapper :before 'fussy-wipe-cache)

  (add-hook 'corfu-mode-hook
            (lambda ()
              (setq-local fussy-score-fn 'flx-score
                          fussy-max-candidate-limit 5000
                          fussy-default-regex-fn 'fussy-pattern-first-letter
                          fussy-prefer-prefix nil))))

;;; orderless
(with-eval-after-load 'orderless
  (add-to-list 'orderless-affix-dispatch-alist
               `(?& . ,#'orderless-literal)))

;;; corfu
(require 'init-corfu)

(provide 'init-completion)
;;; init-completion.el ends here.
