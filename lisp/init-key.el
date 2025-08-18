(global-set-keys
 '(("RET" . newline-and-indent)
   ("S-<return>" . comment-indent-new-line)
   (("s-o" "M-o") . other-window)
   (("s-n" "M-n") . scroll-up-1/3)
   (("s-p" "M-p") . scroll-down-1/3)
   (("s-x" "M-x") . execute-extended-command)
   ("C-s-f" . forward-sexp)
   ("C-s-b" . backward-sexp)))

(with-eval-after-load 'eww
  (keymap-sets eww-mode-map
    '(("M-n" . scroll-up-1/3)
      ("M-p" . scroll-down-1/3))))

;; ;;; ### Watch other window ###
;; ;;; --- 滚动其他窗口
(require 'watch-other-window)
(global-set-keys
 '((("M-N" "s-N") . (lambda ()
                      (interactive)
                      (watch-other-window-internal "up"
                                                   (/ (window-body-height) 3))))
   (("M-P" "s-P") . (lambda ()
                      (interactive)
                      (watch-other-window-internal "down"
                                                   (/ (window-body-height) 3))))))

(provide 'init-key)
