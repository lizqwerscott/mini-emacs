;;; init-key.el --- binding key                      -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(keymap-unset ctl-x-map "b")
(keymap-unset ctl-x-map "C-k")

(global-set-keys
 '(("RET" . newline-and-indent)
   ("S-<return>" . comment-indent-new-line)
   (("s-o" "M-o") . other-window)
   (("s-n" "M-n") . scroll-up-1/3)
   (("s-p" "M-p") . scroll-down-1/3)
   (("M-N" "s-N") . scroll-other-window-up-1/3)
   (("M-P" "s-P") . scroll-other-window-down-1/3)
   (("s-x" "M-x") . execute-extended-command)
   ("C-s-f" . forward-sexp)
   ("C-s-b" . backward-sexp)

   ("C-x C-r" . consult-recent-file)

   ("C-x b g" . revert-buffer-quick)))

(with-eval-after-load 'eww
  (keymap-sets eww-mode-map
    '(("M-n" . scroll-up-1/3)
      ("M-p" . scroll-down-1/3))))

(provide 'init-key)
;;; init-key.el ends here
