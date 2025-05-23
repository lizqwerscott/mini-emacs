;;; init-edit.el --- init edit package               -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:


;;; Code:

;;; can use superword-mode
(add-hook 'prog-mode-hook
          'superword-mode)

;;; electric pair
;; (add-hook 'prog-mode-hook 'electric-pair-local-mode)
;; (add-hook 'conf-mode-hook 'electric-pair-local-mode)
;; (add-hook 'sly-mrepl-hook 'electric-pair-local-mode)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode t)

(require 'hungry-delete)
(setq hungry-delete-chars-to-skip " \t\f\v"
      hungry-delete-except-modes
      '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode))
(global-hungry-delete-mode t)

;;; fingertip
(require 'init-fingertip)

;;; vundo
(require 'vundo)
(setq vundo-glyph-alist vundo-unicode-symbols)
(global-set-key (kbd "C-/") #'vundo)

;;; nxml
(with-eval-after-load 'nxml-mode
  (keymap-sets nxml-mode-map
               '(("C-s-f" . nxml-down-element)
                 ("C-s-n" . nxml-forward-element)
                 ("C-s-p" . nxml-backward-element)
                 ("C-s-b" . nxml-backward-up-element))))

;;; isearch
(setq isearch-lazy-count t
      isearch-case-fold-search t
      lazy-count-prefix-format "%s/%s "
      search-whitespace-regexp ".*?")

(defun my-occur-from-isearch ()
    (interactive)
    (let ((query (if isearch-regexp
               isearch-string
             (regexp-quote isearch-string))))
      (isearch-update-ring isearch-string isearch-regexp)
      (let (search-nonincremental-instead)
        (ignore-errors (isearch-done t t)))
      (occur query)))

(with-eval-after-load 'isearch
  (keymap-sets isearch-mode-map
               '(("<escape>" . isearch-exit)
                 ("C-o" . my-occur-from-isearch))))

;;; auto mark comment
;; from https://github.com/magnars/expand-region.el/blob/b70feaa644310dc2d599dc277cd20a1f2b6446ac/er-basic-expansions.el#L102
(defun er--point-is-in-comment-p ()
  "t if point is in comment, otherwise nil"
  (or (nth 4 (syntax-ppss))
     (memq (get-text-property (point) 'face) '(font-lock-comment-face font-lock-comment-delimiter-face))))

(defun er/mark-comment ()
  "Mark the entire comment around point."
  (interactive)
  (when (er--point-is-in-comment-p)
    (let ((p (point)))
      (while (and (er--point-is-in-comment-p) (not (eobp)))
        (forward-char 1))
      (skip-chars-backward "\n\r")
      (set-mark (point))
      (goto-char p)
      (while (er--point-is-in-comment-p)
        (forward-char -1))
      (forward-char 1))))

(defun line-comment-p ()
  (save-excursion
    (back-to-indentation)
    (or (memq (get-text-property (point) 'face)
             '(font-lock-comment-face font-lock-comment-delimiter-face web-mode-comment-face)))))

(defun line-empty-p ()
  (string= (string-trim
            (buffer-substring-no-properties (line-beginning-position)
                                            (line-end-position)))
           ""))

;; cant mark top buffer comment
(defun find-comment-area (forwardp)
  (let ((findp t)
        (first-find t)
        (first-comment-pos nil)
        (last-comment-pos nil))
    (while (and findp
              (not (if forwardp
                     (= (line-beginning-position) (point-min))
                   (= (line-end-position) (point-max)))))
      (if (line-comment-p)
          (progn
            (when first-find
              (setf first-find nil)
              (setf first-comment-pos
                    (if forwardp
                        (line-end-position)
                      (line-beginning-position))))
            (setf last-comment-pos
                  (if forwardp
                      (line-beginning-position)
                    (line-end-position))))
        (when (not (line-empty-p))
          (setf findp nil)))
      (if forwardp
          (previous-line)
        (next-line)))
    (list first-comment-pos
          last-comment-pos
          (and first-comment-pos
             last-comment-pos))))

(defun lizqwer/mark-line-comment ()
  "Mark the line comment around point."
  (interactive)
  (when (or (line-comment-p)
           (line-empty-p))
    (let ((p (point))
          (start-comment-pos nil)
          (end-comment-pos nil))
      (setq pre-comment-pos (find-comment-area t))
      (goto-char p)
      (setq next-comment-pos (find-comment-area nil))
      (if (and (cl-third pre-comment-pos)
             (cl-third next-comment-pos))
          (progn
            (setq start-comment-pos
                  (cl-second pre-comment-pos))
            (setq end-comment-pos
                  (cl-second next-comment-pos)))
        (progn
          (if (cl-third pre-comment-pos)
              (progn
                (setq start-comment-pos
                      (cl-second pre-comment-pos))
                (setq end-comment-pos
                      (cl-first pre-comment-pos)))
            (if (cl-third next-comment-pos)
                (progn
                  (setq start-comment-pos
                        (cl-first next-comment-pos))
                  (setq end-comment-pos
                        (cl-second next-comment-pos)))))))
      (if (and start-comment-pos
             end-comment-pos)
          (progn
            (goto-char end-comment-pos)
            (set-mark end-comment-pos)
            (goto-char start-comment-pos))
        (message "not find comment!")))))

(defun mark-pre-comment ()
  "Mark pre line comment."
  (interactive)
  (when (or (line-comment-p)
           (line-empty-p))
    (let ((p (point)))
      (setq pre-comment-pos (find-comment-area t))

      (if (and (cl-second pre-comment-pos)
             (cl-first pre-comment-pos))
          (progn
            (set-mark (cl-first pre-comment-pos))
            (goto-char (cl-second pre-comment-pos)))
        (progn
          (goto-char p)
          (message "pos: %s" pre-comment-pos))))))

(defun mark-next-comment ()
  "Mark next line comment"
  (interactive)
  (when (or (line-comment-p)
           (line-empty-p))
    (let ((p (point)))
      (setq pre-comment-pos (find-comment-area nil))

      (if (and (cl-second pre-comment-pos)
             (cl-first pre-comment-pos))
          (progn
            (set-mark (cl-second pre-comment-pos))
            (goto-char (cl-first pre-comment-pos)))
        (progn
          (goto-char p)
          (message "pos: %s" pre-comment-pos))))))

;;; insert trailing semi
(defun insert-or-remove-trailing-char (&optional ch)
  (interactive)
  (let ((ch (or ch (read-char "Input char: ")))
        (fn (lambda (ch)
              (end-of-line)
              (if (eq (char-before) ch)
                  (delete-backward-char 1)
                (insert-char ch)))))
    (save-excursion
      (if (region-active-p)
          (save-restriction
            (narrow-to-region (region-beginning) (region-end))
            (deactivate-mark)
            (goto-char (point-min))
            (funcall fn ch)
            (while (< (point) (- (point-max) 1))
              (next-line)
              (funcall fn ch)))
        (funcall fn ch)))))

(defun insert-or-remove-trailing-semi ()
  (interactive)
  (insert-or-remove-trailing-char ?\;))

(defun insert-or-remove-trailing-comma ()
  (interactive)
  (insert-or-remove-trailing-char ?,))

(defun insert-trailing-char (&optional ch)
  (interactive)
  (let ((ch (or ch (read-char "Input char: ")))
        (fn (lambda (ch)
              (end-of-line)
              (unless (eq (char-before) ch)
                (insert-char ch)))))
    (save-excursion
      (if (region-active-p)
          (save-restriction
            (narrow-to-region (region-beginning) (region-end))
            (deactivate-mark)
            (goto-char (point-min))
            (funcall fn ch)
            (while (< (point) (- (point-max) 1))
              (next-line)
              (funcall fn ch)))
        (funcall fn ch)))))

(defun insert-trailing-semi ()
  (interactive)
  (insert-trailing-char ?\;))

(defun insert-trailing-semi-and-indent ()
  (interactive)
  (insert-trailing-char ?\;)
  (forward-char)
  (newline-and-indent))

;;; grugru
(grugru-default-setup)

(grugru-define-multiple
  ((c-ts-mode c++-ts-mode)
   (non-alphabet "&&" "||")
   (non-alphabet "&" "|")
   (non-alphabet "+" "-")
   (non-alphabet "*" "/" "%")
   (non-alphabet ">>" "<<")
   (non-alphabet "+=" "-=")
   (non-alphabet "*=" "/=" "%=")
   (non-alphabet "&=" "|=" "^=")
   (non-alphabet ">>=" "<<=")
   (non-alphabet "++" "--")
   (non-alphabet "==" "!=")
   (non-alphabet "<" "<=" ">" ">=")
   (symbol "break" "continue")
   (symbol "signed" "unsigned"))
  (c++-ts-mode
   (symbol "true" "false")
   (symbol "vector" "array" "deque")
   (symbol "class" "struct")
   (symbol "float" "double")
   (symbol "private" "public" "protected")
   (symbol "pair" "tuple")
   (symbol "static_cast" "dynamic_cast" "reinterpret_cast" "const_cast"))
  ((python-mode python-ts-mode)
   (symbol "True" "False"))
  ((emacs-lisp-mode lisp-mode)
   (symbol "when-let" "if-let")))

;;; add rectangle number lines
;;;###autoload
(defun my/insert-number-lines (start-at end-at step format)
  (interactive
   (list (read-number "Number to count from: " 1)
         (read-number "Number to count end: " 5)
         (read-number "step: " 1)
         (read-string "Format string: "
                      "%d ")))
  (save-excursion
    (dolist (i (number-sequence start-at end-at step))
      (insert (format format i))
      (newline-and-indent))))

;;; goto precent
;;;###autoload
(defun goto-percent (percent)
  "Goto PERCENT of buffer."
  (interactive "nGoto percent: ")
  (goto-char (/ (* percent (point-max)) 100)))

(defun scroll-up-1/3 ()
  (interactive)
  (scroll-up (/ (window-body-height) 3)))

(defun scroll-down-1/3 ()
  (interactive)
  (scroll-down (/ (window-body-height) 3)))

;;;###autoload
(defun toggle-sub-word-or-super-word ()
  (interactive)
  (if subword-mode
      (progn
        (superword-mode)
        (message "开启 super-word-mode"))
    (subword-mode)
    (message "开启 sub-word-mode")))

(provide 'init-edit)
;;; init-edit.el ends here.
