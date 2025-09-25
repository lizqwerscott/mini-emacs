;;; init-modeline.el --- init modeline               -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defface modeline-face-strong
  '((t (:foreground "#ECEFF4" :weight regular)))
  "Modeline strong face.
Used to highlight important information in the mode line.")

(defface modeline-face-faded
  '((t (:foreground "#b6a0ff")))
  "Modeline faded face.
Used to de-emphasize less important information in the mode line.")

;; #EBCB8B
(defface modeline-face-critical
  '((t (:foreground "#f1fa8c")))
  "Modeline critical face.
Used to indicate critical errors or very important states in the mode line.")

(defface modeline-face-info
  '((t (:foreground "#0000FFFF0000")))
  "Modeline info face.
Used to display informational or neutral status in the mode line.")

(defface modeline-face-warning
  '((t (:foreground "#b6a0ff")))
  "Modeline warning face.
Used to display warnings or cautionary messages in the mode line.")

(defface modeline-face-urgent
  '((t (:foreground "#FFFF00000000")))
  "Modeline urgent face.
Used to indicate urgent or high-priority issues in the mode line.")

(defun modeline-set-face (name &optional foreground background weight)
  "Set face NAME with given FOREGROUND, BACKGROUND and WEIGHT.
FOREGROUND and BACKGROUND should be color strings.
WEIGHT is a symbol such as 'regular, 'bold, or 'light."
  (apply #'set-face-attribute `(,name nil
                                      ,@(when foreground `(:foreground ,foreground))
                                      ,@(when background `(:background ,background))
                                      ,@(when weight `(:weight ,weight)))))

(with-eval-after-load 'meow
  (modeline-set-face 'meow-keypad-indicator "#b6a0ff")
  (modeline-set-face 'meow-normal-indicator "#50fa7b")
  (modeline-set-face 'meow-insert-indicator "#ff79c6")
  (modeline-set-face 'meow-beacon-indicator "#f1fa8c")
  (modeline-set-face 'meow-motion-indicator "#8995ba"))

;; from doom-modeline
(defsubst modeline-encoding ()
  "Return the encoding and EOL style for the current buffer in the mode line.
Shows both end-of-line type (LF, CRLF, CR) and the coding system (e.g. UTF-8)."
  (let ((sep " ")
        (face 'modeline-face-strong)
        (mouse-face 'modeline-face-faded))
    (concat
     sep

     ;; eol type
     (let ((eol (coding-system-eol-type buffer-file-coding-system)))
       (propertize
        (pcase eol
          (0 "LF ")
          (1 "CRLF ")
          (2 "CR ")
          (_ ""))
        'face face
        'mouse-face mouse-face
        'help-echo (format "End-of-line style: %s\nmouse-1: Cycle"
                           (pcase eol
                             (0 "Unix-style LF")
                             (1 "DOS-style CRLF")
                             (2 "Mac-style CR")
                             (_ "Undecided")))
        'local-map (let ((map (make-sparse-keymap)))
                     (define-key map [mode-line mouse-1] 'mode-line-change-eol)
                     map)))

     ;; coding system
     (let* ((sys (coding-system-plist buffer-file-coding-system))
            (cat (plist-get sys :category))
            (sym (if (memq cat
                           '(coding-category-undecided coding-category-utf-8))
                     'utf-8
                   (plist-get sys :name))))
       (propertize
        (upcase (symbol-name sym))
        'face face
        'mouse-face mouse-face
        'help-echo 'mode-line-mule-info-help-echo
        'local-map mode-line-coding-system-map))
     sep)))

(defsubst modeline-vcs ()
  "Return VCS branch and state for the current buffer in the mode line.
Shows the branch name and an icon for the file status:
`*` for modified/added, `?` for needs merge, `!` for warning/conflict,
and `@` for a clean state."
  (when (and vc-mode buffer-file-name)
    (when-let* ((vcs-state (vc-state buffer-file-name
                                     (vc-backend buffer-file-name)))
                (vcs-icon (cond ((memq vcs-state '(edited added))
                                 (list "*" 'modeline-face-info))
                                ((eq vcs-state 'needs-merge)
                                 (list "?" 'modeline-face-info))
                                ((eq vcs-state 'needs-update)
                                 (list "!" 'modeline-face-warning))
                                ((memq vcs-state '(removed conflict unregistered))
                                 (list "!" 'modeline-face-urgent))
                                (t (list "@" 'modeline-face-info))))
                (vcs-branch-name (cadr (split-string (string-trim vc-mode) "^[A-Z]+[-:]+"))))
      (propertize (format "%s%s" (car vcs-icon) vcs-branch-name)
                  'face (cdr vcs-icon)))))

(setq-default mode-line-format
              '((:eval (meow-indicator))
                (:eval (let ((prefix (cond (buffer-read-only     '("Read " . modeline-face-critical))
                                           ((buffer-modified-p)  '("" . modeline-face-critical))
                                           (t                    '("" . modeline-face-strong)))))
                         (propertize (car prefix) 'face (cdr prefix))))
                (:eval (propertize "%12b" 'face (cond (buffer-read-only     'modeline-face-strong)
                                                      ((buffer-modified-p)  'modeline-face-critical)
                                                      (t                    'modeline-face-strong))))
                (:propertize " L%l:C%c" 'face modeline-face-strong)
                (flymake-mode flymake-mode-line-format)
                mode-line-misc-info
                (:eval (let ((mode (cond ((consp mode-name) (car mode-name))
                                         ((stringp mode-name) mode-name)
                                         (t "unknow")))
                             (buffer-encoding (modeline-encoding))
                             (vcs-str (modeline-vcs)))
                         (list
                          (propertize " " 'display `(space :align-to (- right ,(length mode) ,(length buffer-encoding) 1 ,(length vcs-str))))
                          buffer-encoding
                          (propertize mode 'face 'modeline-face-faded)
                          " "
                          vcs-str)))))

(provide 'init-modeline)
;;; init-modeline.el ends here
