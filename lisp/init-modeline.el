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

(defface modeline-face-default '((t)) "")
(defface modeline-face-strong '((t)) "")
(defface modeline-face-faded '((t)) "")
(defface modeline-face-critical '((t)) "")
(defface modeline-face-info '((t)) "")
(defface modeline-face-warning '((t)) "")
(defface modeline-face-urgent '((t)) "")
(defface modeline-face-meow-insert '((t)) "")

(defun modeline-set-face (name &optional foreground background weight)
  "Set NAME and NAME-i faces with given FOREGROUND, BACKGROUND and WEIGHT"

  (apply #'set-face-attribute `(,name nil
                                      ,@(when foreground `(:foreground ,foreground))
                                      ,@(when background `(:background ,background))
                                      ,@(when weight `(:weight ,weight)))))

(modeline-set-face 'modeline-face-default "#ECEFF4" "#2E3440") ;; Snow Storm 3
(modeline-set-face 'modeline-face-strong "#ECEFF4" nil 'regular) ;; Polar Night 0
(modeline-set-face 'modeline-face-faded "#b6a0ff") ;;
(modeline-set-face 'modeline-face-critical "#EBCB8B") ;; Aurora 2
(modeline-set-face 'modeline-face-info "#0000FFFF0000")
(modeline-set-face 'modeline-face-warning "#b6a0ff")
(modeline-set-face 'modeline-face-urgent "#FFFF00000000")
(modeline-set-face 'modeline-face-meow-insert "#9fefff")

(defface doom-modeline-evil-insert-state
  '((t (:inherit (doom-modeline font-lock-keyword-face))))
  "Face for the insert state tag in evil indicator."
  :group 'doom-modeline-faces)

;; from doom-modeline
(defsubst modeline-encoding ()
  "Encoding"
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
      (propertize (format " %s%s " (car vcs-icon) vcs-branch-name)
                  'face (cdr vcs-icon)))))

(setq-default mode-line-format
              '(:eval
                (let ((meow-mode-status (meow--get-state-name (meow--current-state)))
                      (prefix (cond (buffer-read-only     '("Read " . modeline-face-critical))
                                    ((buffer-modified-p)  '("" . modeline-face-critical))
                                    (t                    '("" . modeline-face-strong))))
                      (buffer-name-face (cond (buffer-read-only     'modeline-face-strong)
                                              ((buffer-modified-p)  'modeline-face-critical)
                                              (t                    'modeline-face-strong)))
                      (mode (concat " " (cond ((consp mode-name) (car mode-name))
                                              ((stringp mode-name) mode-name)
                                              (t "unknow"))
                                    " "))
                      (coords (format-mode-line "  L%l:C%c"))
                      (buffer-encoding (modeline-encoding))
                      (vcs-str (modeline-vcs)))
                  (list
                   (propertize (format " %s " meow-mode-status) 'face (if (meow-insert-mode-p)
                                                                          'modeline-face-meow-insert
                                                                        'modeline-face-faded))
                   (propertize (car prefix) 'face (cdr prefix))
                   (propertize (format-mode-line "%b") 'face buffer-name-face)
                   (propertize coords 'face 'modeline-face-strong)
                   (when flymake-mode
                     flymake-mode-line-format)
                   (propertize " " 'display `(space :align-to (- right ,(length mode) ,(length buffer-encoding) ,(length vcs-str))))
                   buffer-encoding
                   (propertize mode 'face 'modeline-face-faded)
                   vcs-str))))

(provide 'init-modeline)
;;; init-modeline.el ends here
