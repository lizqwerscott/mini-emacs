;;; init-headerline.el --- init header line          -*- lexical-binding: t; -*-

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

(defface header-line-face-shadow '((t (:inherit shadow))) "")

(defsubst header-line-file-name-format ()
  "Header line file name format"
  (when-let* ((file-path (buffer-file-name)))
    (list ""
          (propertize (abbreviate-file-name (file-name-directory file-path))
                      'face 'header-line-face-shadow)
          (file-name-base file-path)
          "."
          (file-name-extension file-path)
          "")))

;; (setq-default header-line-format
;;               '((:eval
;;                  (header-line-file-name-format))))

;;;###autoload
(define-minor-mode header-file-path-local-mode
  "Header lines with file path"
  :init-value nil
  (if header-file-path-mode
      (add-to-list 'header-line-format
                   '(:eval (header-line-file-name-format)))
    (setq header-line-format
          (delete '(:eval (header-line-file-name-format))
                  header-line-format))))

(defun header-file-path-turn-on-local-mode ()
  (unless (or (minibufferp)
              (not (buffer-file-name)))
    (header-file-path-local-mode 1)))

;;;###autoload
(define-globalized-minor-mode header-file-path-mode header-file-path-local-mode
  header-file-path-turn-on-local-mode)

(header-file-path-mode 1)

(provide 'init-headerline)
;;; init-headerline.el ends here
