;;; lib-git.el --- git                               -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: git

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

(defun unpackaged/open-magit-status (status-fn)
  "Use STATUS-FN Open a `magit-status' buffer.
If a file was visited in the buffer that was active when this
command was called, go to its unstaged changes section."
  (interactive)
  (let* ((buffer-file-path (when buffer-file-name
                             (file-relative-name buffer-file-name
                                                 (locate-dominating-file buffer-file-name ".git"))))
         (section-ident `((file . ,buffer-file-path) (unstaged) (status))))
    (call-interactively status-fn)
    (when buffer-file-path
      (goto-char (point-min))
      (cl-loop until (when (equal section-ident (magit-section-ident (magit-current-section)))
                       (magit-section-show (magit-current-section))
                       (recenter)
                       t)
               do (condition-case nil
                      (magit-section-forward)
                    (error
                     (cl-return
                      (progn
                        (goto-char (point-min))
                        (magit-status-goto-initial-section)))))))))

;;;###autoload
(defun unpackaged/magit-status ()
  "Open a `magit-status' buffer.
If a file was visited in the buffer that was active when this
command was called, go to its unstaged changes section."
  (interactive)
  (unpackaged/open-magit-status #'magit-status))

;;;###autoload
(defun unpackaged/magit-project-status ()
  "Open a `magit-project-status' buffer.
If a file was visited in the buffer that was active when this
command was called, go to its unstaged changes section."
  (interactive)
  (unpackaged/open-magit-status #'magit-project-status))

(provide 'lib-git)
;;; lib-git.el ends here
