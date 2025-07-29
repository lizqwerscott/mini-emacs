;;; init-project.el --- init project function        -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: lisp, lisp

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

(require 'project)
(require 'project-x)

;;; config
(setq project-find-functions '(project-try-vc my/project-try-local))

;;; project-prefix-map
;; (defalias 'project-prefix-map project-prefix-map)

;; (define-key mode-specific-map "p" 'project-prefix-map)
(define-key project-prefix-map (kbd "B") #'project-switch-to-buffer-other-window)
(define-key project-prefix-map (kbd "v") #'magit-project-status)

;;; project-switch-commands
(setq project-switch-commands nil)
(add-to-list 'project-switch-commands '(project-find-file "Find file") t)
(add-to-list 'project-switch-commands '(project-switch-to-buffer "switch to buffer") t)
(add-to-list 'project-switch-commands '(magit-project-status "Git Status") t)
(add-to-list 'project-switch-commands '(project-find-dir "Find Dir") t)
(add-to-list 'project-switch-commands '(project-dired "Dired") t)

;;; Menu
(require 'transient)

(keymap-set transient-map
            "<escape>"
            #'transient-quit-all)

(transient-define-prefix project-manage-dispatch ()
  "Manage Project menu"
  [["Manage"
    ("r" "Forget under" project-forget-projects-under)
    ("z" "Forget zombie" project-forget-zombie-projects)
    ("a" "Remember under" project-remember-projects-under)]]
  [("q" "Quit" transient-quit-one)])

(transient-define-prefix project-dispatch ()
  "Project dispatch menu"
  [["Project"
    ("C-p" "Manage" project-manage-dispatch
     :transient t)]
   [" "
    ("p" "Switch" project-switch-project)]
   [" "
    ("P" "Switch Open" project-switch-project-open)]]
  [["Find"
    ("f" "File" project-find-file)
    ("F" "File OW" project-find-file-other-window)
    ("o" "Other file" project-find-other-file)
    ("d" "Dir" project-dired-dir)
    ]
   ["Buffer"
    ("b" "Switch" project-switch-to-buffer)
    ("B" "Switch OW" project-switch-to-buffer-other-window)
    ("k" "Kill" project-kill-buffers)]
   ["Build"
    ("c" "Compile" project-compile)]
   ["Dir Locals"
    ("e e" "Edit" project-edit-dir-local)
    ("e s" "Trust" project-add-to-safe-local-variable)
    ("e a" "Add" add-dir-local-variable)]
   ["Other"
    ("v" "Magit status" magit-project-status)
    ("s""Eshell" project-eshell)]]
  [("q" "Quit" transient-quit-all)])

(provide 'init-project)
;;; init-project.el ends here
