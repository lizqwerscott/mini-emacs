;;; project-x.el --- some utils for project          -*- lexical-binding: t; -*-

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

(require 'project)

(defun my/project-try-local (dir)
  "Determine if DIR is a non-Git project."
  (catch 'ret
    (let ((pr-flags '((".project")
                      ("go.mod" "Cargo.toml" "pom.xml") ;; higher priority
                      ("Makefile"))))
      (dolist (current-level pr-flags)
        (dolist (f current-level)
          (when-let* ((root (locate-dominating-file dir f)))
            (throw 'ret (cons 'local root))))))))

(defun my/project-files-in-directory (dir)
  "Use `fd' to list files in DIR."
  (let* ((default-directory dir)
         (localdir (file-local-name (expand-file-name dir)))
         (command (format "fd -H -t f -0 . %s" localdir)))
    (project--remote-file-names
     (sort (split-string (shell-command-to-string command) "\0" t)
           #'string<))))

(cl-defmethod project-root ((project (head local)))
  "Return root directory of the current project.

It usually contains the main build file, dependencies
configuration file, etc. Though neither is mandatory.

The directory name must be absolute.

PROJECT type is local."
  (cdr project))

(cl-defmethod project-files ((project (head local)) &optional dirs)
  "Return a list of files in directories DIRS in PROJECT.
DIRS is a list of absolute directories; it should be some
subset of the project root and external roots.

Use `fd'. PROJECT is used
to find the list of ignores for each directory."
  (mapcan #'my/project-files-in-directory
          (or dirs (list (project-root project)))))

(cl-defmethod project-files (project &optional dirs)
  "Return a list of files in directories DIRS in PROJECT.
DIRS is a list of absolute directories; it should be some
subset of the project root and external roots.

Use `fd'. PROJECT is used to find the list of ignores for each directory.
Check if the current project is a single file project in transient(eglot)
projects."
  (if (equal 'transient (car project))
      (progn
        (message "eglot transient single file")
        (require 'eglot)
        (when-let* ((server (eglot-current-server))
                    (buffers (eglot--managed-buffers server))
                    (paths (project--remote-file-names
                            (mapcar #'(lambda (buffer)
                                        (file-truename (buffer-file-name buffer)))
                                    buffers))))
          paths))
    (mapcan #'my/project-files-in-directory
            (or dirs (list (project-root project))))))

;;;###autoload
(defun project-root-path ()
  "Get current project path."
  (let ((project (project-current nil)))
    (when project
      (project-root project))))

(defun my/project-info ()
  "Get project info."
  (interactive)
  (message "%s" (project-current t)))

(defun my/add-dot-project ()
  "Add empty project."
  (interactive)
  (let* ((root-dir (read-directory-name "Root: "))
         (f (expand-file-name ".project" root-dir)))
    (message "Create %s..." f)
    (make-empty-file f)))

(defun my/project-discover ()
  "Add dir under search-path to project."
  (interactive)
  (dolist (search-path '("~/MyProject/" "~/github/"))
    (dolist (file (file-name-all-completions  "" search-path))
      (when (not (member file '("./" "../")))
        (let ((full-name (expand-file-name file search-path)))
          (when (file-directory-p full-name)
            (when-let* ((pr (project-current nil full-name)))
              (project-remember-project pr)
              (message "add project %s..." pr))))))))

(defun project-dired-dir (dired-dir)
  "Open project dir in project root directory.
DIRED-DIR is want open directory."
  (interactive (list
                (read-directory-name "Dired open: " (project-root (project-current)))))
  (dired dired-dir))

(defun project--open-projects ()
  "Return a list of projects with open buffers."
  (let* ((buffer-list
          ;; Ignore ephemeral buffers
          (match-buffers (lambda (buf)
                           (not (string-prefix-p " " (buffer-name buf))))))
         (directories
          (cl-remove-duplicates (mapcar
                                 (lambda (buf)
                                   (buffer-local-value 'default-directory buf))
                                 buffer-list)
                                :test #'equal)))
    (cl-remove-duplicates
     (seq-mapcat (lambda (directory)
                   (if-let* ((project (project-current nil directory)))
                       (list project)))
                 directories)
     :test (lambda (p1 p2) (equal (project-root p1) (project-root p2))))))

(defun project-switch-project-open ()
  "Switch to an open project to dispatch commands on."
  (interactive)
  (let* ((open-projects (mapcar #'project-root (project--open-projects)))
         ;; `project--file-completion-table' seems to accept any collection as
         ;; defined by `completing-read'.
         (completion-table (project--file-completion-table open-projects))
         (project-directory (completing-read "Select open project: "
                                             completion-table nil t)))
    (project-switch-project project-directory)))

(defun project-find-file-in-other-window (suggested-filename dirs project &optional include-all)
  "Complete a file name in DIRS in PROJECT and visit the result.

SUGGESTED-FILENAME is a file name, or part of it, which
is used as part of \"future history\".

If INCLUDE-ALL is non-nil, or with prefix argument when called
interactively, include all files from DIRS, except for VCS
directories listed in `vc-directory-exclusion-list'."
  (let* ((vc-dirs-ignores (mapcar
                           (lambda (dir)
                             (concat dir "/"))
                           vc-directory-exclusion-list))
         (all-files
          (if include-all
              (mapcan
               (lambda (dir) (project--files-in-directory dir vc-dirs-ignores))
               dirs)
            (project-files project dirs)))
         (completion-ignore-case read-file-name-completion-ignore-case)
         (default-directory (project-root project))
         (file (project--read-file-name
                project "Find file"
                all-files nil 'file-name-history
                suggested-filename)))
    (if (string= file "")
        (user-error "You didn't specify the file")
      (find-file-other-window file))))

(defun project-find-file-other-window (&optional include-all)
  "Visit a file (with completion) in the current project.

The filename at point (determined by `thing-at-point'), if any,
is available as part of \"future history\".  If none, the current
buffer's file name is used.

If INCLUDE-ALL is non-nil, or with prefix argument when called
interactively, include all files under the project root, except
for VCS directories listed in `vc-directory-exclusion-list'."
  (interactive "P")
  (let* ((pr (project-current t))
         (root (project-root pr))
         (dirs (list root))
         (project-files-relative-names t))
    (project-find-file-in-other-window
     (delq nil (list (and buffer-file-name (project--find-default-from
                                            buffer-file-name pr))
                     (thing-at-point 'filename)))
     dirs pr include-all)))

(defun project-switch-to-buffer-other-window (buffer-or-name)
  "Display buffer BUFFER-OR-NAME in the other window.
When called interactively, prompts for a buffer belonging to the
current project.  Two buffers belong to the same project if their
project instances, as reported by `project-current' in each
buffer, are identical."
  (interactive (list (project--read-project-buffer)))
  (switch-to-buffer-other-window buffer-or-name))


(defun project-edit-dir-local ()
  "Edit project root dir local."
  (interactive)
  (let* ((default-directory (project-root (project-current)))
         (dir-locals-file (concat default-directory
                                  ".dir-locals.el")))
    (if (file-exists-p dir-locals-file)
        (find-file dir-locals-file)
      (call-interactively #'add-dir-local-variable))))

(defun project-add-to-safe-local-variable ()
  "Let project be safe."
  (interactive)
  (let* ((project-path (project-root (project-current))))
    (customize-save-variable
     'safe-local-variable-directories
     (add-to-list 'safe-local-variable-directories
                  (file-truename project-path)))))


;;; from projection
(defcustom project-find-other-file-suffix
  '(;; handle C/C++ extensions
    ("cpp" "h" "hpp" "ipp")
    ("ipp" "h" "hpp" "cpp")
    ("hpp" "h" "ipp" "cpp" "cc")
    ("cxx" "h" "hxx" "ixx")
    ("ixx" "h" "hxx" "cxx")
    ("hxx" "h" "ixx" "cxx")
    ("c"   "h")
    ("m"   "h")
    ("mm"  "h")
    ("h"   "c" "cc" "cpp" "ipp" "hpp" "cxx" "ixx" "hxx" "m" "mm")
    ("cc"  "h" "hh" "hpp")
    ("hh"  "cc"))
  "Alist associating related files in a project by extension.
Configures relationships between files with similar base-names and different
extensions. For example foo.h is related to foo.cpp and can be jumped between
each other with `projection-find-other-file' by adding the following mappings
to this configuration:

    ((\"h\" \"cpp\")
     (\"cpp\" \"h\"))

In many cases the mapping between extensions should be reciprocal to ensure
you can jump between them from either file but this isn't required."
  :type '(alist :key-type string :value-type (list string)))

(defun project-find--related-extensions (initial-extension)
  "Get list of file extensions related to INITIAL-EXTENSION.
Looks for extensions based on `project-find-other-file-suffix'."
  (let ((extensions (make-hash-table :test #'equal))
        (searched-extensions (make-hash-table :test #'equal)))
    (puthash initial-extension t searched-extensions)
    (dolist (extention (cdr (assoc nil project-find-other-file-suffix)))
      (puthash extention t extensions))

    (while (> (hash-table-count searched-extensions) 0)
      (let ((ext (car (hash-table-keys searched-extensions))))
        (remhash ext searched-extensions)
        (puthash ext t extensions)
        (cl-loop for ext in (cdr (assoc ext project-find-other-file-suffix #'string-equal))
                 when (and ext (not (gethash ext extensions)))
                   do (puthash ext t searched-extensions))))

    (sort (hash-table-keys extensions) #'string<)))

(defun project-find--related-file-basenames (file-name)
  "Get list of basenames for other-files to FILE-NAME."
  (let* ((basename (file-name-nondirectory file-name))
         (extension (file-name-extension basename))
         (basename-no-ext
          (substring basename 0 (- (1+ (length extension)))))
         (related-extensions
          (project-find--related-extensions extension))
         (related-basenames (make-hash-table :test #'equal)))
    (when (> (length basename-no-ext) 0)
      (dolist (extension related-extensions)
        (when (> (length extension) 0)
          (setq extension (concat "." extension)))
        ;; File name with just the extension added on.
        (puthash (concat basename-no-ext extension) t related-basenames)))
    related-basenames))

(defun project-find--other-file-list (project file-name)
  "Get list of other files for the FILE-NAME in PROJECT."
  (unless (file-name-absolute-p file-name)
    (expand-file-name file-name (project-root project)))

  (let* ((other-file-basenames
          (project-find--related-file-basenames
           file-name))
         other-files)
    (dolist (file (project-files project))
      (when (gethash (file-name-nondirectory file) other-file-basenames)
        (push file other-files)))
    ;; Ensure current file-name is included in the other file list.
    (when (and (file-exists-p file-name)
               (not (gethash (file-name-nondirectory file-name)
                             other-file-basenames)))
      (push file-name other-files))
    ;; Return consistently ordered list of files.
    other-files))

(defun project-find--other-file ()
  "Select another file to jump to for `project-find-other-file'."
  (let* ((project (project-current))
         (files (project-find--other-file-list
                 project
                 (or buffer-file-name
                     (buffer-name))))
         ;; Existing position of the current file in the other-file list.
         (current-file-pos
          (when buffer-file-name
            (seq-position files buffer-file-name #'string-equal)))
         ;; Position of the next file in the other-file list.
         (other-file-pos (or (when current-file-pos
                               (unless (equal current-file-pos (1- (length files)))
                                 (1+ current-file-pos)))
                           0))
         ;; Other-files not including the current file.
         (files-not-current
          (if current-file-pos
              (append
               (seq-take files current-file-pos)
               (nthcdr   (1+ current-file-pos) files))
            (seq-copy files))))
    (cond
     ((not files-not-current)
      (error "No other files found"))
     ;; Select the next file relative to the current one
     (t
      (nth other-file-pos files)))))

;;;###autoload
(defun project-find-other-file ()
  "Switch between similar files to the current file in this project.
This function will huerestically determine all files in the project similar
to the current file and then `find-file' it. For example this can be used to
switch between C++ header and implementation files assuming the two have the
same basename and a different extension."
  (interactive)
  (when-let* ((file
               (project-find--other-file)))
    (funcall #'find-file file)))

(provide 'project-x)
;;; project-x.el ends here
