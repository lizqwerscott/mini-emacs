;;; dired
(require 'dired)
;; (setq trash-directory "~/.trashs/")
;; (setq delete-by-moving-to-trash t)
;; (setq dired-recursive-deletes 'always)
(setq dired-auto-revert-buffer t)
(setq dired-hide-details-hide-symlink-targets nil)
(setq dired-dwim-target t)
(setq dired-listing-switches "-AFhlv --group-directories-first")
(setq dired-kill-when-opening-new-dired-buffer t)

;; Dont prompt about killing buffer visiting delete file
(setq dired-clean-confirm-killing-deleted-buffers nil)
(setq dired-guess-shell-alist-user
      `((,(rx "."
              (or
               ;; Videos
               "mp4" "avi" "mkv" "flv" "ogv" "ogg" "mov"
               ;; Music
               "wav" "mp3" "flac"
               ;; Images
               "jpg" "jpeg" "png" "gif" "xpm" "svg" "bmp"
               ;; Docs
               "pdf" "md" "djvu" "ps" "eps" "doc" "docx" "xls" "xlsx" "ppt" "pptx")
              string-end)
         ,(cond ((eq system-type 'gnu/linux) "xdg-open")
                ((eq system-type 'darwin) "open")
                ((eq system-type 'windows-nt) "start")
                (t "")))))

(setq ls-lisp-dirs-first t)
(when sys/macp
  (setq insert-directory-program "/opt/homebrew/bin/gls"))

;; (setq dired-listing-switches
;;       "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")

;; Enable the disabled dired commands
(put 'dired-find-alternate-file 'disabled nil)

;;; dired-aux
(require 'dired-aux)
(setq dired-isearch-filenames 'dwim)
(setq dired-create-destination-dirs 'ask)
(setq dired-vc-rename-file t)

;;; dired-x
(require 'dired-x)
(setq dired-bind-vm nil)
(setq dired-bind-man nil)
(setq dired-bind-info nil)
(setq dired-omit-verbose nil)
(setq dired-omit-files (rx string-start
                           (or ".DS_Store"
                              ".cache"
                              ".vscode"
                              ".ccls-cache" ".clangd")
                           string-end))
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..*$"))

;;; Find file auto
(defcustom user/find-file-auto-regexp-list '("\\.xlsx?\\'" "\\.pptx?\\'" "\\.docx?\\'" "\\.mp4\\'" "\\.app\\'")
  "Open files via external program regexp list  when using find-file "
  :group 'user
  :type 'list)

(defun find-file-auto (orig-fun &rest args)
  "Open files via external program when using find-file"
  (let ((file (car args)))
    (if (cl-find-if (lambda (regexp)
                      (string-match regexp file))
                    user/find-file-auto-regexp-list)
        (shell-command-do-open
         (list (file-truename file)))
      (apply orig-fun args))))
(advice-add 'find-file :around 'find-file-auto)

;;; Functions
(when (version<= emacs-version "30.1")
  ;;; from emacs 30.1
  (defun shell-command-do-open (files)
    "Open each of FILES using an external program.
  This \"opens\" the file(s) using the external command that is most
  appropriate for the file(s) according to the system conventions."
    (let ((command shell-command-guess-open))
      (when (and (memq system-type '(windows-nt))
                 (equal command "start"))
        (setq command "open"))
      (if command
          (cond
           ((memq system-type '(ms-dos))
            (dolist (file files)
              (shell-command (concat command " " (shell-quote-argument file)))))
           ((memq system-type '(windows-nt))
            (dolist (file files)
              (w32-shell-execute command (convert-standard-filename file))))
           ((memq system-type '(cygwin))
            (dolist (file files)
              (call-process command nil nil nil file)))
           ((memq system-type '(darwin))
            (dolist (file files)
              (start-process (concat command " " file) nil command file)))
           (t
            (dolist (file files)
              (call-process command nil 0 nil file))))
        (error "Open not supported on this system")))))

(defun dired-copy-path ()
  "In dired, copy file path to kill-buffer.
At 2nd time it copy current directory to kill-buffer."
  (interactive)
  (when-let* ((path (dired-file-name-at-point)))
    (if (string= path (current-kill 0 1))
        (setq path (dired-current-directory)))
    (message path)
    (kill-new path)))

(defun dired-do-open-default ()
  "Open the current default directory using the external app."
  (interactive)
  (shell-command-do-open
   (list (file-truename default-directory))))

;;; Hook
(add-hook 'dired-mode-hook
          #'(lambda ()
              (when (boundp 'dired-async-mode)
                (dired-async-mode))
              (diredfl-mode)
              (dired-omit-mode)))

;;; Keymap
(keymap-sets dired-mode-map
  '(("TAB" . dired-subtree-cycle)
    ("e" . dired-toggle-read-only)
    ("W" . dired-copy-path)
    ("C-c +" . dired-create-empty-file)
    ("M-n" . scroll-up-1/3)
    ("M-p" . scroll-down-1/3)
    ("h" . dired-up-directory)
    ("C-c e" . dired-do-open-default)
    ("C-o" . dired-dispatch)))


;;; Menu
(defun transient-show--variable-to-checkbox (v)
  "Checkbox string representation of variable V.
V is either nil or non-nil."
  (if v "[x]" "[ ]"))

(defun transient-show--prefix-label (label prefix)
  "Label constructed with PREFIX and LABEL separated by a space."
  (format "%s %s" prefix label))

(defun transient-show-checkbox-label (v label)
  "Checkbox label using variable V and LABEL."
  (transient-show--prefix-label label (transient-show--variable-to-checkbox v)))

(require 'transient)

;;;###autoload
(transient-define-prefix dired-dispatch ()
  "Dired dispatch menu"
  [["Directory"
    ("h" "Hide Details" dired-hide-details-mode
     :description
     (lambda ()
       (transient-show-checkbox-label dired-hide-details-mode "Hide Details")))
    ("o" "Omit Mode" dired-omit-mode
     :description
     (lambda () (transient-show-checkbox-label dired-omit-mode "Omit Mode")))]]
  [("q" "Quit" transient-quit-all)])

(provide 'init-dired)
