;;; lib-org.el --- lib org                           -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: tools

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

(require 'org)

(defun org-toggle-display-emphasis-markers ()
  "Toggle show emphasis markers."
  (interactive)
  (setq org-hide-emphasis-markers
        (not org-hide-emphasis-markers))
  (revert-buffer-quick))

(defun latex-math-from-calc ()
  "Evaluate `calc' on the contents of line at point."
  (interactive)
  (cond ((region-active-p)
         (let* ((beg (region-beginning))
                (end (region-end))
                (string (buffer-substring-no-properties beg end)))
           (kill-region beg end)
           (insert (calc-eval `(,string calc-language latex
                                        calc-prefer-frac t
                                        calc-angle-mode rad)))))
        (t (let ((l (thing-at-point 'line)))
             (end-of-line 1) (kill-line 0)
             (insert (calc-eval `(,l
                                  calc-language latex
                                  calc-prefer-frac t
                                  calc-angle-mode rad)))))))

(defun org-insert-item-auto-checkbox ()
  "Org insert auto-checkbox item."
  (interactive)
  (org-insert-item
   (and (org-in-item-p)
        (save-excursion
          (looking-back "\\[[ xX]\\].**" (line-beginning-position))))))

(defun org-meta-return-auto (&optional arg)
  "Insert a new heading or wrap a region in a table.
Calls `org-insert-heading', `org-insert-item-auto-checkbox' or
`org-table-wrap-region', depending on context.  When called with
an ARG, unconditionally call `org-insert-heading'."
  (interactive "P")
  (or (run-hook-with-args-until-success 'org-metareturn-hook)
      (call-interactively (cond (arg #'org-insert-heading)
				                ((org-at-table-p) #'org-table-wrap-region)
				                ((org-in-item-p) #'org-insert-item-auto-checkbox)
				                (t #'org-insert-heading)))))

(require 'ox)

(defun org-get-export-file-paths ()
  "Get all possible export file paths for current Org buffer.
Returns an alist where keys are export formats and values are file paths."
  (interactive)
  (let ((paths '())
        (formats '(("html" . ".html")
                   ("latex" . ".tex")
                   ("pdf" . ".pdf")
                   ("markdown" . ".md")
                   ("odt" . ".odt")
                   ("org" . ".org")
                   ("ascii" . ".txt")
                   ("texinfo" . ".texi"))))
    (dolist (format formats)
      (let* ((backend (car format))
             (extension (cdr format))
             (output-file (org-export-output-file-name extension)))
        (when (file-exists-p output-file)
          (push (cons backend output-file) paths))))
    paths))

(defun org-list-export-file ()
  "Open export files Dired buffer."
  (interactive)
  (let* ((export-files (org-get-export-file-paths))
         (files (mapcar #'cdr export-files)))
    (cond
     ((null files)
      (message "No export files found for current buffer"))
     (t
      (dired (cons default-directory files))))))

(defun my/org-open-links-in-dired ()
  "Open all ‘file:’ links in Dired buffer."
  (interactive)
  (let* ((links
          (org-element-map (org-element-parse-buffer) 'link
            (lambda (link)
              (when (string= (org-element-property :type link) "file")
                (org-element-property :path link)))))
         (abs-paths
          (mapcar (lambda (path)
                    (expand-file-name path default-directory))
                  links)))
    (if abs-paths
        (dired abs-paths)
      (message "No file links found in this buffer."))))

;; From https://xenodium.com/emacs-dwim-do-what-i-mean
(require 'dom)
(defun ar/org-insert-link-dwim ()
  "Like `org-insert-link' but with personal dwim preferences."
  (interactive)
  (let* ((point-in-link (org-in-regexp org-link-any-re 1))
         (clipboard-url (when (string-match-p "^http" (current-kill 0))
                          (current-kill 0)))
         (region-content (when (region-active-p)
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end)))))
    (cond ((and region-content clipboard-url (not point-in-link))
           (delete-region (region-beginning) (region-end))
           (insert (org-make-link-string clipboard-url region-content)))
          ((and clipboard-url (not point-in-link))
           (insert (org-make-link-string
                    clipboard-url
                    (read-string "title: "
                                 (with-current-buffer (url-retrieve-synchronously clipboard-url)
                                   (dom-text (car
                                              (dom-by-tag (libxml-parse-html-region
                                                           (point-min)
                                                           (point-max))
                                                          'title))))))))
          (t
           (call-interactively 'org-insert-link)))))

;;; menu
(require 'lib-transient)

(defun hot-expand (str &optional mod)
  "Expand org template.

STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
  (let (text)
    (when (region-active-p)
      (setq text (buffer-substring (region-beginning) (region-end)))
      (delete-region (region-beginning) (region-end)))
    (insert str)
    (if (fboundp 'org-try-structure-completion)
        (org-try-structure-completion) ; < org 9
      (progn
        ;; New template expansion since org 9
        (require 'org-tempo nil t)
        (org-tempo-complete-tag)))
    (when mod (insert mod) (forward-line))
    (when text (insert text))))

(pretty-transient-define-prefix transient-org-template ()
  "Transient org template menu."
  [["Basic"
    ("e" "example" (hot-expand "<e"))
    ("l" "latex" (hot-expand "<l"))
    ("x" "quote" (hot-expand "<q"))
    ("v" "verse" (hot-expand "<v"))
    ("b" "bash" (hot-expand "<s" "bash"))]
   ["Head"
    ("i" "index" (hot-expand "<i"))
    ("I" "INCLUDE" (hot-expand "<I"))
    ("S" "Startup" (insert "#+STARTUP: "))
    ("L" "LaTeX" (hot-expand "<L"))
    ("P" "Latex Preview" (insert "#+STARTUP: latexpreview "))
    ("Mb" "Html Bigblow Theme" (insert "#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-bigblow.setup"))
    ("Mr" "Html Readtheorg Theme" (insert "#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-readtheorg.setup"))
    ("Mn" "Html Normal Css" (insert "#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"http://gongzhitaao.org/orgcss/org.css\"/>"))]
   ["Source"
    ("ss" "src" (hot-expand "<s"))
    ("se" "emacs-lisp" (hot-expand "<s" "emacs-lisp"))
    ("sp" "python" (hot-expand "<s" "python"))
    ("sP" "python" (hot-expand "<s" "python :results output"))
    ("sc" "c++" (hot-expand "<s" "c++"))
    ("sy" "yaml" (hot-expand "<s" "yaml-ts"))]
   ["Misc"
    ("u" "plantuml" (hot-expand "<s" "plantuml :file chart.png"))
    ("G" "gnuplot" (hot-expand "<s" "gnuplot :results output :file ./result.png"))
    ("<" "ins" self-insert-command)]]
  [("q" "Quit" transient-quit-one)])

(defun org-insert-or-surround (open close)
  "Insert or surround text with LaTeX-style delimiters.

If the region is active, wrap the selected text with the delimiters specified by
OPEN and CLOSE. Otherwise, insert the delimiters with space for text in between."
  (if (use-region-p)
      (let ((begin (region-beginning))
            (end (region-end)))
        (save-excursion
          (goto-char begin)
          (insert (format "\\%s " open))
          (goto-char (+ end 3))
          (insert (format " \\%s" close))))
    (insert (format "\\%s  \\%s" open close))
    (backward-char 3)))

(pretty-transient-define-prefix transient-org-line-template ()
  "Transient org line menu."
  [["Link"
    ("l" "Normal" ar/org-insert-link-dwim)]
   ["Emphasize"
    ("=" "Verbatim" (org-emphasize ?=))
    ("~" "Code" (org-emphasize ?=))
    ("+" "Delete" (org-emphasize ?+))
    ("_" "Underline" (org-emphasize ?_))

    ("/" "Italic" (org-emphasize ?/))
    ("*" "Bold" (org-emphasize ?*))
    ("e" "Emphasize" org-emphasize)]
   ["Latex"
    ("i" "Inline math" (org-insert-or-surround "(" ")"))
    ("I" "Display math" (org-insert-or-surround "[" "]"))
    ("L" "Convert to latex" latex-math-from-calc :if region-active-p)]
   ["Misc"
    (">" "ins" self-insert-command)]]
  [("q" "Quit" transient-quit-one)])

(pretty-transient-define-prefix transient-org-toggles ()
  "Transient org menu."
  :transient-non-suffix 'transient--do-stay
  [["Display"
    ("l" "Display Link" org-toggle-link-display :toggle (not org-link-descriptive) :transient t)
    ("m" "Hide Emphasis Markers" org-toggle-display-emphasis-markers :toggle org-hide-emphasis-markers :transient t)
    ("e" "Display Pretty Entities" org-toggle-pretty-entities :toggle org-pretty-entities :transient t)
    ("i" "Display inline images" org-toggle-inline-images :toggle org-inline-image-overlays :transient t)]
   ["Org Management"
    ("E" "Export" org-export-dispatch)
    ("L" "List export file" org-list-export-file)]]
  [("q" "Quit" transient-quit-one)])

(provide 'lib-org)
;;; lib-org.el ends here
