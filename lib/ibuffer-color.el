;;; ibuffer-color.el --- ibuffer color               -*- lexical-binding: t; -*-

;; Copyright (C) 2026  lizqwer scott

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

(require 'ibuffer)

(defgroup ibuffer-color nil
  "Display color in ibuffer."
  :group 'ibuffer)

(defface ibuffer-color-size-face
  '((t (:inherit font-lock-constant-face)))
  "Face used for the size."
  :group 'ibuffer-color)

(defface ibuffer-color-mode-face
  '((t (:inherit font-lock-keyword-face)))
  "Face used for the major mode."
  :group 'ibuffer-color)

(defface ibuffer-color-file-face
  '((t (:inherit completions-annotations)))
  "Face used for the filename/process."
  :group 'ibuffer-color)

(define-ibuffer-column size-h
  (:name "Size"
   :inline t
   :props ('font-lock-face 'ibuffer-color-size-face)
   :header-mouse-map ibuffer-size-header-map
   :summarizer
   (lambda (strings)
     (let ((total
            (cl-loop
             for s in strings
             for i = (text-property-not-all 0 (length s) 'ibuffer-size nil s)
             if i sum (get-text-property i 'ibuffer-size s))))
       (if ibuffer-human-readable-size
           (file-size-human-readable total)
         (number-to-string total)))))
  (let ((size (buffer-size)))
    (propertize (if ibuffer-human-readable-size
                    (file-size-human-readable size)
                  (number-to-string size))
                'ibuffer-size size)))

(define-ibuffer-column mode+
  (:name "Mode"
   :inline t
   :header-mouse-map ibuffer-mode-header-map
   :props ('font-lock-face 'ibuffer-color-mode-face
                           'mouse-face 'highlight
	                       'keymap ibuffer-mode-name-map
	                       'help-echo "mouse-2: filter by this mode"))
  (format-mode-line mode-name nil nil (current-buffer)))

(define-ibuffer-column filename-and-process+
  (:name "Filename/Process"
   :props ('font-lock-face 'ibuffer-color-file-face)
   :header-mouse-map ibuffer-filename/process-header-map
   :summarizer
   (lambda (strings)
     (setq strings (delete "" strings))
     (let ((procs 0)
	       (files 0))
       (dolist (string strings)
         (when (get-text-property 1 'ibuffer-process string)
           (setq procs (1+ procs)))
	     (setq files (1+ files)))
       (concat (cond ((zerop files) "No files")
		             ((= 1 files) "1 file")
		             (t (format "%d files" files)))
	           ", "
	           (cond ((zerop procs) "no processes")
		             ((= 1 procs) "1 process")
		             (t (format "%d processes" procs)))))))
  (let ((proc (get-buffer-process buffer))
	    (filename (ibuffer-make-column-filename buffer mark)))
    (if proc
	    (concat (propertize (format "(%s %s)" proc (process-status proc))
			                'font-lock-face 'italic
                            'ibuffer-process proc)
		        (if (> (length filename) 0)
		            (format " %s" filename)
		          ""))
      filename)))

(define-ibuffer-column recency+
  (:name "Recency" :inline t :header-mouse-map ibuffer-recency-header-map)
  (if-let* ((time (buffer-local-value 'buffer-display-time buffer)))
      (format "%s ago" (seconds-to-string
                        (float-time (time-since time)) t t))
    "never"))

(provide 'ibuffer-color)
;;; ibuffer-color.el ends here
