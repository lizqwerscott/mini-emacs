;;; minibuffer-prefix.el --- minibuffer prefix package  -*- lexical-binding: t; -*-

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



(defface icomplete-vertical-selected-prefix-indicator-face
  '((t :inherit font-lock-keyword-face :weight bold :foreground "cyan"))
  "Face used for the prefix set by `icomplete-vertical-selected-prefix-indicator'."
  :group 'icomplete
  :version "31.1")

(defface icomplete-vertical-unselected-prefix-indicator-face
  '((t :inherit font-lock-keyword-face :weight normal :foreground "gray"))
  "Face used for the prefix set by `icomplete-vertical-unselected-prefix-indicator'."
  :group 'icomplete
  :version "31.1")

(defcustom icomplete-vertical-in-buffer-adjust-list t
  "Control whether in-buffer completion should align the cursor position.
If this is t and `icomplete-in-buffer' is t, and `icomplete-vertical-mode'
is activated, the in-buffer vertical completions are shown aligned to the
cursor position when the completion started, not on the first column, as
the default behaviour."
  :type 'boolean
  :group 'icomplete
  :version "31.1")

(defcustom icomplete-vertical-render-prefix-indicator t
  "Control whether a indicator is added as a prefix to each candidate.
If this is t and `icomplete-vertical-mode' is activated, a indicator,
controlled by `icomplete-vertical-selected-prefix-indicator' is shown
as a prefix to the current under selection candidate, while the
remaining of the candidates will receive the indicator controlled
by `icomplete-vertical-unselected-prefix-indicator'."
  :type 'boolean
  :group 'icomplete
  :version "31.1")

(defcustom icomplete-vertical-selected-prefix-indicator "» "
  "Prefix string used to mark the selected completion candidate.
If `icomplete-vertical-render-prefix-indicator' is t, the string
defined here is used as a prefix of the currently selected entry in the
list.  It can be further customized by the face
`icomplete-vertical-selected-prefix-indicator-face'."
  :type 'string
  :group 'icomplete
  :version "31.1")

(defcustom icomplete-vertical-unselected-prefix-indicator "  "
  "Prefix string used on the unselected completion candidates.
If `icomplete-vertical-render-prefix-indicator' is t, the string
defined here is used as a prefix for all unselected entries in the list.
list.  It can be further customized by the face
`icomplete-vertical-unselected-prefix-indicator-face'."
  :type 'string
  :group 'icomplete
  :version "31.1")

;; FIXME: make this into PATCH - OK
(defun icomplete-vertical--adjust-lines-for-column (lines buffer data)
  "Adjust the LINES to align with the column in BUFFER based on DATA."
  (if icomplete-vertical-in-buffer-adjust-list
      (let* ((column (current-column))
             (prefix-indicator-width
              (if icomplete-vertical-render-prefix-indicator
                  (max (length icomplete-vertical-selected-prefix-indicator)
                       (length icomplete-vertical-unselected-prefix-indicator))
                0))
             (wrapped-line (with-current-buffer buffer
                             (save-excursion
                               (goto-char (car data))
                               (beginning-of-line)
                               (count-screen-lines (point) (car data)))))
             (window-width (+ (window-hscroll) (window-body-width)))
             (longest-line-width (apply #'max (mapcar #'length lines)))
             (spaces-to-add
              (if (> wrapped-line 1)
                  (- column (* (- wrapped-line 1) (- window-width 5)))
                column))
             (spaces-to-add-avoiding-scrolling
              (if (>= (+ spaces-to-add longest-line-width prefix-indicator-width) window-width)
                  (- spaces-to-add longest-line-width)
                spaces-to-add)))

        (mapcar (lambda (line)
                  (concat (make-string spaces-to-add-avoiding-scrolling ?\s) line))
                lines))
    lines))

;; FIXME: what to demo/test:
;;
;; This patch provides two more new features, which improves icomplete-vertical-mode, 1 and 2,
;; explained below:
;;
;;
;; 1.) Improve feature provided by `icomplete-in-buffer'.
;;     If user, besides setting `icomplete-in-buffer' to t, also set the
;;     new `icomplete-vertical-in-buffer-adjust-list' to t, the following are fixed/ improved:
;;
;; Without the new `icomplete-vertical-in-buffer-adjust-list':
;; - [ ] wrapped lines   - completion candidates on different columns always shows candidates at column 0
;; - [ ] wrapped lines   - completion candidates on different lines always shows candidates at column 0
;; - [ ] wrapped lines   - completion candidates close to the end of buffer won't be printed
;; - [ ] truncated lines - completion candidates on different columns always shows candidates at column 0
;; - [ ] truncated lines - completion candidates on horizontally scrolled windows won't appear on buffer
;;                         as they're on column 0
;; - [ ] truncated lines - completion candidates close to the end of buffer wont be shown
;;
;;
;; With the new `icomplete-vertical-in-buffer-adjust-list':
;; - [ ] wrapped lines   - fix    : completion candidates on different columns will always be printed
;;                                  under the cursor
;; - [ ] wrapped lines   - feature: completion candidates on different columns close to the end
;;                                  of the buffer will adjust so they stay visible
;; - [ ] wrapped lines   - fix:   : completion candidates on different lines always be printed under
;;                                  the cursor
;; - [ ] wrapped lines   - fix    : if icomplete-prospects-height won't fit from current line to the
;;                                  end of vertical space, our window will be scrolled so we have at
;;                                  least this amount of lines. This ensures our candidates list is
;;                                  always visible
;; - [ ] truncated lines - fix    : completion candidates on different columns will always be printed
;;                                  under the cursor
;; - [ ] truncated lines - feature: completion candidates on different columns close to the end
;;                                  of the buffer will adjust so they stay visible even when we scroll
;;                                  horizontally
;; - [ ] truncated lines - feature: completion candidates on horizontally scrolled windows will be
;;                                  printed under the cursor
;; - [ ] wrapped lines   - feature: if icomplete-prospects-height won't fit from current line to the
;;                                  end of vertical space, our window will be scrolled so we have at
;;                                  least this amount of lines. This ensures our candidates list is
;;                                  always visible
;; - [ ] from wrapped    - feature: if we are on wrapped lines and manually horiontal scroll, the lines
;;       to truncated               will become automatically truncated, in this case, all the features
;;                                  above still works from either mode (wrapped or truncated).
;;
;;
;; 2.) Implements new feature which provides customizable prefix indicators
;;
;; Setting `icomplete-vertical-render-prefix-indicator' to t will provide a prefix indicator
;; to indicate the current selected candidate, by default "» ".
;;
;; This prefix is customizable through the variable `icomplete-vertical-selected-prefix-indicator'
;; and de face `icomplete-vertical-selected-prefix-indicator-face'.
;;
;; Users can also customize an indicator to the not selected candidates trhough the use of
;; the variable `icomplete-vertical-unselected-prefix-indicator', by default: "  ", and the face
;; `icomplete-vertical-unselected-prefix-indicator-face'.
;;


;; FIXME: remove this after patch
(defun icomplete-vertical--ensure-visible-lines-inside-buffer ()
  "Ensure the completion list is visible in regular buffers only.
Scrolls the screen to be at least `icomplete-prospects-height' real lines
away from the bottom.  Counts wrapped lines as real lines."
  (unless (minibufferp)
    (let* ((window-height (window-body-height))
           (current-line (count-screen-lines (window-start) (point)))
           (lines-to-bottom (- window-height current-line)))
      (when (< lines-to-bottom icomplete-prospects-height)
        (scroll-up (- icomplete-prospects-height lines-to-bottom))))))


(defun icomplete-vertical--add-indicator-to-selected (comp)
  "Add indicators to the selected/unselected COMP completions."
  (if (and icomplete-vertical-render-prefix-indicator
           (get-text-property 0 'icomplete-selected comp))
      (concat (propertize icomplete-vertical-selected-prefix-indicator
                          'face 'icomplete-vertical-selected-prefix-indicator-face)
              comp)
    (concat (propertize icomplete-vertical-unselected-prefix-indicator
                        'face 'icomplete-vertical-unselected-prefix-indicator-face)
            comp)))


(cl-defun icomplete--render-vertical
    (comps md &aux scroll-above scroll-below
           (total-space ; number of mini-window lines available
            (1- (min
                 icomplete-prospects-height
                 (truncate (max-mini-window-lines) 1)))))
  ;; Welcome to loopapalooza!
  ;;
  ;; First, be mindful of `icomplete-scroll' and manual scrolls.  If
  ;; `icomplete--scrolled-completions' and `icomplete--scrolled-past'
  ;; are:
  ;;
  ;; - both nil, there is no manual scroll;
  ;; - both non-nil, there is a healthy manual scroll that doesn't need
  ;;   to be readjusted (user just moved around the minibuffer, for
  ;;   example);
  ;; - non-nil and nil, respectively, a refiltering took place and we
  ;;   may need to readjust them to the new filtered `comps'.
  (when (and icomplete-scroll                                    ;; FIXME: remove this after patch
             (not icomplete--scrolled-completions)
             (not icomplete--scrolled-past))
    (icomplete-vertical--ensure-visible-lines-inside-buffer))
  (when (and icomplete-scroll
             icomplete--scrolled-completions
             (null icomplete--scrolled-past))
    (icomplete-vertical--ensure-visible-lines-inside-buffer)     ;; FIXME: remove this after patch
    (cl-loop with preds
             for (comp . rest) on comps
             when (equal comp (car icomplete--scrolled-completions))
             do
             (setq icomplete--scrolled-past preds
                   comps (cons comp rest))
             (completion--cache-all-sorted-completions
              (icomplete--field-beg)
              (icomplete--field-end)
              comps)
             and return nil
             do (push comp preds)
             finally (setq icomplete--scrolled-completions nil)))
  ;; Then, in this pretty ugly loop, collect completions to display
  ;; above and below the selected one, considering scrolling
  ;; positions.
  (cl-loop with preds = icomplete--scrolled-past
           with succs = (cdr comps)
           with space-above = (- total-space
                                 1
                                 (cl-loop for (_ . r) on comps
                                          repeat (truncate total-space 2)
                                          while (listp r)
                                          count 1))
           repeat total-space
           for neighbor = nil
           if (and preds (> space-above 0)) do
           (push (setq neighbor (pop preds)) scroll-above)
           (cl-decf space-above)
           else if (consp succs) collect
           (setq neighbor (pop succs)) into scroll-below-aux
           while neighbor
           finally (setq scroll-below scroll-below-aux))
  ;; Halfway there...
  (let* ((selected (propertize (car comps) 'icomplete-selected t))
         (chosen (append scroll-above (list selected) scroll-below))
         (tuples (icomplete--augment md chosen))
         max-prefix-len max-comp-len lines nsections)
    (add-face-text-property 0 (length selected)
                            'icomplete-selected-match 'append selected)
    ;; Figure out parameters for horizontal spacing
    (cl-loop
     for (comp prefix) in tuples
     maximizing (length prefix) into max-prefix-len-aux
     maximizing (length comp) into max-comp-len-aux
     finally (setq max-prefix-len max-prefix-len-aux
                   max-comp-len max-comp-len-aux))
    ;; Serialize completions and section titles into a list
    ;; of lines to render
    (cl-loop
     for (comp prefix suffix section) in tuples
     when section
     collect (propertize section 'face 'icomplete-section) into lines-aux
     and count 1 into nsections-aux
     for comp = (icomplete-vertical--add-indicator-to-selected comp)
     when (get-text-property 0 'icomplete-selected comp)
     do (add-face-text-property 0 (length comp)
                                'icomplete-selected-match 'append comp)
     collect (concat prefix
                     (make-string (max 0 (- max-prefix-len (length prefix))) ? )
                     (completion-lazy-hilit comp)
                     (make-string (max 0 (- max-comp-len (length comp))) ? )
                     suffix)
     into lines-aux
     finally (setq lines lines-aux
                   nsections nsections-aux))
    ;; Kick out some lines from the beginning due to extra sections.
    ;; This hopes to keep the selected entry more or less in the
    ;; middle of the dropdown-like widget when `icomplete-scroll' is
    ;; t.  Funky, but at least I didn't use `cl-loop'
    (setq lines
          (nthcdr
           (cond ((<= (length lines) total-space) 0)
                 ((> (length scroll-above) (length scroll-below)) nsections)
                 (t (min (ceiling nsections 2) (length scroll-above))))
           lines))
    (when icomplete--in-region-buffer
      (setq lines (icomplete-vertical--adjust-lines-for-column
                   lines icomplete--in-region-buffer completion-in-region--data)))
    ;; At long last, render final string return value.  This may still
    ;; kick out lines at the end.
    (concat " \n"
            (cl-loop for l in lines repeat total-space concat l concat "\n"))))

(provide 'minibuffer-prefix)
;;; minibuffer-prefix.el ends here.
