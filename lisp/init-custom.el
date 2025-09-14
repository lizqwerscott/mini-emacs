;;; init-custom.el --- define customizations         -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defcustom user/day-theme 'modus-operandi
  "Day theme name."
  :group 'user
  :type 'symbol)

(defcustom user/night-theme 'modus-vivendi
  "Night theme name."
  :group 'user
  :type 'symbol)

(defcustom user/font-mac-size 230
  "The font size in mac."
  :group 'user
  :type 'number)

(defcustom user/font-win-size 180
  "The font size in window."
  :group 'user
  :type 'number)

(defcustom user/font-linux-size 190
  "The font size in linux."
  :group 'user
  :type 'number)

(defcustom user/start-fullscreen t
  "Is fullscreen in start."
  :group 'user
  :type 'boolean)

(defcustom user/start-transparent nil
  "Is transparent in start."
  :group 'user
  :type 'boolean)

(defcustom user/minibuffer 'fido
  "The minibuffer farmework."
  :group 'user
  :type '(choice (const :tag "fido" fido)
                 (const :tag "mct" mct)))

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init-custom)
;;; init-custom.el ends here
