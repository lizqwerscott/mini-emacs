;;; init-const.el --- some const file                -*- lexical-binding: t; -*-

;; Copyright (C) 2022  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:
(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defcustom user/day-theme 'ef-spring
  "User day theme"
  :group 'user
  :type 'symbol)

(defcustom user/night-theme 'modus-vivendi-tinted
  "User night theme"
  :group 'user
  :type 'symbol)

(defcustom user/font-mac-size 230
  "The font size in mac"
  :group 'user
  :type 'number)

(defcustom user/font-win-size 180
  "The font size in window"
  :group 'user
  :type 'number)

(defcustom user/font-linux-size 190
  "The font size in linux"
  :group 'user
  :type 'number)

(provide 'init-const)
;;; init-const.el ends here.
