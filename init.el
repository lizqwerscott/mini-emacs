;;; init.el --- init                                 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (file-exists-p custom-file)
  (load custom-file))

(require 'init-super-save)

(require 'init-ui)

(require 'init-key)
(require 'init-meow)

(require 'init-edit)

(require 'init-completion)
(require 'init-minibuffer)

(require 'init-dired)
(require 'init-helpful)

(require 'init-org)

(require 'init-git)

(require 'init-program)

;;; init.el ends here.
