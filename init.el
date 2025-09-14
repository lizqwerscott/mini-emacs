;;; init.el --- init                                 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(and (file-readable-p custom-file) (load custom-file))

(require 'lib-elisp-utils)
(require 'lib-utils)

(require 'init-super-save)

(require 'init-ui)

(require 'init-meow)

(require 'init-edit)

(require 'init-completion)
(require 'init-minibuffer)

(require 'init-dired)
(require 'init-helpful)

(require 'init-tools)

(require 'init-org)

(require 'init-git)

(require 'init-program)

;;; init.el ends here.
