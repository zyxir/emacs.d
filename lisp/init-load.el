;;; init-load.el --- Load all Emacs Lisp codes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Setup borg to manage 3rd-party packages as git submodules

(add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
(require 'borg)
(borg-initialize)


(provide 'init-load)

;;; init-load.el ends here.
