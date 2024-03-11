;;; init-treesit.el --- Tree-sitter configuration.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'treesit-auto)

;; Manage tree-sitter grammars and modes with Treesit-auto.
(require 'treesit-auto)
(setq treesit-auto-install 'prompt)
(treesit-auto-add-to-auto-mode-alist 'all)
(global-treesit-auto-mode 1)

(provide 'init-treesit)

;;; init-treesit.el ends here
