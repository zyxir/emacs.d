;; init-treesit.el --- Tree-sitter configuration.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'treesit-auto)

;; Fontify everything.
(setq treesit-font-lock-level 4)

;; Manage tree-sitter grammars and modes with Treesit-auto.
(require 'treesit-auto)
(setq treesit-auto-install 'prompt)

;; HACK add Scala to the list until it is officially added.
(add-to-list 'treesit-auto-langs 'scala)
(add-to-list 'treesit-auto-recipe-list
             (make-treesit-auto-recipe
              :lang 'scala
              :ts-mode 'scala-ts-mode
              :remap 'scala-mode
              :url "https://github.com/tree-sitter/tree-sitter-scala"
              :ext "\\.\\(scala\\|sbt\\)\\'"))
(add-to-list 'global-treesit-auto-modes 'scala-mode)
(add-to-list 'global-treesit-auto-modes 'scala-ts-mode)

;; Enable Treesit-auto.
(treesit-auto-add-to-auto-mode-alist 'all)
(global-treesit-auto-mode 1)

(provide 'init-treesit)

;;; init-treesit.el ends here
