;;; zy-treesit.el --- Tree-sitter. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+treesit' module of the configuration.

;; Tree-sitter is an incremental parsing library that parses the source code
;; more efficiently and accurately than traditional tools. Emacs has very good
;; support for tree-sitter. This module sets up Treesit-auto, a package used to
;; automatically install tree-sitter grammar for Emacs.

;;; Code:

(require 'zylib)

(pkg! 'treesit-auto)

;; Fontify everything with tree-sitter.
(after! 'treesit
  (setq treesit-font-lock-level 4))

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

;; HACK add Nix to the list until it is officially added.
(add-to-list 'treesit-auto-langs 'nix)
(add-to-list 'treesit-auto-recipe-list
             (make-treesit-auto-recipe
              :lang 'nix
              :ts-mode 'nix-ts-mode
              :remap 'nix-mode
              :url "https://github.com/nix-community/tree-sitter-nix"
              :ext "\\.nix\\'"))
(add-to-list 'global-treesit-auto-modes 'nix-mode)
(add-to-list 'global-treesit-auto-modes 'nix-ts-mode)

;; Enable Treesit-auto.
(treesit-auto-add-to-auto-mode-alist 'all)
(global-treesit-auto-mode 1)

(defun +treesit-mode-p ()
  "Return t if the current major mode is powered by tree-sitter."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       (string-suffix-p "-ts-mode" (symbol-name major-mode))))

(provide 'zy-treesit)

;;; zy-treesit.el ends here
