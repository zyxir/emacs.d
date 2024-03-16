;; init-treesit.el --- Tree-sitter configuration.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-and-compile (require 'init-basic))

(require-package 'treesit-auto)
(require-package 'evil-textobj-tree-sitter)

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

;; Override some text objects from Evil-cleverparens, and provide more, by
;; Evil-textobj-tree-sitter.
(after! 'evil
  (require 'evil-textobj-tree-sitter)
  (with-no-warnings
    (general-def
      :keymaps 'evil-inner-text-objects-map
      "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
    (general-def
      :keymaps 'evil-outer-text-objects-map
      "f" (evil-textobj-tree-sitter-get-textobj "function.outer")
      "a" (evil-textobj-tree-sitter-get-textobj "conditional.outer"))))

(provide 'init-treesit)

;;; init-treesit.el ends here
