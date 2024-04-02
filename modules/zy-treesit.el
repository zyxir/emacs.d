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

;; HACK: Add Nix to the list until it is officially added.
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

;; HACK: These tree-sitter grammars are currently problematic: (a) Org makes
;; Emacs crash in segmentation fault; (b) Janet, LaTeX, and Markdown could not
;; be correctly installed. Disable them.
(setq treesit-auto-langs
      (seq-remove (lambda (lang)
                    (memq lang '(org janet latex markdown)))
                  treesit-auto-langs))

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
