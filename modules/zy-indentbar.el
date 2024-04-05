;;; zy-indentbar.el --- Indentation guides. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+indentbar' module of the configuration.

;; Indentation guidelines are useful while editing complicated, nested code.
;; This module sets them up via the Indent-bars package. This package is
;; relatively new for this purpose and is not in any package archive yet, but it
;; is blazingly fast, and promising, since it is experimenting with tree-sitter
;; support.
;;
;; However, its displaying still have some minor issues, like: (1) The first
;; indentation is usually not displayed; (2) It does not display correct bars
;; for `scala-ts-mode'; (3) The highlighting is very strange.
;;
;; For these drawbacks, I decided to not use this module by now (as of
;; 2024-04-01) and wait until it matures.

;;; Code:

(require 'zylib)

(pkg! 'indent-bars
      :url "https://github.com/jdtsmith/indent-bars"
      :branch "main")

;; Display indent bars for any non-Lisp prog-mode.
(add-hook! 'prog-mode-hook
  (defun +indentbar-activate-h (&rest _)
    "Activate indent bars for any non-Lisp prog-mode."
    (unless (derived-mode-p 'lisp-data-mode)
      (indent-bars-mode 1))))

(after! 'indent-bars
  ;; Use a minimal appearance.
  (setq-default
   indent-bars-color '(highlight :face-bg t :blend 0.15)
   indent-bars-pattern "."
   indent-bars-width-frac 0.1
   indent-bars-pad-frac 0.1
   indent-bars-zigzag nil
   indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
   indent-bars-highlight-current-depth '(:blend 0.5))

  ;; Enable tree-sitter support.
  (setq indent-bars-treesit-support t)

  ;; Configure tree-sitter for each individual mode.
  (setq indent-bars-treesit-wrap
        '((python argument_list parameters list list_comprehension
                  dictionary dictionary_comprehension
                  parenthesized_expression subscript)
          (c argument_list parameter_list init_declarator))))

(provide 'zy-indentbar)

;;; zy-indentbar.el ends here
