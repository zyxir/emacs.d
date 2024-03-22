;;; zy-orderless.el --- Orderless completion style. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+orderless' module of the configuration.

;; It configures the Orderless completion style provided by the Orderless
;; pacakge, which provides an enhanced, flexible, and configurable completion
;; experience for completing commands, file names, symbols, and so on.

;;; Code:

(require 'zylib)

(pkg! 'orderless)

(daemon-require! 'orderless)

(after! 'orderless
  (defun +orderless-literal-dispatcher (word _index _total)
    "Read WORD= as a literal string."
    (when (string-suffix-p "=" word)
      `(orderless-literal . ,(substring word 0 -1))))

  (defun +orderless-initialism-dispatcher (word _index _total)
    "Read WORD% as an initialism."
    (when (string-suffix-p "%" word)
      `(orderless-initialism . ,(substring word 0 -1))))

  (setq orderless-matching-styles '(;; Match what is typed literally.
                                    orderless-literal
                                    ;; Match with regexps.
                                    orderless-regexp
                                    ;; It acts like the default flex style. Very
                                    ;; useful for fuzzy matching.
                                    orderless-flex)
        orderless-style-dispatchers '(+orderless-literal-dispatcher
                                      +orderless-initialism-dispatcher)))

;; Setting up completion styles. The built-in completion styles are fine, and I
;; have found that the flex completion style is very useful in fuzzy
;; completions. However the Orderless completion style is much more versatile
;; and includes the features of most of them. Therefore just use Orderless by
;; default and fallback to the basic style.
(setq completion-styles '(orderless basic))

;; Reset all the built-in per-category defaults so that `completion-styles' is
;; used everywhere, and we can tweak `completion-category-overrides' to fine
;; tune completion styles for a single category.
(setq completion-category-defaults nil)

;; Fine-tune category-specific completion styles. If Orderless does not do good
;; in some categories, just override it.
(setq completion-category-overrides
      '((file (styles . (basic partial-completion orderless)))
        (bookmark (styles . (basic substring)))
        (library (styles . (basic substring)))
        (embark-keybinding (styles . (basic substring)))
        (imenu (styles . (basic substring orderless)))
        (consult-location (styles . (basic substring orderless)))
        (kill-ring (styles . (emacs22 orderless)))
        (eglot (styles . (emacs22 substring orderless)))))

(provide 'zy-orderless)

;;; zy-orderless.el ends here
