;;; zy-orderless.el --- Orderless completion style. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+orderless' module of the configuration.

;; It configures the Orderless completion style provided by the Orderless
;; pacakge, which provides an enhanced, flexible, and configurable completion
;; experience for completing commands, file names, symbols, and so on.

;;; Code:

(require 'zylib)

(pkg! orderless)

(daemon-require! 'orderless)

(after! 'orderless
  (defun +orderless-literal-dispatcher (word _index _total)
    "Read WORD= as a literal string."
    (when (string-suffix-p "=" word)
      `(orderless-literal . ,(substring word 0 -1))))

  (defun +orderless-prefix-dispatcher (word _index _total)
    "Read WORD^ as a prefix."
    (when (string-suffix-p "^" word)
      `(orderless-regexp . ,(format "^%s" (substring word 0 -1)))))

  (defun +orderless-initialism-dispatcher (word _index _total)
    "Read WORD% as an initialism."
    (when (string-suffix-p "%" word)
      `(orderless-initialism . ,(substring word 0 -1))))

  (defun +orderless-file-ext-dispatcher (word _index _total)
    "Expand WORD. to a file suffix when completing file names."
    (when (and minibuffer-completing-file-name
               (string-suffix-p "." word))
      `(orderless-regexp . ,(format "\\.%s\\'" (substring word 0 -1)))))

  (setq orderless-matching-styles '(orderless-literal orderless-regexp)
        orderless-style-dispatchers '(+orderless-literal-dispatcher
                                      +orderless-prefix-dispatcher
                                      +orderless-initialism-dispatcher
                                      +orderless-file-ext-dispatcher)))

;; Setting up completion styles. The basic style should be preferred over
;; Orderless because it prioritize full matches.
(setq completion-styles '(orderless basic))

;; Fine-tune category-specific completion styles.
(setq completion-category-overrides
      '((file (styles basic partial-completion))))

(provide 'zy-orderless)

;;; zy-orderless.el ends here
