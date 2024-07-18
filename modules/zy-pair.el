;;; zy-pair.el --- Pair-editing. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+pair' module of the configuration.

;; This module sets up Smartparens, a powerful pair-editing suite.

;;; Code:

(require 'zylib)

;; As of 2024-03-28, Smartparens in ELPA and Melpa-stable was updated in 2017,
;; lacks many useful functions and bugfixes, and even requires the deprecated
;; feature `cl', whereas the Melpa version was updated within this month. I have
;; to pin it to Melpa to provide a better experience.
(pin-to! "melpa" 'smartparens)

(pkg! 'smartparens)

;; Use Smartparens for most modes.
(add-hook! '(prog-mode-hook
             text-mode-hook
             conf-mode-hook)
  (defun +pair-setup-smartparens-h (&rest _)
    ;; Enable Smartparens mode.
    (smartparens-mode 1)
    ;; If the mode is a Lisp mode, use the strict mode.
    (when (derived-mode-p 'lisp-data-mode)
      (smartparens-strict-mode 1))))

;; For the minibuffer, Smartparens does not work well. Let's use the built-in
;; `electric-pair-mode' instead.
(add-hook! 'minibuffer-mode-hook #'electric-pair-local-mode)

(after! 'smartparens
  ;; Load the default config for many languages.
  (require 'smartparens-config)

  ;; Do not show overlays.
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil)

  ;; Do not insert colons automatically for Python.
  (setq-default sp-python-insert-colon-in-function-definitions nil)

  (keybind! nil smartparens-mode-map
    ;; Rebind built-in commands to their Smartparens counterparts.
    [remap mark-sexp] #'sp-mark-sexp
    [remap forward-sexp] #'sp-forward-sexp
    [remap backward-sexp] #'sp-backward-sexp
    [remap backward-delete-char] #'sp-backward-delete-char
    [remap backward-delete-char-untabify] #'sp-backward-delete-char
    [remap backward-kill-word] #'sp-backward-delete-word
    ;; Provide additional keys.
    "C-M-d" #'sp-delete-sexp
    "C-M-h" #'sp-backward-delete-sexp
    "M-)" #'sp-unwrap-sexp
    "M-(" #'sp-backward-unwrap-sexp))

(provide 'zy-pair)

;;; zy-pair.el ends here
