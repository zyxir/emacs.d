;;; zy-pair.el --- Pair-editing. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+pair' module of the configuration.

;; This module sets up Smartparens, a powerful pair-editing suite. Although it
;; is mostly intended for Emacs keybindings or the insert state, it can be made
;; useful with Evil-cleverparens and text objects.

;;; Code:

(require 'zylib)

;; As of 2024-03-28, Smartparens in ELPA and Melpa-stable was updated in 2017,
;; lacks many useful functions and bugfixes, and even requires the deprecated
;; feature `cl', whereas the Melpa version was updated within this month. I have
;; to pin it to Melpa to provide a better experience.
(pin-to! "melpa" 'smartparens)

(pkg! 'smartparens)
(pkg! 'evil-cleverparens)

;; Use Smartparens for most modes.
(add-hook! '(prog-mode-hook
             text-mode-hook
             conf-mode-hook
             minibuffer-mode-hook)
  (smartparens-mode 1)

  ;; If the mode is a Lisp mode, use the strict mode.
  (when (derived-mode-p 'lisp-data-mode)
    (smartparens-strict-mode 1))

  ;; Also enable Cleverparens.
  (evil-cleverparens-mode 1))

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
    ;; Provide additional keys. We don't need too many of these commands because
    ;; Cleverparens provides many useful Evil-style commands.
    "C-M-d" #'sp-delete-sexp
    "C-M-h" #'sp-backward-delete-sexp
    "M-)" #'sp-unwrap-sexp
    "M-(" #'sp-backward-unwrap-sexp))

;; The default keybindings of Cleverparens is stupid. For its movement keys,
;; it overrides "[" and "]", which shadows a lot of useful keybindings
;; prefixed by them, especially those \"unimpaired\" ones. Besides, it uses
;; "M-]" as a wrapping key, which is originally used to quote escape sequences
;; in a terminal. As a result, any mouse movement triggers a bunch of
;; Cleverparens commands, essentially messing up the buffer content, which is
;; a disaster. Therefore, it is vital to stop Cleverparens from binding these
;; keys, and provide my own bindings.
(setq-default evil-cleverparens-use-additional-movement-keys nil
              evil-cleverparens-use-additional-bindings nil)

;; Therefore, bind keys myself.
(after! '(smartparens evil-cleverparens)
  (keybind! 'motion evil-cleverparens-mode-map
    "H" #'evil-cp-backward-sexp
    "L" #'evil-cp-forward-sexp
    "M-J" #'sp-join-sexp
    "M-s" #'sp-splice-sexp
    "M-S" #'sp-split-sexp
    "M-t" #'sp-transpose-sexp
    "M-v" #'sp-convolute-sexp
    "M-r" #'sp-raise-sexp
    "+" #'sp-forward-barf-sexp
    "-" #'sp-backward-barf-sexp
    "M-}" #'sp-forward-slurp-sexp
    "M-{" #'sp-backward-slurp-sexp))

(provide 'zy-pair)

;;; zy-pair.el ends here
