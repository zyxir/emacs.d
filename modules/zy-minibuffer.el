;;; zy-minibuffer.el --- Vertico completion UI. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+minibuffer' module of the configuration.

;; Apart from some minibuffer tweaks, it sets up Vertico as a vertical and
;; interactive minibuffer completion UI which replaces the default one. The
;; package Marginalia provides additional annotations to completion candidates,
;; further enhancing the completion experience.
;;
;; Another important package is Stillness-mode, which prevents the minibuffer
;; from resizing the windows.

;;; Code:

(require 'zylib)

(pkg! 'vertico)
(pkg! 'marginalia)
(pkg! 'stillness-mode :url "https://github.com/neeasade/stillness-mode.el")

(defun +minibuffer-crm-indicator (args)
  "Indicator for `completing-read-multiple'.

ARGS are the arguments passed."
  (defvar crm-separator)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))

(add-hook! 'window-setup-hook
  ;; Allow minibuffer commands while in the minibuffer.
  (setq enable-recursive-minibuffers t)
  ;; And show current minibuffer depth.
  (minibuffer-depth-indicate-mode 1)

  ;; Use shorter format string for default value.
  (setq minibuffer-default-prompt-format " [%s]")
  ;; And do not show default value if a user value is being typed.
  (minibuffer-electric-default-mode 1)

  ;; Do not allow the cursor to move inside the minibuffer prompt.
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable Vertico.
  (vertico-mode 1)
  (eval-and-compile (require 'vertico))

  ;; Use "C-j" and "C-RET" for force exit since "M-RET" is occupied by default
  ;; in Windows Terminal. It is worth noting that "C-RET" equals to "C-j" in
  ;; terminal.
  (keybind! nil vertico-map
    "C-j" #'vertico-exit-input
    "C-RET" #'vertico-exit-input)

  ;; Show candidate info with Marginalia.
  (marginalia-mode 1)

  (advice-add #'completing-read-multiple :filter-args
              #'+minibuffer-crm-indicator)

  ;; Enable stillness mode.
  (stillness-mode 1))

(provide 'zy-minibuffer)

;;; zy-minibuffer.el ends here
