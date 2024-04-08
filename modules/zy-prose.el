;;; zy-prose.el --- Prose editing. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+prose' module of the configuration.

;; The `+prose-mode' is defined here, which is a minor mode built on top of
;; `olivetti-mode', and is specifically for prose-editing.

;;; Code:

(require 'zylib)

(pkg! 'olivetti)

(define-minor-mode +prose-mode
  "A minor mode for distraction-free prose editing."
  :group '+prose
  ;; Toggle Olivetti mode.
  (olivetti-mode 'toggle)
  ;; Toggle serif fonts.
  (when (fboundp '+font/serif-mode)
    (+font/serif-mode 'toggle))
  ;; Toggle line numbers.
  (display-line-numbers-mode 'toggle))

(after! '+leader
  (keybind! nil +leader-y-map
    "p" (cons "Prose Display" #'+prose-mode)))

(provide 'zy-prose)

;;; zy-prose.el ends here
