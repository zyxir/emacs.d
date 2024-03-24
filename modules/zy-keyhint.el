;;; zy-keyhint.el --- Shortcut hints. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+keyhint' module of the configuration.

;; In a keyboard-driven editing flow, getting hints for keystrokes is vital.
;; However, typing "C-h k" requires breaking out of the current keystroke, and
;; typing "C-h" triggers a window which is not very helpful sometimes.
;;
;; This module incorporates Which-key, a package which provides automatic,
;; well-structured, and pretty key hints after a short delay if you press some
;; key. It is very helpful and is a necessity for any Emacs configuration in my
;; opinion.

;;; Code:

(require 'zylib)

(pkg! 'which-key)

(add-hook! 'window-setup-hook
  (eval-and-compile (require 'which-key))

  ;; Show key hints initially after 1.0 seconds, and 0.5 seconds afterwards.
  (setq which-key-idle-delay 1.0
        which-key-idle-secondary-delay 0.5)

  ;; Sort keys alphabetically.
  (setq which-key-sort-order #'which-key-key-order-alpha)

  (which-key-mode 1))

(provide 'zy-keyhint)

;;; zy-keyhint.el ends here
