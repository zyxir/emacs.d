;;; zy-undo.el --- Undo/redo. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+undo' module of the configuration.

;; Emacs has built-in support for linear undo/redo starting from version 28.1,
;; and it is very handy. This module intends to introduce two more features to
;; the undo system: persistent undo/redo history across sessions, and undo
;; visualization.

;;; Code:

(require 'zylib)

(pkg! 'vundo)
(pkg! 'undo-fu-session)

;; Always save undo history with Undo-fu-session.
(add-hook! 'window-setup-hook
  (undo-fu-session-global-mode 1))

(after! 'undo-fu-session
  ;; Limit the number of saved files to 200, the same as the limit of recentf.
  (setq undo-fu-session-file-limit 200))

;; Use Vundo with a shortcut.
(keybind! 'motion 'global "U" #'vundo)

(provide 'zy-undo)

;;; zy-undo.el ends here
