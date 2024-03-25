;;; zy-terminal.el --- Terminal settings. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+terminal' module of the configuration.

;; Unlike Vim, Emacs is a graphical application with built-in support for many
;; graphical elements, including buttons, window fringes, font properties, and
;; so on. However, working in the terminal provides the following benefits:
;;
;; - It is very lightweight and blazingly fast.
;;
;; - It is handy in the edit-and-go workflow when working with the terminal.
;;
;; - It provides perfect support for character alignment, since the characters
;;   are aligned by the terminal emulator. Aligning characters are usually
;;   painful in Emacs, especially when working with mixed fonts.
;;
;; Besides, less is more. Too many icons and graphical elements distract you.
;; The terminal is simple enough for you to focus.
;;
;; Therefore, this configuration trys to provide first-class support for the
;; terminal, and try to use as few graphical-only features as possible. This
;; module specifically fix several features that is missing in the terminal by
;; default. It is assumed that the terminal Emacs is running on emulates xterm
;; and supports 256 colors.
;;
;; When using Emacs on Windows, which is what I try to avoid, I'd like to
;; disable this module and drop terminal support, since it has too many problems
;; that are very hard to fix, if not impossible.

;; commentary

;;; Code:

(require 'zylib)

(pkg! 'evil-terminal-cursor-changer)
(pkg! 'corfu-terminal "https://codeberg.org/akib/emacs-corfu-terminal")

;; Enable mouse clicks in the terminal.
(xterm-mouse-mode 1)

;; Change cursor shape by Evil state in the terminal like in GUI.
(when (modulep! '+evil)
  (evil-terminal-cursor-changer-activate))

;; Corfu-terminal makes Corfu completion UI available in the terminal by drawing
;; the completion UI with popon instead of posframe.
(when (modulep! '+corfu)
  (after! 'corfu
    (corfu-terminal-mode 1)))

(provide 'zy-terminal)

;;; zy-terminal.el ends here
