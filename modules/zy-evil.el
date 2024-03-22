;;; zy-evil.el --- VI emulator. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+evil' module of the configuration.

;; This module sets up Evil, the VI emulator for Emacs, and several auxiliary
;; packages like Evil-collection and Evil-surround. On top of the default Evil
;; keybindings, several tweaks are applied, like remap "f" to Avy for more
;; powerful cursor movement.

;;; Code:

(require 'zylib)

(pkg! 'evil)
(pkg! 'evil-collection)
(pkg! 'evil-surround)
(pkg! 'evil-lion)
(pkg! 'avy)
(pkg! 'consult)

(setq-default
 ;; Delete back to indentation with C-u in insert state.
 evil-want-C-u-delete t
 ;; Scroll with C-u/d in normal state.
 evil-want-C-u-scroll t
 evil-want-C-d-scroll t
 ;; Respect visual lines.
 evil-respect-visual-line-mode t
 ;; Evil-collection requires this to be nil.
 evil-want-keybinding nil
 ;; Use the built-in `undo-redo' system.
 evil-undo-system 'undo-redo)

;; Silence "`evil-want-keybinding' was set to nil but not before loading evil".
(eval-when-compile (setq-default evil-want-keybinding nil))

;; Evil mode should be activated late enought, since there might be additional
;; settings in other modules which must be loaded before Evil is loaded, for
;; instance adding keys to `evil-collection-key-blacklist'.
(add-hook! 'window-setup-hook (evil-mode 1))

(after! 'evil
  ;; Some modes activate insert state by default, but I am so accustomed to
  ;; being in normal state by default, that I always accidentally press "i" or
  ;; "a" upon entering these modes. Therefore I decided to set the initial state
  ;; of these mode to normal state to provide a consistent experience.
  (setq evil-insert-state-modes nil)

  ;; Setup Evil in many other modes with Evil-collection.
  (evil-collection-init)

  ;; Enable Evil-surround for powerful pair-editing commands (see doc).
  (global-evil-surround-mode 1)

  ;; Enable Evil-lion for powerful aligning commands (with gl or gL).
  (evil-lion-mode 1)

  ;; Restore several keys to default Emacs bindings. In my opinion these keys
  ;; are more useful than default Evil keys and allow moving around easier
  ;; without switching states. Besides, my muscle memory needs them!
  (keybind! 'insert global-map
    "C-d" #'delete-char
    "C-e" #'end-of-visual-line
    "C-a" #'beginning-of-visual-line
    "C-k" #'kill-visual-line
    "C-y" #'yank
    "C-p" #'previous-line
    "C-n" #'next-line)

  ;; Provide more motions via Avy and Consult.
  (keybind! 'motion global-map
    "f" #'avy-goto-char
    "F" #'avy-goto-char-timer
    "g c" #'evil-goto-char
    "g o" #'consult-outline
    "g '" #'consult-imenu)

  ;; I am not used to the Vim way of defining labeled macros, and I constantly
  ;; mispress "q" in order to quit a window. Remapping "q" and "Q" to the Emacs
  ;; way of defining macros makes sense to me.
  (keybind! 'normal global-map
    "Q" #'kmacro-start-macro-or-insert-counter
    "q" #'kmacro-end-or-call-macro))

(provide 'zy-evil)

;;; zy-evil.el ends here
