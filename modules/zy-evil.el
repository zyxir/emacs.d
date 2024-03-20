;;; zy-evil.el --- VI emulator. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `evil' module of Zyxir's Emacs configuration.

;; This module sets up Evil, the VI emulator for Emacs, and several auxiliary
;; packages like Evil-collection and Evil-surround. On top of the default Evil
;; keybindings, several tweaks are applied, like remap "f" to Avy for more
;; powerful cursor movement.

;;; Code:

(require 'zylib)

(pkg! evil)
(pkg! evil-collection)
(pkg! evil-surround)
(pkg! evil-lion)
(pkg! avy)
(pkg! consult)

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

(add-hook! 'window-setup-hook (evil-mode 1))

(after! 'evil
  ;; Define the space key as the leader key, like some popular editor
  ;; configurations do, including SpaceVim, Spacemacs, and Doom Emacs.
  (evil-set-leader '(normal visual motion operator) (kbd "SPC"))

  ;; Some modes activate insert state by default, but I am so accustomed to
  ;; being in normal state by default, that I always accidentally press "i" or
  ;; "a" upon entering these modes. Therefore I decided to set the initial state
  ;; of every mode to normal state to provide a consistent experience.
  (evil-set-initial-state 'comint-mode 'normal)
  (evil-set-initial-state 'shell-mode 'normal)
  (evil-set-initial-state 'eshell-mode 'normal)

  ;; Setup Evil in many other modes with Evil-collection.
  (evil-collection-init)

  ;; Enable Evil-surround for powerful pair-editing commands (see doc).
  (global-evil-surround-mode 1)

  ;; Enable Evil-lion for powerful aligning commands (with gl or gL).
  (evil-lion-mode 1)

  ;; It's nmemonic to use "M-SPC" (Alt-Space) as the leader key in insert state,
  ;; but this combination is often used by the window manager (as in Microsoft
  ;; Windows and many Linux desktop environments) to open window menu. Therefore
  ;; use "M-m" instead.
  (evil-set-leader '(insert emacs) "M-m")

  ;; The local leader key is used to perform mode-specific operations. "," (the
  ;; comma key) is a good choice. Since "M-," is available almost anywhere, use
  ;; it as the local leader key in insert state.
  (evil-set-leader '(normal visual motion) "," 'localleader)
  (evil-set-leader '(insert emacs) "M-," 'localleader)

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
