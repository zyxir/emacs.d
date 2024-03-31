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

;; Scroll up/down with C-u/d in normal state, as Vim does.
(setq-default evil-want-C-u-scroll t
              evil-want-C-d-scroll t)

;; Use C-h as an alternate Backspace. Useful for further customizations in the
;; `+pair' module.
(setq-default evil-want-C-h-delete t)

;; Respect visual lines. This means that movement commands move according to
;; visual lines, rather than actual lines.
(setq-default evil-respect-visual-line-mode t)

;; When `evil-want-keybinding' is unset, Evil doesn't setup keys for many major
;; modes, so that Evil-collection can set its keybindings up instead. Wrapping
;; this line in `eval-and-compile' silence the "`evil-want-keybinding' was set
;; to nil but not before loading evil" warning during byte compilation.
(eval-and-compile (setq-default evil-want-keybinding nil))

;; Use the built-in `undo-redo' system introduced in Emacs 28.1.
(setq-default evil-undo-system 'undo-redo)

;; Use Evil's search module, which supports Chinese input methods, and is more
;; Vimmy. Further customization is in the `+search' module.
(setq-default evil-search-module 'evil-search)

;; Evil mode should be loaded late enough, since there might be additional
;; settings in other modules which must be loaded before loading Evil, for
;; instance adding keys to `evil-collection-key-blacklist'. However, Evil should
;; also be loaded before any buffer is created, which happens in hooks like
;; `after-init-hook', `emacs-startup-hook', and `window-setup-hook'. Now we make
;; sure that Evil is always loaded first at `after-init-hook'.
(add-hook! 'after-init-hook :depth -90
  (require 'evil))

(after! 'evil
  ;; Enable Evil mode. If Evil mode is not enabled beforehand, some of the
  ;; following configuration might fail. For instance,
  ;; `evil-collection-unimpaired-mode' keybindings may be bugged until the user
  ;; switch state manually.
  (evil-mode 1)

  ;; Some modes activate insert state by default, but I am so accustomed to
  ;; being in normal state by default, that I always accidentally press "i" or
  ;; "a" upon entering these modes. Therefore I decided to set the initial state
  ;; of these mode to normal state to provide a consistent experience.
  (setq evil-insert-state-modes nil)

  ;; Do not echo the current state in the echo area, as it blocks ELdoc
  ;; sometimes.
  (setq evil-echo-state nil)

  ;; Setup Evil in many other modes with Evil-collection.
  (evil-collection-init)

  ;; Enable Evil-surround for powerful pair-editing commands (see doc).
  (global-evil-surround-mode 1)

  ;; Enable Evil-lion for powerful aligning commands (with gl or gL).
  (evil-lion-mode 1)

  ;; Clear search highlights by pressing "C-l".
  (advice-add
   #'recenter-top-bottom :after
   (defun +evil-clear-highlights (&rest _)
     "Clear Evil search highlights."
     (evil-ex-nohighlight)))

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
    "q" #'kmacro-end-or-call-macro)

  ;; More Unimpaired style keybindings.
  (keybind! 'motion global-map
    "[ t" #'tab-previous
    "] t" #'tab-next))

(provide 'zy-evil)

;;; zy-evil.el ends here
