;;; zy-embark.el --- Contextual menu on objects. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+embark' module of the configuration.

;; Embark is a revolutionary package in my view. It collect the "thing at
;; point", and provides a context menu of all operations available. It makes
;; multi-level selection, context-aware operation, and actions on minibuffer
;; entries so much easier, and reduces many unnecessary keybindings. It is a
;; lifesaver, and I always bind it to the most accessible key.
;;
;; However, its default keybindings resemble the Emacs keybindings. I'd like to
;; remap them to be more mnemonic in the Evil environment.

;;; Code:

(require 'zylib)

(pkg! 'embark)
(pkg! 'consult)
(pkg! 'embark-consult)

(daemon-require! 'embark)

;; Use ";", a key easily accessible by your right pinky, for Embark. Use "M-g"
;; instead in insert state. Why don't we use "M-;" here? Because the key is used
;; as `comment-dwim' by default and is used by Embark keymaps. I always use it
;; to toggle comments and it's so hard for me to change this musle memory!
(keybind! 'motion 'global
  ";" (cons "Embark" #'embark-act))
(keybind! nil 'global
  "M-g" (cons "Embark" #'embark-act))

;; Use the ";" key for cycling.
(setq-default embark-cycle-key ";")

;; Add (or replace) some Vim-styled keys.
(after! '(embark evil)
  (keybind! nil embark-general-map
    "C-s" nil
    "C-r" nil
    "/" #'evil-ex-search-forward
    "?" #'evil-ex-search-backward
    "y" #'embark-copy-as-kill)

  (keybind! nil embark-region-map
    "TAB" nil
    "=" #'indent-region
    "C-/" #'comment-or-uncomment-region))

;; "gx" was `browse-url-at-point'. Let's bind it to `embark-dwim', which can do
;; what `browse-url-at-point' does, and much more than that.
(keybind! 'normal 'global
  "g x" #'embark-dwim)

(provide 'zy-embark)

;;; zy-embark.el ends here
