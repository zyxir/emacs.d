;;; zy-leader.el --- Leader key setup. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `leader' module of Zyxir's Emacs configuration.
;;
;; This module sets up several keymaps and some keybindings around the leader
;; key.
;;
;; It is worth noting that the leader key is defined in the `evil' module,
;; because the function used to define it requires loading Evil.
;;
;; This file is not the only place to create keymaps and bind keys. Most
;; keybindings are scattered across the modules to make this configuration more
;; modular. But all leader-started prefixes should be defined here with the
;; `defprefix!' macro.

;;; Code:

(require 'zylib)

(defprefix! zy-leader-map "Leader"
            nil 'global "<leader>"
  ;; Quick commands with leader plus a single key.
  "<leader>" #'execute-extended-command
  "b" '("Switch Buffer" . switch-to-buffer)
  "d" '("Dired" . dired)
  "B" '("List Buffers" . list-buffers)
  "s" '("Save Buffer" . save-buffer)
  "k" '("Kill Buffer" . kill-buffer)
  "w" `("Other Window ..." . ,ctl-x-4-map)
  "t" `("Other Tab ..." . ,tab-bar-map))

;; Make `zy-leader-map' universally available using Evil intercept keymaps.
(after! 'evil
  (dolist (state '(normal motion visual insert emacs))
    (evil-get-auxiliary-keymap zy-leader-map state 'create 'ignore-parent)
    state))

(provide 'zy-leader)

;;; zy-leader.el ends here
