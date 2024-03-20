;;; zylib-keybind.el --- Keybinding utility. -*- lexical-binding: t -*-
;;; Commentary:

;; This file defines the leader-key based keybinding framework of Zyxir's Emacs
;; configuration:
;;
;;   - Most shortcuts are prefixed with the leader key.
;;   - Local shortcuts are prefixed with the local leader key.
;;   - The (local) leader key is defined when Evil is loaded.
;;
;; Due to the deep coupling of the keybinding framework with Evil, it is hard to
;; implement these utilities without Evil. Therefore Evil is the only package
;; that is required in Zylib. Nevertheless, this file trys to only use the
;; utility functions provided to Evil, and leave all customizations of Evil to
;; the `evil' module, including choosing the (local) leader key.

;;; Code:

(require 'zylib-pkg)

(pkg! evil)

(defmacro defkey! (state keymap key def &rest bindings)
  "Create a STATE binding from KEY to DEF for KEYMAP.

STATE is one of the Evil states, or a list of one or more of
them. Omitting a state by using nil corresponds to a standard
Emacs binding using ‘define-key’.

KEYMAP is a keymap to define the binding in. If KEYMAP is the
quoted symbol `global', the global evil keymap corresponding to
the state(s) is used.

KEY and DEF are like those in `define-key', except that KEY is a
human-readable string which will be converted to a sequence of
keystrokes via the `kbd' function.

It is possible to specify multiple KEY and DEF pairs in BINDINGS.

\(fn STATE KEYMAP KEY DEF [KEY DEF])"
  (let* ((key (kbd key))
         ;; TODO: Wrap all KEYs in BINDINGS with `kbd'.
         )
   `(eval-after-load 'evil
     #'(lambda ()
         (evil-define-key* ,state ,keymap ,key ,def ,@bindings)))))

(provide 'zylib-keybind)

;;; zylib-keybind.el ends here
