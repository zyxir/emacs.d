;;; zylib-key.el --- Keybinding utility. -*- lexical-binding: t -*-
;;; Commentary:

;; This file defines the keybinding framework of the configuration. It provides
;; two simple macros, `keybind!' and `defprefix!', for an efficient and
;; consistent keybinding interface and automatic loading management.
;;
;; Due to the deep coupling of the keybinding framework with Evil, it is hard to
;; implement these utilities without Evil. Therefore Evil is the only package
;; that is required in Zylib. Nevertheless, this file trys to only use the
;; utility functions provided to Evil, and leave all customizations of Evil to
;; the `evil' module, including choosing the (local) leader key.

;;; Code:

(require 'zylib-pkg)

(pkg! evil)

(defmacro keybind! (state keymap key def &rest bindings)
  "Create a STATE binding from KEY to DEF for KEYMAP.

STATE is one of the Evil states, or a list of one or more of
them. Omitting a state by using nil corresponds to a standard
Emacs binding using ‘define-key’.

KEYMAP is a keymap to define the binding in. If KEYMAP is the
quoted symbol `global', the global evil keymap corresponding to
the state(s) is used.

KEY and DEF are like those in `define-key', except that KEY is a
human-readable string which can be converted to a sequence of
keystrokes via the `kbd' function.

It is possible to specify multiple KEY and DEF pairs in BINDINGS.

\(fn STATE KEYMAP KEY DEF [KEY DEF])"
  (declare (indent 2))
  (let* ((bindings (append `(,key ,def) bindings))
         (index 0)
         (cur nil)
         (wrapped-bindings '()))
    ;; Wrap all keys in `bindings' with `kbd'.
    (while bindings
      (setq cur (pop bindings))
      (push (if (eq (% index 2) 0) `(kbd ,cur) cur) wrapped-bindings)
      (setq index (+ index 1)))
    (setq wrapped-bindings (reverse wrapped-bindings))
    `(eval-after-load 'evil
       #'(lambda ()
           (evil-define-key* ,state ,keymap ,@wrapped-bindings)))))

(defmacro defprefix! (command name state keymap key &rest bindings)
  "Define COMMAND as a prefix command. COMMAND should be a symbol.

A new sparse keymap is stored as COMMAND's function definition
and its value. NAME is the menu name string for the map.

Bind this prefix command to KEY in STATE and KEYMAP as per
`keybind!'.

You may continue to define keybindings using KEY and DEF pairs as
per `keybind!' in BINDINGS."
  (declare (indent 5))
  (let ((form `(prog1
                   (define-prefix-command ',command nil ,name)
                 (keybind! ,state ,keymap ,key ,command)))
        (keybind-form (when bindings
                        `(keybind! nil ,command ,@bindings))))
    (append form `(,keybind-form))))

(provide 'zylib-key)

;;; zylib-key.el ends here
