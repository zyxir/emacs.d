;;; init-keybindings.el --- Keybindings setup  -*- lexical-binding: t -*-
;;; Commentary:

;; This module setups Evil, which emulates the main features of Vim in Emacs.
;; Several utility packages are also configured here.

;;; Code:

(require 'init-util)

(require-package 'evil)
(require-package 'evil-collection)
(require-package 'evil-terminal-cursor-changer)
(require-package 'evil-surround)
(require-package 'evil-lion)
(require-package 'general)
(require-package 'avy)
(require-package 'consult)
(require-package 'cape)
(require-package 'consult-yasnippet)
(require-package 'embark)
(require-package 'embark-consult)
(require-package 'which-key)

;; Rewrite `general-create-definer' to silence warnings.
(defmacro zy/create-definer (name &rest defaults)
  "Create definer NAME with DEFAULTS.
This is the simplified version of `general-create-definer', and
it silences warnings."
  (declare (indent defun))
  (let ((defaults (cl-loop for (key val) on defaults by 'cddr
                           unless (eq key :wrapping)
                           collect key
                           and collect val)))
    `(defmacro ,name (&rest args)
       "A wrapper for `general-def'."
       (declare (indent defun))
       `(general-def ,@args ,@',defaults))))

;;;; Setup Evil

(setq-default
 ;; Delete back to indentation with C-u in insert state.
 evil-want-C-u-delete t
 ;; Scroll with C-u/d in normal state.
 evil-want-C-u-scroll t
 evil-want-C-d-scroll t
 ;; Use C-h as a quicker backspace.
 evil-want-C-h-delete t
 ;; Respect visual lines.
 evil-respect-visual-line-mode t
 ;; Load Evil keybindings for several other modes as well.
 evil-want-keybinding nil
 ;; Use the built-in `undo-redo' system.
 evil-undo-system 'undo-redo)

;; The leader key and the local leader key.
(defconst zy/leader-key "SPC"
  "The leader key used as a common shortcut prefix.")
(defconst zy/leader-key-insert "M-m"
  "The leader key in insert state.")
(defconst zy/local-leader-key ";"
  "The local leader key used as a mode-specific shortcut prefix.")
(defconst zy/local-leader-key-insert "M-;"
  "The local leader key in insert state.")

;; Silence "`evil-want-keybinding' was set to nil but not before loading evil".
(eval-when-compile (setq-default evil-want-keybinding nil))

(after-deferred! 'evil
  ;; Activate Evil mode.
  (evil-mode 1)

  ;; Setup Evil in many other modes.
  (setq-default
   ;; Do not bind my leader key.
   evil-collection-key-blacklist `(,zy/leader-key ,zy/local-leader-key))
  (evil-collection-init)

  ;; Use normal state, rather than insert state, for these modes.
  (evil-set-initial-state 'comint-mode 'normal)
  (evil-set-initial-state 'shell-mode 'normal)
  (evil-set-initial-state 'eshell-mode 'normal)

  ;; Also change Evil cursor in terminal.
  (evil-terminal-cursor-changer-activate)

  ;; Enable Evil-surround (for pair-editing, very powerful).
  (global-evil-surround-mode 1)

  ;; Enable Evil-lion (use gl or gL for aligning).
  (evil-lion-mode 1))

;;;; Setup Keybindings

;; Insert state customization.
(general-def
  :states 'insert
  "C-d" #'evil-delete-char
  "C-g" #'evil-force-normal-state)

;; Motion state customization.
(general-def
  :states 'motion
  "f" #'avy-goto-char
  "F" #'avy-goto-char-timer
  "g c" #'evil-goto-char
  "g o" #'consult-outline
  "g j" #'consult-imenu)

;; Normal state customization.
(general-def
  :states 'normal
  "Q" #'kmacro-start-macro-or-insert-counter
  "q" #'kmacro-end-or-call-macro)

;; Embark keys in all states.
(general-def
  :states '(normal visual)
  "," #'embark-act)
(general-def
  :maps 'global-map
  "M-," #'embark-act)

;; Insert or complete many things with "C-v" in insert state. More commands are
;; defined in other files.
(zy/create-definer zy/C-v-def
  :states 'insert
  :prefix-map 'zy/C-v-map
  :prefix "C-v")
(zy/C-v-def
  "8" #'insert-char
  "0" (defun zy/insert-zwsp ()
        "Insert a zero-width space."
        (interactive)
        (insert #x200B))
  "C-f" #'cape-file
  "C-e" #'cape-emoji
  "C-s" #'consult-yasnippet)

;; The leader definer.
(zy/create-definer zy/leader-def
  :states '(normal insert visual)
  :prefix-map 'zy/leader-map
  :prefix zy/leader-key
  :non-normal-prefix zy/leader-key-insert)

;; The local leader definer.
(zy/create-definer zy/local-leader-def
  :states '(normal insert visual)
  :prefix-map 'zy/local-leader-map
  :prefix zy/local-leader-key
  :non-normal-prefix zy/local-leader-key-insert)

;; Leader commands.
(zy/leader-def
  "SPC" #'execute-extended-command
  "0" #'delete-window
  "1" #'delete-other-windows
  "2" #'split-window-below
  "3" #'split-window-right
  "4" #'other-window-prefix
  "5" #'other-frame-prefix
  "b" #'consult-buffer
  "B" #'list-buffers
  "k" #'kill-buffer
  "s" #'save-buffer
  "," #'embark-dwim)

;; "<leader> f" for file-related operations.
(zy/create-definer zy/leader-f-def
  :keymaps 'zy/leader-map
  :prefix-map 'zy/leader-f-map
  :prefix "f")
(zy/leader-f-def
  "b" #'switch-to-buffer
  "D" #'zy/delete-file-and-buffer
  "f" #'find-file
  "r" #'revert-buffer-quick
  "c" #'consult-recent-file
  "R" #'rename-visited-file
  "s" #'save-some-buffers
  "v" #'find-alternate-file
  "V" #'zy/echo-filename
  "w" #'write-file)
(defun zy/echo-filename ()
  "Echo the value of the variable `buffer-file-name'."
  (interactive)
  (message buffer-file-name))

;; "<leader> t" for toggling many switches.
(zy/create-definer zy/leader-t-def
  :keymaps 'zy/leader-map
  :prefix-map 'zy/leader-t-map
  :prefix "t")
(zy/leader-t-def
  "c" #'corfu-mode
  "l" #'display-line-numbers-mode
  "o" #'outline-minor-mode)

;; "<leader> q" for quitting-related operations.
(zy/create-definer zy/leader-q-def
  :keymaps 'zy/leader-map
  :prefix-map 'zy/leader-q-map
  :prefix "q")
(zy/leader-q-def
 "q" #'save-buffers-kill-terminal
 "r" #'restart-emacs
 "z" #'suspend-emacs)

;; "<leader> h" for help.
(zy/leader-def
  "h" help-map)
(general-def
  :keymaps 'help-map
  "M" #'describe-keymap)

;; "<leader> c" for generic code actions.
(zy/create-definer zy/leader-c-def
  :keymaps 'zy/leader-map
  :prefix-map 'zy/leader-c-map
  :prefix "c")

(defmacro zy/create-action (action &optional eglot-action)
  "Create a placeholder command for action ACTION.

ACTION must be a string. If EGLOT-ACTION is non-nil and is a
command, call it if Eglot is available."
  (declare (indent defun))
  (let* ((fn-name (intern (format "zy/do-%s" action)))
         (eglot-action (zy/unquote eglot-action)))
    ;; Validate EGLOT-ACTION at compile time.
    (when eglot-action
      (require 'eglot)
      (unless (fboundp eglot-action)
        (error "`%s' is not a valid Eglot command" eglot-action)))
    `(defun ,fn-name (&rest _)
       ,(format
         "A placeholder command for action \"%s\"."
         action)
       (interactive)
       (if (eglot-managed-p)
           (call-interactively ',eglot-action))
       (message ,(format "Action \"%s\" is not implemented." action)))))

(zy/leader-c-def
  "x" (zy/create-action "extract" 'eglot-code-action-extract)
  "f" (zy/create-action "format" 'eglot-format)
  "i" (zy/create-action "inline" 'eglot-code-action-inline)
  "o" (zy/create-action "organize-imports" 'eglot-code-action-organize-imports)
  "q" (zy/create-action "quickfix" 'eglot-code-action-quickfix)
  "r" (zy/create-action "rename" 'eglot-rename)
  "R" (zy/create-action "rewrite" 'eglot-code-action-rewrite))

;;;; Other Tweaks

;; Show key hints with Which-key.
(after-deferred! 'which-key
  (which-key-mode 1)
  (setq which-key-idle-delay 0.5))

(provide 'init-keybindings)

;;; init-keybindings.el ends here
