;;; init-keybindings.el --- Keybindings setup  -*- lexical-binding: t -*-
;;; Commentary:

;; This module setups Evil, which emulates the main features of Vim in Emacs.
;; Several utility packages are also configured here.

;;; Code:

(require-package 'evil)
(require-package 'evil-collection)
(require-package 'evil-terminal-cursor-changer)
(require-package 'evil-surround)
(require-package 'evil-lion)
(require-package 'general)
(require-package 'avy)
(require-package 'consult)
(require-package 'embark)
(require-package 'embark-consult)
(require-package 'which-key)


;;;; Setup Evil

(setq
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
(evil-mode 1)

;; The leader key and the local leader key.
(defconst zy/leader-key "SPC"
  "The leader key used as a common shortcut prefix.")
(defconst zy/leader-key-insert "M-m"
  "The leader key in insert state.")
(defconst zy/local-leader-key ";"
  "The local leader key used as a mode-specific shortcut prefix.")
(defconst zy/local-leader-key-insert "M-;"
  "The local leader key in insert state.")

;; Setup Evil in many other modes.
(setq
 ;; Do not bind my leader key.
 evil-collection-key-blacklist `(,zy/leader-key ,zy/local-leader-key))
(evil-collection-init)

;; Set proper initial states of some modes.
(evil-set-initial-state 'comint-mode 'normal)

;; Also change Evil cursor in terminal.
(evil-terminal-cursor-changer-activate)

;; Enable Evil-surround (for pair-editing, very powerful).
(global-evil-surround-mode 1)

;; Enable Evil-lion (use gl or gL for aligning).
(evil-lion-mode 1)


;;;; Tweak Evil Keybindings

;; Extra insert state customization.
(general-def
  :states 'insert
  "C-d" #'evil-delete-char
  "C-g" #'evil-force-normal-state)

;; Embark keys in all states.
(general-def
  :states '(normal visual)
  "," #'embark-act)
(general-def
  :maps 'global-map
  "M-," #'embark-act)

;; Remap `evil-find-char' to Avy.
(general-def
  :states 'motion
  "f" #'avy-goto-char
  "F" #'avy-goto-char-timer)

;; Remap "q" and "Q" to Emacs-style macro.
(general-def
  :states 'normal
  "Q" #'kmacro-start-macro-or-insert-counter
  "q" #'kmacro-end-or-call-macro)

;; Remap (or cancel) some "goto" keys.
(general-def
  :states 'motion
  "g c" #'evil-goto-char)

;; Insert or complete many things with "C-v" in insert state. More commands are
;; defined in other files.
(general-create-definer zy/C-v-def
  :keymaps 'insert
  :prefix-map 'zy/C-v-map
  :prefix "C-v")
(zy/C-v-def
  "8" #'insert-char
  "0" (defun zy/insert-zwsp ()
        "Insert a zero-width space."
        (interactive)
        (insert #x200B)))


;;;; Setup Leader Keys

;; The leader definer.
(general-create-definer zy/leader-def
  :states '(normal insert visual)
  :prefix-map 'zy/leader-map
  :prefix zy/leader-key
  :non-normal-prefix zy/leader-key-insert)

;; The local leader definer.
(general-create-definer zy/local-leader-def
  :states '(normal insert visual)
  :prefix-map 'zy/local-leader-map
  :prefix zy/local-leader-key
  :non-normal-prefix zy/local-leader-key-insert)

;; Leader commands.
(zy/leader-def
  "SPC" #'execute-extended-command
  "0" #'delete-window
  "1" #'delete-other-windows
  ;; TODO remap 2 and 3 to other more useful commands.
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
(general-create-definer zy/leader-f-def
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
  "Echo `buffer-file-name'."
  (interactive)
  (message buffer-file-name))


;; "<leader> t" for toggling many switches.
(general-create-definer zy/leader-t-def
  :keymaps 'zy/leader-map
  :prefix-map 'zy/leader-t-map
  :prefix "t")
(zy/leader-t-def
  "c" #'corfu-mode
  "l" #'display-line-numbers-mode
  "o" #'outline-minor-mode)


;; "<leader> q" for quitting-related operations.
(general-create-definer zy/leader-q-def
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
(general-create-definer zy/leader-c-def
  :keymaps 'zy/leader-map
  :prefix-map 'zy/leader-c-map
  :prefix "c")

(defmacro zy/create-action (action &optional eglot-action)
  "Create a placeholder command for action ACTION.

ACTION must be a string. If EGLOT-ACTION is non-nil and is a
command, call it if Eglot is available."
  (declare (indent defun))
  (let* ((fn-name (intern (format "zy/do-%s" action))))
    `(defun ,fn-name (&rest _)
       ,(format
         "A placeholder command for action \"%s\".

If `eglot-managed-p' returns non-nil, call `%s' instead."
         action eglot-action)
       (interactive)
       (if (eglot-managed-p)
           (call-interactively ,eglot-action))
       (message ,(format "Action \"%s\" is not implemented." action)))))

(zy/leader-c-def
  "x" (zy/create-action "extract" #'eglot-code-action-extract)
  "f" (zy/create-action "format" #'eglot-format)
  "i" (zy/create-action "inline" #'eglot-code-action-inline)
  "o" (zy/create-action "organize-imports" #'eglot-code-action-organize-imports)
  "q" (zy/create-action "quickfix" #'eglot-code-action-quickfix)
  "r" (zy/create-action "rename" #'eglot-rename)
  "R" (zy/create-action "rewrite" #'eglot-code-action-rewrite))


;; Other Tweaks

;; Show key hints with Which-key.
(setq which-key-idle-delay 0.5)
(which-key-mode 1)

(provide 'init-keybindings)

;;; init-keybindings.el ends here
