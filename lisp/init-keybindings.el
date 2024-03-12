;;; init-keybindings.el --- Keybindings setup  -*- lexical-binding: t -*-
;;; Commentary:

;; This module setups Evil, which emulates the main features of Vim in Emacs.
;; Several utility packages are also configured here.

;;; Code:

(require-package 'evil)
(require-package 'evil-collection)
(require-package 'evil-terminal-cursor-changer)
(require-package 'general)
(require-package 'consult)
(require-package 'embark)
(require-package 'embark-consult)
(require-package 'which-key)


;; Setup Evil
(setq
 ;; Delete back to indentation with C-u in insert state.
 evil-want-C-u-delete t
 ;; Scroll with C-u/d in normal state.
 evil-want-C-u-scroll t
 evil-want-C-d-scroll t
 ;; Respect visual lines.
 evil-respect-visual-line-mode t
 ;; Load Evil keybindings for several other modes as well.
 evil-want-keybinding nil)
(evil-mode 1)

;; The leader key and the local leader key.
(defconst zy/leader-key "SPC"
  "The leader key used as a common shortcut prefix.")
(defconst zy/local-leader-key ","
  "The local leader key used as a mode-specific shortcut prefix.")

;; Setup Evil in many other modes.
(setq
 ;; Do not bind my leader key.
 evil-collection-key-blacklist `(,zy/leader-key ,zy/local-leader-key)
 ;; Do not bind unimpaired keys.
 evil-collection-want-unimpaired-p nil)
(evil-collection-init)

;; Also change Evil cursor in terminal.
(evil-terminal-cursor-changer-activate)


;; Setup Embark

;; Embark keys in all states.
(general-def
  :states '(normal visual)
  "'" #'embark-act)
(general-def
  :maps 'global-map
  "M-'" #'embark-act)


;; Setup Keybindings

;; Remap (or cancel) some "goto" keys.
(general-def
  :states 'motion
  "g c" #'evil-goto-char)

;; The leader key.
(general-create-definer zy/leader-def
  :keymaps '(normal insert visual)
  :prefix-map 'zy/leader-map
  :prefix "SPC"
  :non-normal-prefix "M-SPC")

;; Leader commands.
(zy/leader-def
  "0" #'delete-window
  "1" #'delete-other-windows
  ;; TODO remap 2 and 3 to other more useful commands.
  "2" #'split-window-below
  "3" #'split-window-right
  "4" #'other-window-prefix
  "5" #'other-frame-prefix
  "b" #'consult-buffer
  "k" #'kill-buffer
  "s" #'save-buffer
  ";" #'embark-dwim)

;; "<leader> f" for file-related operations.
(general-create-definer zy/leader-f-def
  :keymaps 'zy/leader-map
  :prefix-map 'zy/leader-f-map
  :prefix "f")
(zy/leader-f-def
  "b" #'switch-to-buffer
  "D" #'zy/delete-file-and-buffer
  "f" #'find-file
  "g" #'revert-buffer-quick
  "r" #'consult-recent-file
  "R" #'zy/rename-file-and-buffer
  "s" #'save-some-buffers
  "w" #'write-file)

;; "<leader> q" for quitting-related operations.
(general-create-definer zy/leader-q-def
  :keymaps 'zy/leader-map
  :prefix-map 'zy/leader-q-map
  :prefix "q")
(zy/leader-q-def
 "q" #'save-buffers-kill-terminal
 "r" #'restart-emacs)

;; "<leader> h" for help.
(zy/leader-def
  "h" help-map)

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

;; The omnipotent "C-g".
(defun zy/C-g (&rest _)
  "Do \"C-g\" action depending on the context."
  (interactive)
  (cond
   (;; While completing with Corfu, quit the completion.
    (or corfu--frame corfu-terminal--popon)
    (corfu-quit))
   (;; While in insert state, go to normal state.
    (eq evil-state 'insert)
    (evil-force-normal-state))
   (;; In other cases, do `keyboard-quit'.
    t (keyboard-quit))))
(general-def
  :states '(insert normal)
  "C-g" #'zy/C-g)

;; Show key hints with Which-key.
(setq which-key-idle-delay 0.5)
(which-key-mode 1)

(provide 'init-keybindings)

;;; init-keybindings.el ends here
