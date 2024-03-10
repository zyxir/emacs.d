;;; init-keybindings.el --- Keybindings setup  -*- lexical-binding: t -*-
;;; Commentary:

;; This module setups Evil, which emulates the main features of Vim in Emacs.
;; Several utility packages are also configured here.

;;; Code:

(require-package 'evil)
(require-package 'evil-collection)
(require-package 'general)
(require-package 'consult)
(require-package 'embark)
(require-package 'embark-consult)
(require-package 'which-key)

;; Setup Evil.
(setq
 ;; Delete back to indentation with C-u in insert state.
 evil-want-C-u-delete t
 ;; Scroll with C-u/d in normal state.
 evil-want-C-u-scroll t
 evil-want-C-d-scroll t
 ;; Use Evil's own interactive search.
 evil-search-module 'evil-search
 ;; Respect visual lines.
 evil-respect-visual-line-mode t
 ;; Load Evil keybindings for several other modes as well.
 evil-want-keybinding nil)
(evil-mode 1)

;; Setup Evil in many other modes.
(evil-collection-init '(dired
                        magit
                        wdired))

;; The leader key.
(general-create-definer zy/leader-def
  :keymaps '(normal insert emacs)
  :prefix-map 'zy/leader-map
  :prefix "SPC"
  :non-normal-prefix "M-SPC")

;; Single key commands.
(zy/leader-def
  "0" #'delete-window
  "1" #'delete-other-window
  ;; TODO remap 2 and 3 to other more useful commands.
  "2" #'split-window-below
  "3" #'split-window-right
  "4" #'other-window-prefix
  "5" #'other-frame-prefix
  "SPC" #'embark-act
  "b" #'consult-buffer
  "s" #'save-buffer)

;; "<leader> f" for file-related operations.
(general-create-definer zy/leader-f-def
  :keymaps 'zy/leader-map
  :prefix-map 'zy/leader-f-map
  :prefix "f")
(zy/leader-f-def
  "b" #'switch-to-buffer
  "D" #'zy/delete-file-and-buffer
  "f" #'find-file
  "r" #'consult-recent-file
  "R" #'zy/rename-file-and-buffer
  "s" #'save-some-buffers
  "w" #'write-file)

;; Show key hints with Which-key.
(setq which-key-idle-delay 0.5)
(which-key-mode 1)

(provide 'init-keybindings)

;;; init-keybindings.el ends here
