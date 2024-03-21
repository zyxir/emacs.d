;;; zy-leader.el --- Leader key setup. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+leader' module of the configuration.
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

(pkg! consult)
(pkg! which-key)

;; Define the (local) leader key after loading Evil.
(after! 'evil
  ;; Define the space key as the leader key, like some popular editor
  ;; configurations do, including SpaceVim, Spacemacs, and Doom Emacs.
  ;;
  ;; Setting it in motion state automatically works for normal, visual, and
  ;; operator states, makes keybindings cleaner, and makes the menu entries
  ;; correctly show up after pressing the leader key (If set in normal state,
  ;; the menu entries don't show up correctly).
  (evil-set-leader 'motion (kbd "SPC"))

  ;; It's nmemonic to use "M-SPC" (Alt-Space) as the leader key in insert state,
  ;; but this combination is often used by the window manager (as in Microsoft
  ;; Windows and many Linux desktop environments) to open window menu. Therefore
  ;; use "M-m" instead.
  (evil-set-leader 'insert (kbd "M-m"))

  ;; The local leader key is used to perform mode-specific operations. "," (the
  ;; comma key) is a good choice. Since "M-," is available almost anywhere, use
  ;; it as the local leader key in insert state.
  (evil-set-leader 'motion (kbd ",") 'localleader)
  (evil-set-leader 'insert (kbd "M-,") 'localleader))

;; Unmap the occupied leader key in these keymaps.
;;
;; I've searched extensively on the Internet about "how to make my leader key
;; available in all modes, including dired-mode and help-mode". There are many
;; related contents with all sorts of different solutions. The most recommended
;; is to use the `override' keymap with general.el. Since I decided to not use
;; general.el in my configuration, I have to figure out a solution myself.
;; Unbinding the occupied keys in these special modes seems the most reliable
;; way to me.
(dolist (feature-map-pair `((help-mode . ,help-mode-map)))
  (let ((feature (car feature-map-pair))
        (map (cdr feature-map-pair)))
    (with-eval-after-load feature
      (keybind! '(normal visual motion operator) map "SPC" nil)
      (keybind! 'insert map "M-m" nil))))

;; Tell Evil-collection to not touch my leader keys.
(defvar evil-collection-key-blacklist nil)
(add-to-list 'evil-collection-key-blacklist "SPC")
(add-to-list 'evil-collection-key-blacklist "M-m")

;; As previously mentioned, it is only necessary to bind the map to motion and
;; insert state.
(defprefix! +leader-map "Leader"
            '(motion insert) 'global "<leader>"
  ;; Quick commands with leader plus a single key.
  "<leader>" '("Execute..." . execute-extended-command)
  "b" '("Switch to Buffer" . consult-buffer)
  "B" '("Switch to Buffer*" . switch-to-buffer)
  "C-b" '("List Buffers" . list-buffers)
  "d" '("Dired" . dired)
  "s" '("Save Buffer" . save-buffer)
  "k" '("Kill Buffer" . kill-buffer))

(defprefix! +leader-c-map "Code"
            nil +leader-map "c")

(defprefix! +leader-f-map "File"
            nil +leader-map "f"
  "f" '("Open" . find-file)
  "r" '("Revert" . revert-buffer-quick)
  "c" '("Recent" . consult-recent-file)
  "R" '("Rename" . rename-visited-file)
  "s" '("Save" . save-buffer)
  "S" '("Mass Save" . save-some-buffers)
  "w" '("Save As..." . write-file))

(defprefix! +leader-h-map "Help"
            nil +leader-map "h"
  ;; Some of these keys mirror those of `help-map', but not all of them do. For
  ;; example, "F" is `Info-goto-emacs-command-node' in `help-map', but is
  ;; `describe-face' here.
  "f" '("Function" . describe-function)
  "v" '("Variable" . describe-variable)
  "o" '("Symbol" . describe-symbol)
  "m" '("Mode" . describe-mode)
  "k" '("Key" . describe-key)
  "M" '("Keymap" . describe-keymap)
  "F" '("Face" . describe-face))

(defprefix! +leader-p-map "Project"
            nil +leader-map "p"
  "p" '("Switch" . project-switch-project)
  "f" '("File" . project-find-file)
  "d" '("Dir" . project-find-dir)
  "b" '("Buffer" . project-switch-to-buffer)
  "v" '("VC" . project-vc-dir))

(defprefix! +leader-q-map "Quit"
            nil +leader-map "q"
  "q" '("Quit" . save-buffers-kill-terminal)
  "r" '("Restart" . restart-emacs)
  "z" '("Suspend" . suspend-emacs))

(other-tabbed! +leader-p-other-tab-map +leader-p-map)

(defprefix! +leader-t-map "Tab"
            nil +leader-map "t"
  "n" '("New Tab" . tab-new)
  "t" '("Other Tab Prefix" . other-tab-prefix)
  "c" '("Close Tab" . tab-close)
  "b" '("Buffer" . consult-buffer-other-tab)
  "d" '("Dired" . dired-other-tab)
  "f" '("File" . find-file-other-tab)
  "p" '("Project" . +leader-p-other-tab-map))

(other-windowed! +leader-p-other-window-map +leader-p-map)

(defprefix! +leader-w-map "Other Window..."
            nil +leader-map "w"
  "c" '("Close Window" . delete-window)
  "w" '("Other Window Prefix" . other-window-prefix)
  "b" '("Buffer" . consult-buffer-other-window)
  "d" '("Dired" . dired-other-window)
  "f" '("File" . find-file-other-window)
  "p" '("Project" . +leader-p-other-window-map))

(defprefix! +leader-y-map "Toggle..."
            nil +leader-map "y"
  "l" #'display-line-numbers-mode
  "o" #'outline-minor-mode)

;; Show automatic key hints with Which-key.
(add-hook! 'window-setup-hook
  (eval-and-compile (require 'which-key))
  (setq which-key-idle-delay 0.5
        which-key-sort-order #'which-key-key-order-alpha)
  (which-key-mode 1))

(provide 'zy-leader)

;;; zy-leader.el ends here
