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

(pkg! 'consult)
(pkg! 'magit)

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
(dolist (feature-map-pair `((help-mode . help-mode-map)
                            (magit-status . magit-status-mode-map)
                            (info . Info-mode-map)))
  (let ((feature (car feature-map-pair))
        (map-symbol (cdr feature-map-pair)))
    (with-eval-after-load feature
      ;; For some reason you need to both unbind SPC normally and then unbind it
      ;; in these Evil states to ensure that it is not occupied at all.
      (keybind! nil (symbol-value map-symbol) "SPC" nil)
      (keybind! '(normal motion visual operator)
          (symbol-value map-symbol) "SPC" nil)
      (keybind! 'insert (symbol-value map-symbol) "M-m" nil))))

;; Tell Evil-collection to not touch my leader keys.
(defvar evil-collection-key-blacklist nil)
(add-to-list 'evil-collection-key-blacklist "SPC")
(add-to-list 'evil-collection-key-blacklist "M-m")

;; As previously mentioned, it is only necessary to bind the map to motion and
;; insert state.
(defprefix! +leader-map "Leader"
            '(motion insert) 'global "<leader>"
  ;; Quick commands with leader plus a single key.
  "b" (cons "Go to Buffer" #'consult-buffer)
  "B" (cons "Go to Buffer*" #'switch-to-buffer)
  "C-b" (cons "List Buffers" #'list-buffers)
  "d" (cons "Dired" #'dired)
  "s" (cons "Save Buffer" #'save-buffer)
  "k" (cons "Kill Buffer" #'kill-buffer))

(defmacro +leader-c-create-action (action &optional eglot-action)
  "Create a placeholder command for action ACTION.

ACTION must be a string. If EGLOT-ACTION is non-nil and is a
command, call it if Eglot is available.

Return a cons cell (NAME . COMMAND), where NAME is the
capitalized ACTION, and COMMAND is the command created. This cons
cell is ready to be used in `define-key'."
  (declare (indent defun))
  (let* ((fn-name (intern (format "+leader-do-%s" action)))
         (eglot-action (unquote! eglot-action)))
    ;; Validate EGLOT-ACTION at compile time.
    (when eglot-action
      (require 'eglot)
      (unless (fboundp eglot-action)
        (error "`%s' is not a valid Eglot command" eglot-action)))
    `(progn
       (defun ,fn-name (&rest _)
         ,(format
           "A placeholder command for action \"%s\"."
           action)
         (interactive)
         (if (and (fboundp 'eglot-managed-p)
                  (eglot-managed-p))
             (call-interactively ',eglot-action)
           (message ,(format "Action \"%s\" is not implemented." action))))
       (cons ,(capitalize (string-replace "-" " " action)) #',fn-name))))

(defprefix! +leader-c-map "Code"
            nil +leader-map "c"
  "f" (+leader-c-create-action "format" 'eglot-format)
  "i" (+leader-c-create-action "inline" 'eglot-code-action-inline)
  "o" (+leader-c-create-action "organize-imports"
        'eglot-code-action-organize-imports)
  "q" (+leader-c-create-action "quickfix" 'eglot-code-action-quickfix)
  "r" (+leader-c-create-action "rename" 'eglot-rename)
  "R" (+leader-c-create-action "rewrite" 'eglot-code-action-rewrite)
  "x" (+leader-c-create-action "extract" 'eglot-code-action-extract))

(defprefix! +leader-f-map "File"
            nil +leader-map "f"
  "f" (cons "Open" #'find-file)
  "r" (cons "Revert" #'revert-buffer-quick)
  "c" (cons "Recent" #'consult-recent-file)
  "R" (cons "Rename" #'rename-visited-file)
  "s" (cons "Save" #'save-buffer)
  "S" (cons "Mass Save" #'save-some-buffers)
  "w" (cons "Save As..." #'write-file))

(defprefix! +leader-g-map "Git"
            nil +leader-map "g")

(defprefix! +leader-h-map "Help"
            nil +leader-map "h"
  ;; Some of these keys mirror those of `help-map', but not all of them do. For
  ;; example, "F" is `Info-goto-emacs-command-node' in `help-map', but is
  ;; `describe-face' here.
  "f" (cons "Function" #'describe-function)
  "v" (cons "Variable" #'describe-variable)
  "o" (cons "Symbol" #'describe-symbol)
  "m" (cons "Mode" #'describe-mode)
  "k" (cons "Key" #'describe-key)
  "p" (cons "Package" #'describe-package)
  "M" (cons "Keymap" #'describe-keymap)
  "F" (cons "Face" #'describe-face))

(defprefix! +leader-o-map "Org"
            nil +leader-map "o"
  "a" (cons "Agenda" #'org-agenda)
  "c" (cons "Capture" #'org-capture)
  "l" (cons "Calendar" #'calendar))

(defprefix! +leader-p-map "Project"
            nil +leader-map "p"
  "p" (cons "Switch" #'project-switch-project)
  "f" (cons "File" #'project-find-file)
  "d" (cons "Dir" #'project-find-dir)
  "b" (cons "Buffer" #'project-switch-to-buffer)
  "e" (cons "Eshell" #'project-eshell)
  "g" (cons "Magit" #'magit-project-status)
  "v" (cons "VC" #'project-vc-dir)
  "k" (cons "Kill Buffers" #'project-kill-buffers)
  "/" (cons "Search" #'project-find-regexp)
  "%" (cons "Replace" #'project-query-replace-regexp)
  "&" (cons "Shell Command" #'project-async-shell-command))

(defprefix! +leader-q-map "Quit"
            nil +leader-map "q"
  "q" (cons "Quit" #'save-buffers-kill-terminal)
  "r" (cons "Restart" #'restart-emacs)
  "z" (cons "Suspend" #'suspend-emacs))

(defprefix! +leader-t-map "Tab"
            nil +leader-map "t"
  "n" (cons "New" #'tab-new)
  "c" (cons "Close" #'tab-close)
  "t" (cons "O.T. Prefix" #'other-tab-prefix)
  "b" (cons "Buffer" #'consult-buffer-other-tab)
  "d" (cons "Dired" #'dired-other-tab)
  "f" (cons "File" #'find-file-other-tab)
  "p" (other-tabbed! "Project" +leader-p-map))

(defprefix! +leader-w-map "Window"
            nil +leader-map "w"
  "c" (cons "Close" #'delete-window)
  "o" (cons "Only" #'delete-other-windows)
  "h" (cons "Left" #'evil-window-left)
  "j" (cons "Down" #'evil-window-down)
  "k" (cons "Up" #'evil-window-up)
  "s" (cons "Split" #'evil-window-split)
  "v" (cons "Vsplit" #'evil-window-vsplit)
  "l" (cons "Right" #'evil-window-right)
  "w" (cons "O.W. Prefix" #'other-window-prefix)
  "b" (cons "Buffer" #'consult-buffer-other-window)
  "d" (cons "Dired" #'dired-other-window)
  "f" (cons "File" #'find-file-other-window)
  "p" (other-windowed! "Project" +leader-p-map))

(defprefix! +leader-y-map "Toggle..."
            nil +leader-map "y"
  "l" (cons "Line Numbers" #'display-line-numbers-mode)
  "o" (cons "Outline" #'outline-minor-mode))

(provide 'zy-leader)

;;; zy-leader.el ends here
