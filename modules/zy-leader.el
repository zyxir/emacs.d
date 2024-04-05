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

;; Define the space key as the leader key, like other popular Emacs
;; configurations do, including SpaceVim, Spacemacs, and Doom Emacs. We should
;; make the leader key available in all these "normal-like" states, so that it
;; never gets overriden.
(set-leader! '(normal visual operator motion) (kbd "SPC"))

;; We should also have a universally-available leader key, so that we can use
;; the leader key in insert/replace/emacs states, or even when Evil is not
;; enabled altogether. Using "M-SPC" (Alt-Space) is a good idea, but this
;; combination is often occupied by the window manager (as in Microsoft Windows
;; and many Linux desktop environments) to open the window menu. Therefore, we
;; use "M-m" instead.
(set-leader! nil (kbd "M-m"))

;; The local leader key is used to perform mode-specific operations. "," (the
;; comma key) is a good choice. Since "M-," is available almost anywhere, use
;; it as the local leader key in insert state.
(set-leader! '(normal visual operator motion) (kbd ",") 'localleader)
(set-leader! nil (kbd "M-,") 'localleader)

(defprefix! +leader-map "Leader"
            nil 'global "<leader>"
  ;; Quick commands with leader plus a single key.
  "a" (cons "Sidebar"
            (defun +leader-sidebar ()
              "Reserved for a sidebar command."
              (interactive)
              (message "This key is reserved for a side bar command.")))
  "b" (cons "Go to Buffer" #'consult-buffer)
  "B" (cons "Go to Buffer*" #'switch-to-buffer)
  "C-b" (cons "List Buffers" #'list-buffers)
  "d" (cons "Dired" #'dired)
  "s" (cons "Save Buffer" #'save-buffer)
  "k" (cons "Kill Buffer" #'kill-buffer)
  "]" (cons "Next Tab" #'tab-next)
  "[" (cons "Prev Tab" #'tab-previous))

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
       (eval-and-compile
         (defun ,fn-name (&rest _)
           ,(format
             "A placeholder command for action \"%s\"."
             action)
           (interactive)
           (if (and (fboundp 'eglot-managed-p)
                    (eglot-managed-p))
               (call-interactively ',eglot-action)
             (message ,(format "Action \"%s\" is not implemented." action)))))
       (cons ,(capitalize (string-replace "-" " " action)) #',fn-name))))

(defprefix! +leader-c-map "Code"
            nil +leader-map "c"
  "c" (cons "Compile" #'compile)
  "f" (+leader-c-create-action "format" 'eglot-format)
  "i" (+leader-c-create-action "inline" 'eglot-code-action-inline)
  "o" (+leader-c-create-action "organize-imports"
        'eglot-code-action-organize-imports)
  "q" (+leader-c-create-action "quickfix" 'eglot-code-action-quickfix)
  "r" (+leader-c-create-action "rename" 'eglot-rename)
  "R" (+leader-c-create-action "rewrite" 'eglot-code-action-rewrite)
  "x" (+leader-c-create-action "extract" 'eglot-code-action-extract))

(defprefix! +leader-e-map "Extra"
            nil +leader-map "e")

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

(defprefix! +leader-r-map "Roam"
            nil +leader-map "r")

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
  "h" (cons "Left" #'windmove-left)
  "j" (cons "Down" #'windmove-right)
  "k" (cons "Up" #'windmove-up)
  "l" (cons "Right" #'windmove-right)
  "s" (cons "Split" #'split-window-below)
  "v" (cons "Vsplit" #'split-window-right)
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
