;;; init-keybindings.el --- Keybindings setup  -*- lexical-binding: t -*-
;;; Commentary:

;; This module setups Evil, which emulates the main features of Vim in Emacs.
;; Several utility packages are also configured here.

;;; Code:

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
         (eglot-action (unquote! eglot-action)))
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

(provide 'init-keybindings)

;;; init-keybindings.el ends here
