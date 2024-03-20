;;; init-lsp.el --- Language server protocol.  -*- lexical-binding: t -*-
;;; Commentary:

;; Most LSP keybindings have been defined in `init-keybindings' as code actions.

;;; Code:

(eval-and-compile (require 'init-basic))

(after-or-now! 'eglot
  (setq
   ;; Do not require confirmation on code actions.
   eglot-confirm-server-initiated-edits nil)

  ;; HACK Fix Scala language server association.
  (when-let* ((old-entry (assoc 'scala-mode eglot-server-programs))
              (new-entry `((scala-mode scala-ts-mode) . ,(cdr old-entry))))
    ;; Remove the old entry and add the new entry.
    (setq eglot-server-programs
          (remove old-entry eglot-server-programs)
          eglot-server-programs
          (cons new-entry eglot-server-programs))))

;; Try to enable Eglot for all prog-modes if possible, unless explicitly
;; blacklisted.

(defcustom zy/eglot-blacklist '()
  "Don't activate Eglot for these languages.
Each entry is a LANG-ID string returned by
`eglot--guess-contact'."
  :type '(repeat string)
  :group 'zyemacs)

(add-hook! prog-mode
  (defun zy/-try-to-ensure-eglot-h (&rest _)
    "Try to ensure Eglot if possible."
    (require 'eglot)
    (when-let* ((guessed (ignore-errors (eglot--guess-contact)))
                (managed-mode (nth 0 guessed))
                (lang-id (nth 4 guessed))
                ;; According to the docstring of `eglot-server-programs', there
                ;; are several possible forms of `contact'. Here we only deal
                ;; with the most basic type: a list starting with a string
                ;; PROGRAM. Personally this covers 100% of my use case, but this
                ;; may not work (or break) in some cases.
                (contact (nth 3 guessed))
                (program (and (stringp (car contact))
                              (car contact)))
                (exists (executable-find program)))
      (eglot-ensure))))

(after-or-now! 'envrc
  (defadvice! zy/-try-to-ensure-eglot-a (buf result)
    "Try to ensure Eglot when Direnv updates.
This is an advice for `envrc--apply' and only when RESULT is a
list, which indicates a successful update as indicated by the
source code. BUF is used as the current buffer."
    :after #'envrc--apply
    (when (and result
               (listp result)
               (not (memq result '(none error))))
      (with-current-buffer buf
        (zy/-try-to-ensure-eglot-h)))))

(provide 'init-lsp)

;;; init-lsp.el ends here
