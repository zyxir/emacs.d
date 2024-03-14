;;; init-lsp.el --- Language server protocol.  -*- lexical-binding: t -*-
;;; Commentary:

;; Most LSP keybindings have been defined in `init-keybindings' as code actions.

;;; Code:

(with-eval-after-load 'eglot
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
  :type '(repeat string))

(add-hook! prog-mode
  (defun zy/-try-to-ensure-eglot-h (&rest _)
    "Try to ensure Eglot if possible."
    (require 'eglot)
    (let* ((contact (ignore-errors (eglot--guess-contact)))
           (managed-mode (nth 0 contact))
           (lang-id (nth 4 contact)))
      (when (and managed-mode
                 (not (member lang-id zy/eglot-blacklist)))
        (eglot-ensure)))))

(defadvice! zy/-try-to-ensure-eglot-a (&rest _)
  "Try to ensure Eglot if possible."
  :after #'envrc--update
  (zy/-try-to-ensure-eglot-h))

(provide 'init-lsp)

;;; init-lsp.el ends here
