;;; init-eglot.el --- Language server protocol.  -*- lexical-binding: t -*-
;;; Commentary:

;; Most LSP keybindings have been defined in `init-keybindings' as code actions.

;;; Code:

(provide 'init-eglot)

(with-eval-after-load 'eglot
  ;; HACK Fix Scala language server association.
  (when-let* ((old-entry (assoc 'scala-mode eglot-server-programs))
              (new-entry `((scala-mode scala-ts-mode) . ,(cdr old-entry))))
    ;; Remove the old entry and add the new entry.
    (setq eglot-server-programs
          (remove old-entry eglot-server-programs)
          eglot-server-programs
          (cons new-entry eglot-server-programs)))

  (setq
   ;; Do not require confirmation on code actions.
   eglot-confirm-server-initiated-edits nil))

;; Ensure Eglot only when the corresponding executable is available.
(dolist (pair
         '((c-mode-common-hook . "ccls")
           (python-base-mode-hook . "pyright")
           ((scala-mode-hook scala-ts-mode-hook) . "metals")))
  (let* ((hook (car pair))
         (exec (cdr pair))
         (hooks (if (listp hook) hook (list hook))))
    (dolist (h hooks)
      (add-hook h
                `(lambda ()
                   (when (executable-find ,exec)
                     (eglot-ensure)))))))

(setq
 ;; Do not require confirmation on code actions.
 eglot-confirm-server-initiated-edits nil)

;;; init-eglot.el ends here
