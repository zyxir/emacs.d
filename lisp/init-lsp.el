;;; init-eglot.el --- Language server protocol.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(provide 'init-eglot)

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

;; "<leader> o" for Eglot operations.
(general-create-definer zy/leader-o-def
  :keymaps 'zy/leader-map
  :prefix-map 'zy/leader-o-map
  :prefix "o")
(zy/leader-o-def
  "a" #'eglot-code-actions
  "f" #'eglot-format
  "r" #'eglot-rename
  "R" #'eglot-reconnect)

;;; init-eglot.el ends here
