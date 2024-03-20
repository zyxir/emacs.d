;;; init-check.el --- Syntax and spell checking.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-and-compile (require 'init-basic))

(require-package 'flycheck)
(require-package 'flycheck-eglot)

;; Enable Flycheck everywhere.
(global-flycheck-mode 1)

(after-or-now! 'flycheck
  ;; From URL `https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc'.
  (eval-and-compile
    (defun zy/-flycheck-eldoc (callback &rest _)
      "Print Flycheck messages at point by calling CALLBACK."
      (when-let ((flycheck-errors (and flycheck-mode
                                       (flycheck-overlay-errors-at (point)))))
        (mapc
         (lambda (err)
           (funcall callback
                    (format
                     "%s: %s"
                     (let ((level (flycheck-error-level err)))
                       (pcase level
                         ('info (propertize "I" 'face
                                            'flycheck-error-list-info))
                         ('error (propertize "E" 'face
                                             'flycheck-error-list-error))
                         ('warning (propertize "W" 'face
                                               'flycheck-error-list-warning))
                         (_ level)))
                     (flycheck-error-message err))
                    :thing (or (flycheck-error-id err)
                               (flycheck-error-group err))
                    :face 'font-lock-doc-face))
         flycheck-errors))))

  ;; Display Flycheck errors with Eldoc.
  (add-hook 'eldoc-documentation-functions #'zy/-flycheck-eldoc)
  (setq
   ;; Override Flycheck's default echoing function, which breaks Eldoc.
   flycheck-display-errors-function nil
   ;; Don't show Flycheck markers. They are useless and don't work well with
   ;; other packages.
   flycheck-indication-mode nil))

;; Use Flycheck rather than Flymake with Eglot.
(after-or-now! 'eglot
  (global-flycheck-eglot-mode 1))

(provide 'init-check)

;;; init-check.el ends here
