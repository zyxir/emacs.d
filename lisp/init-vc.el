;;; -*- lexical-binding: t; -*-
;;; Commentary:

;; Version control settings.

;;; Code:

;; Use magit as git interface.

(use-package magit
  :straight t
  :config
  ;; Do not show diff on commit by default.
  ;; Toggle diff with C-c C-d.
  (remove-hook 'magit-refs-section-hook 'magit-insert-tags))

;; End of config.

(provide 'init-vc)
