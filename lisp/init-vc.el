;;; init-vc.el --- Version control settings.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'magit)

;; Visit symbolic links automatically.
(setq vc-follow-symlinks t)

;; "<leader> g" for git operations.
(general-create-definer zy/leader-g-def
  :keymaps 'zy/leader-map
  :prefix-map 'zy/leader-g-map
  :prefix "g")
(zy/leader-g-def
  "g" #'magit-status
  "d" #'magit-dispatch
  "f" #'magit-file-dispatch)

(provide 'init-vc)

;;; init-vc.el ends here
