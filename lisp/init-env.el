;;; init-env.el --- Programming environment.  -*- lexical-binding: t -*-
;;; Commentary:

;; Load programming languages defined by Direnv, Nix, and other tools.

;;; Code:

(require-package 'envrc)
(require-package 'editorconfig)

;; Manage Direnv with the Envrc package.
(envrc-global-mode 1)

;; "<leader> e" for environment-related operations.
(zy/leader-def
  "e" #'envrc-command-map)

;; Load EditorConfig mode.
(editorconfig-mode 1)

(provide 'init-env)

;;; init-env.el ends here
