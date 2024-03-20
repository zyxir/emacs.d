;;; init-env.el --- Programming environment.  -*- lexical-binding: t -*-
;;; Commentary:

;; Load programming languages defined by Direnv, Nix, and other tools.

;;; Code:

(eval-and-compile (require 'init-basic))

(declare-function zy/leader-c-def 'init-keybindings)

(pkg! 'envrc)
(pkg! 'editorconfig)

;; Manage Direnv with the Envrc package.
(after-deferred! 'envrc
  (envrc-global-mode 1)

  ;; "<leader> c e" for environment-related operations.
  (zy/leader-c-def
    "e" #'envrc-command-map))

;; Load EditorConfig mode.
(after-deferred! 'editorconfig
  (editorconfig-mode 1))

(provide 'init-env)

;;; init-env.el ends here
