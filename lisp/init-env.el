;;; init-env.el --- Programming environment.  -*- lexical-binding: t -*-
;;; Commentary:

;; Load programming languages defined by Direnv, Nix, and other tools.

;;; Code:

(require-package 'direnv)
(require-package 'editorconfig)

;; Load Direnv mode.
(direnv-mode 1)

;; Load EditorConfig mode.
(editorconfig-mode 1)

(provide 'init-env)

;;; init-env.el ends here
