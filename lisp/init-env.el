;;; init-env.el --- Programming environment.  -*- lexical-binding: t -*-
;;; Commentary:

;; Load programming languages defined by Direnv and Nix.

;;; Code:

(require-package 'direnv)

;; Load Direnv mode.
(direnv-mode 1)

(provide 'init-env)

;;; init-env.el ends here
