;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(let ((minver "29.0"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old --- this config requires v%s or higher" minver)))


;; Startup benchmarking

(push (expand-file-name "lisp" user-emacs-directory) load-path)
(require 'init-bench)


;; Bootstrap config

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file) (load custom-file))

;; Core
(zy/require 'init-load)
(zy/require 'init-common)
(zy/require 'init-keybinding)

;; Features
(zy/require 'init-file)
(zy/require 'init-completion)
(zy/require 'init-programming)
(zy/require 'init-project)
(zy/require 'init-search)
(zy/require 'init-ui)

;; Major modes
(zy/require 'init-elisp)


(provide 'init)

;;; init.el ends here
