;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(let ((minver "29.0"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old --- this config requires v%s or higher" minver)))


;; Speed up startup

(let ((normal-gc-cons-threshold (* 16 1024 1024))
      (normal-gc-cons-percentage gc-cons-percentage)
      (normal-file-name-handler-alist file-name-handler-alist)
      (init-gc-cons-threshold (* 128 1024 1024))
      (init-gc-cons-percentage 0.6)
      (init-file-name-handler-alist nil))
  (setq gc-cons-threshold init-gc-cons-threshold
	gc-cons-percentage init-gc-cons-percentage
	file-name-handler-alist init-file-name-handler-alist)
  (add-hook 'emacs-startup-hook
            (lambda ()
	      (setq gc-cons-threshold normal-gc-cons-threshold
		    gc-cons-percentage normal-gc-cons-percentage
		    file-name-handler-alist (nconc
					     file-name-handler-alist
					     normal-file-name-handler-alist)))))


;; Startup benchmarking

(push (expand-file-name "lisp" user-emacs-directory) load-path)
(require 'init-benchmark)


;; Bootstrap config

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file) (load custom-file))

;; Loading
(require 'init-load)
;; (require 'init-loaddefs)

;; Core
(require 'init-common)
(require 'init-keybinding)

;; Features
(require 'init-completion)
(require 'init-editing)
(require 'init-file)
(require 'init-lingual)
(require 'init-programming)
(require 'init-project)
(require 'init-search)
(require 'init-tex)
(require 'init-ui)

;; Major modes
(require 'init-elisp)
(require 'init-org)

;; Provide the `zyemacs' feature so that my Zyutils package can load, as my
;; configuration is a dependency of it.
(require 'zyemacs)


(provide 'init)

;;; init.el ends here
