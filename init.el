;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(let ((minver "29.0"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old --- this config requires v%s or higher" minver)))


;; Benchmark startup

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-bench)


;; Speed up startup

(let ((normal-gc-cons-threshold (* 16 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


;; Bootstrap config

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file) (load custom-file))

(require 'init-load)


(provide 'init)

;;; init.el ends here
