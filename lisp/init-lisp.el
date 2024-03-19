;;; init-lisp.el --- Lisp family settings.  -*- lexical-binding: t -*-
;;; Commentary:

;; This file configures all child modes of `lisp-data-mode'.

;;; Code:

(eval-and-compile (require 'init-basic))

(after-or-now! 'flycheck
  ;; Check syntax using the load-path populated by the init file.
  (defvar flycheck-emacs-lisp-load-path)
  (setq flycheck-emacs-lisp-load-path 'inherit))

;; Emacs Lisp keybindings.
(zy/local-leader-def
  :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
  "e" #'eval-last-sexp
  "E" #'elisp-eval-region-or-buffer
  "d" #'eval-defun
  "x" #'pp-macroexpand-last-sexp
  "C" #'emacs-lisp-byte-compile)

(provide 'init-lisp)

;;; init-lisp.el ends here
