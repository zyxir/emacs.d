;;; init-lisp.el --- Lisp family settings.  -*- lexical-binding: t -*-
;;; Commentary:

;; This file configures all child modes of `lisp-data-mode'.

;;; Code:

;; Emacs Lisp keybindings.
(zy/local-leader-def
  :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
  "e" #'eval-last-sexp
  "E" #'elisp-eval-region-or-buffer
  "x" #'pp-macroexpand-last-sexp)

(provide 'init-lisp)

;;; init-lisp.el ends here
