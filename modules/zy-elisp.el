;;; zy-elisp.el --- Emacs Lisp. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+elisp' module of the configuration.

;; Emacs is already the best IDE for Emacs Lisp. This file configures it to be a
;; little more handy.

;;; Code:

(require 'zylib)

;; Emacs Lisp keybindings.
(after! 'elisp-mode
  (defprefix! +elisp-map "Emacs Lisp"
              nil emacs-lisp-mode-map "<localleader>"
    "e" (cons "Eval Last" #'eval-last-sexp)
    "E" (cons "Eval Region/Buffer"#'elisp-eval-region-or-buffer)
    "d" (cons "Eval Defun" #'eval-defun)
    "m" (cons "Macro Expand" #'pp-macroexpand-last-sexp))
  (keybind! nil lisp-interaction-mode-map
    "<localleader>" +elisp-map))

(provide 'zy-elisp)

;;; zy-elisp.el ends here
