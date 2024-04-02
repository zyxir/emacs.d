;;; zy-elisp.el --- Emacs Lisp. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+elisp' module of the configuration.

;; Emacs is already the best IDE for Emacs Lisp. This file configures it to be a
;; little more handy.

;;; Code:

(require 'zylib)

(defun +elisp-compile-dwim (&optional file)
  "Compile Emacs Lisp file FILE into byte code.
If FILE is omitted or nil, or when called interactively, compile
visited file, or Dired marked files.

If native compilation is available, also natively compile the
file(s)."
  (interactive)
  (let* ((file-list (if file (list file)
                      (if (and (derived-mode-p 'dired-mode)
                               (fboundp 'dired-get-marked-files))
                          (dired-get-marked-files)
                        (list (buffer-file-name))))))
    (dolist (file file-list)
      (byte-compile-file file)
      (when (native-comp-available-p!)
        (native-compile file)))))

;; Emacs Lisp keybindings.
(after! 'elisp-mode
  (defprefix! +elisp-map "Emacs Lisp"
              nil emacs-lisp-mode-map "<localleader>"
    "C" (cons "Compile" #'+elisp-compile-dwim)
    "e" (cons "Eval Last" #'eval-last-sexp)
    "E" (cons "Eval Region/Buffer"#'elisp-eval-region-or-buffer)
    "d" (cons "Eval Defun" #'eval-defun)
    "m" (cons "Macro Expand" #'pp-macroexpand-last-sexp))
  (keybind! nil lisp-interaction-mode-map
    "<localleader>" +elisp-map))

(provide 'zy-elisp)

;;; zy-elisp.el ends here
