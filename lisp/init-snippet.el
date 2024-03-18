;;; init-snippet.el --- Snippets.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-and-compile (require 'init-basic))

(require-package 'yasnippet)
(require-package 'yasnippet-snippets)

;; Enable snippets.
(after-deferred! 'yasnippet
  ;; Enable Yasnippet without annoying messages.
  (cl-letf (((symbol-function #'yas--message)
             (symbol-function #'ignore)))
    (yas-global-mode 1)))

(provide 'init-snippet)

;;; init-snippet.el ends here
