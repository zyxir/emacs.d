;;; init-snippet.el --- Snippets.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-and-compile (require 'init-basic))

(require-package 'yasnippet)
(require-package 'yasnippet-snippets)

;; Enable snippets.
(after-deferred! 'yasnippet
  ;; Enable Yasnippet without annoying messages.
  (defadvice! zy/-silence-yas-a (&rest _) :override #'yas--message)
  (yas-global-mode 1)
  (advice-remove #'yas--message 'zy/-silence-yas-a))

(provide 'init-snippet)

;;; init-snippet.el ends here
