;;; init-snippet.el --- Snippets.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-and-compile (require 'init-basic))

(require-package 'yasnippet)
(require-package 'yasnippet-snippets)

;; Enable snippets.
(after-deferred! 'yasnippet
  (yas-global-mode 1))

(provide 'init-snippet)

;;; init-snippet.el ends here
