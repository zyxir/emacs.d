;;; init-snippet.el --- Snippets.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-basic)

(require-package 'yasnippet)
(require-package 'yasnippet-snippets)

;; Enable snippets.
(defer-and-after! 'yasnippet
  (yas-global-mode 1))

(provide 'init-snippet)

;;; init-snippet.el ends here
