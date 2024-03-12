;;; init-snippet.el --- Snippets.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'yasnippet)
(require-package 'yasnippet-snippets)

;; Enable snippets.
(yas-global-mode 1)

;; Remap `dabbrev-expand' to expand snippets.
(keymap-unset yas-minor-mode-map "<tab>")
(keymap-unset yas-minor-mode-map "TAB")
(keymap-set yas-minor-mode-map "M-/" #'yas-expand)

(provide 'init-snippet)

;;; init-snippet.el ends here
