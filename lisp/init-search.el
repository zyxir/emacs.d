;;; init-search.el --- Searching features -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-keybinding)


;; Search and replace across files with Rg and Wgrep

(with-eval-after-load 'rg)


;; Leader search commands

(define-prefix-command 'zy/search-map)
(zy/leader-def
  "s" '(zy/search-map :which-key "search"))
(general-def zy/search-map
  "g" #'consult-ripgrep
  "G" #'rg-menu)


(provide 'init-search)

;;; init-search.el ends here.
