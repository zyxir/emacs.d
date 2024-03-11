;;; init-search.el --- Searching and replacing.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Use Ripgrep for xref if possible.
(when (executable-find "rg")
  (setq xref-search-program 'ripgrep))

(provide 'init-search)

;;; init-search.el ends here
