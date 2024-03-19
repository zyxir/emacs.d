;;; init-search.el --- Searching and replacing.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-and-compile (require 'init-basic))

;; Use Ripgrep for xref if possible.
(after-or-now! 'xref
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep)))

(provide 'init-search)

;;; init-search.el ends here
