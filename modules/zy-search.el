;;; zy-search.el --- Text searching. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+search' module of the configuration.

;; This module enhances text searching both in the buffer (as per `isearch') and
;; in a directory/project (as per `project-find-regexp'), since they are similar
;; and share many common features/keybindings.

;;; Code:

(require 'zylib)



;; Use Ripgrep for xref if possible.
(after! 'xref
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep)))

(provide 'zy-search)

;;; zy-search.el ends here
