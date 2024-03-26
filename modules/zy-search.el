;;; zy-search.el --- Text searching. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+search' module of the configuration.

;; This module enhances text searching both in the buffer (as per `isearch') and
;; in a directory/project (as per `project-find-regexp'), since they are similar
;; and share many common features/keybindings.

;;; Code:

(require 'zylib)

(pkg! 'anzu)

;; Use Anzu's `query-replace' alternatives for replace preview. Anzu actually
;; provides more features than previewing query-replace, but some of them are
;; not useful to me because: (a) I use Evil-search instead of Isearch because
;; Isearch does not support Chinese input methods, but Anzu only supports
;; Isearch, and (b) Evil-anzu does not work when I tried.
(keybind! nil 'global
  [remap query-replace] #'anzu-query-replace-regexp
  [remap query-replace-regexp] #'anzu-query-replace)

;; Use Ripgrep for xref if possible.
(after! 'xref
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep)))

(provide 'zy-search)

;;; zy-search.el ends here
