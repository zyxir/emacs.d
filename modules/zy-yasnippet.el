;;; zy-yasnippet.el --- Yasnippet setup. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+yasnippet' module of the configuration.

;; The package Yasnippet provides the snippet framework for this configuration.
;; The package Yasnippet-snippets provides many built-in snippets. I also have
;; some personal snippets stored in the "snippet" directory in this repository.
;;
;; If the `+quickins' module is enabled, snippets can be quickly inserted via
;; "C-v C-s" in insert state.

;;; Code:

(require 'zylib)

(pkg! 'yasnippet)
(pkg! 'yasnippet-snippets)

;; Enable snippets.
(after-deferred! 'yasnippet
  ;; HACK: Yasnippet prints a message at startup, which is very annoying and
  ;; cannot be inhibited by setting `inhibit-message'. Here the message is
  ;; silenced by temporarily rebinding `yas--message', the function used by
  ;; Yasnippet internally to print messages.
  (cl-letf (((symbol-function #'yas--message)
             (symbol-function #'ignore)))
    (yas-global-mode 1)))

(provide 'zy-yasnippet)

;;; zy-yasnippet.el ends here
