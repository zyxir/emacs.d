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

;; Make Yasnippet ready after setup.
(add-hook! 'window-setup-hook (yas-global-mode 1))

;; Inhibit the echoing of messages with this advice. Messages will still be
;; appended to the *Messages* buffer.
(defun +yasnippet--inhibit-message-a (oldfun &rest args)
  "Inhibit message echoing in OLDFUN.
ARGS are passed to OLDFUN as is."
  (let ((inhibit-message t))
    (apply oldfun args)))

(after! 'yasnippet
  ;; HACK: Yasnippet prints a lot of messages verbosely. The most annoying one
  ;; is its startup message. We disable the echoing of all messages of
  ;; `yas--message' here. They are still available in the *Messages* buffer.
  (advice-add #'yas--message :around #'+yasnippet--inhibit-message-a)

  ;; HACK: `yas--parse-template' sometimes prints uninterested warnings, and it
  ;; uses the function `message' instead of `yas--message'. Let's inhibit these
  ;; unnecessary messages.
  (advice-add #'yas--parse-template :around #'+yasnippet--inhibit-message-a))

(provide 'zy-yasnippet)

;;; zy-yasnippet.el ends here
