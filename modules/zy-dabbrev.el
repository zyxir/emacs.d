;;; zy-dabbrev.el --- Dynamic abbreviations. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+dabbrev' module of the configuration.

;; Dabbrev provides dynamic abbreviation expansion based on the content of the
;; buffer. It is very handy while writing prose.

;;; Code:

(require 'zylib)

;; Replace the bindings of `dabbrev-completion' and `dabbrev-expand'. The former
;; one is handier with the help of Corfu completion UI and should be used more.
(keybind! nil 'global
  "M-/" #'dabbrev-completion
  "C-M-/" #'dabbrev-expand)

(after! 'dabbrev
  ;; Ignore buffers whose names start with a space. I don't know any buffer like
  ;; this personally, since this piece of code was copied from Corfu's README.
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")

  ;; Ignore buffers with these major modes.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))

(provide 'zy-dabbrev)

;;; zy-dabbrev.el ends here
