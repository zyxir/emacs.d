;;; zy-quickins.el --- Quick insertion of text. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+quickins' module of the configuration.

;; This file introduces the keymap `+quickins-map' for quickly inserting
;; frequently-used text, or inserting snippets, emojis, and filesystem paths
;; with the help of completion.

;;; Code:

(require 'zylib)

(pkg! 'cape)
(pkg! 'consult-yasnippet)

(defun +quickins/zwsp ()
  "Insert a ZWSP (zero-width space)."
  (interactive)
  (insert #x200B))

;; Evil (Vim) uses both "C-q" and "C-v" for quoted insert. Since "C-q" is
;; consistent across Emacs and Vim, I decided to remap "C-v" as a handy shortcut
;; to insert frequently-used pieces of text.
(defprefix! +quickins-map "QuickIns"
            'insert 'global "C-v"
  "8" '("Char by Desc..." . insert-char)
  "0" '("ZWSP" . +quickins/zwsp)
  "C-f" '("Path" . cape-file)
  "C-e" '("Emoji" . cape-emoji))

(when (modulep! '+yasnippet)
  (keybind! nil +quickins-map
    "C-s" '("Snippet" . consult-yasnippet)))

(provide 'zy-quickins)

;;; zy-quickins.el ends here
