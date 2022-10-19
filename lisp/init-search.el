;;; init-search.el --- Searching features -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Setup powerful searching tools.

;;; Code:

(require 'init-keybinding)


;; Consult navigation commands

(zy/define-key
  '("M-g g" "M-g M-g") 'consult-goto-line
  "M-g m" 'consult-mark
  "M-g M" 'consult-global-mark
  "M-g o" 'consult-outline)


;; Rg for advanced Ripgrep usage

(straight-use-package 'rg)

(zy/snip-from-feature 'rg :weight 0)


;; Leader search commands

(zy/define-leader-submap
    zy/leader-search-map "s" "search"
  "Keymap for searching.")
(zy/define-key :keymap 'zy/leader-search-map
  "." '("Symbol at point" . isearch-forward-symbol-at-point)
  "_" '("Symbol" . isearch-forward-symbol)
  "w" '("Word" . isearch-forward-word)
  "M-." '("Thing at point" . isearch-forward-thing-at-point)
  "l" '("Line" . consult-line)
  "g" '("Consult grep" . consult-ripgrep)
  "G" '("Advanced grep" . rg-menu))


(provide 'init-search)

;;; init-search.el ends here.
