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
(require 'init-load)

;;;; Navigation Commands

(zy/define-key :prefix "M-g"
  '("g" "M-g") 'consult-goto-line
  "m" 'consult-mark
  "M" 'consult-global-mark
  "o" 'consult-outline)


;;;; Search Commands
;; Rg for advanced Ripgrep usage

(straight-use-package 'rg)

(zy/incload-register 'rg :level 3)

(zy/define-key :prefix "M-s"
  "l" 'consult-line
  "g" 'consult-ripgrep
  "G" 'rg-menu)


;;;; Isearch-mb

;; Isearch-mb is an enhanced version of Isearch

(straight-use-package 'isearch-mb)

(zy/defsnip 'snip-isearch-mb
  (isearch-mb-mode t))

(zy/incload-register 'snip-isearch-mb)


(provide 'init-search)

;;; init-search.el ends here.
