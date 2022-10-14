;;; init-elisp.el --- Emacs Lisp utilities -*- lexical-binding: t -*-

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

;; Utilities for coding in Emacs Lisp.

;;; Code:

(require 'init-editing)
(require 'init-keybinding)


;; Handy commands

(zy/define-key
  :keymap 'zy/leader-edit-map
  "x" '("Expand macro" . emacs-lisp-macroexpand)
  "R" '("Eval and replace" . crux-eval-and-replace))


(provide 'init-elisp)

;;; init-elisp.el ends here
