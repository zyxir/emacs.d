;;; init-programming.el --- Features for writing programs -*- lexical-binding: t -*-

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

;; Enhance the coding experience from various aspects, and making Emacs an
;; IDE-like environment.

;;; Code:

(require 'init-keybinding)
(require 'init-load)


;;;; Flycheck

(straight-use-package 'flycheck)

(zy/defsnip snip-flycheck
    (:events 'find-file :weight 0)
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode +1)
  (zy/define-key :keymap 'flycheck-mode-map
    "M-p" 'flycheck-previous-error
    "M-n" 'flycheck-next-error))


(provide 'init-programming)

;;; init-programming.el ends here
