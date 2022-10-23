;;; init-vc.el --- Version control -*- lexical-binding: t -*-


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

;; Version control settings (currently I only use Git).

;;; Code:

(require 'init-load)
(require 'init-keybinding)


;;;; Setup Magit

(straight-use-package 'magit)

(zy/incload-register 'magit :level 3
		     :after 'dash 'with-editor 'transient
		     ('git-commit :level 3) 'magit-section)

(setq magit-define-global-key-bindings nil)

;; Overwrite default VC keys

(zy/define-key
  "C-x v" 'magit-status
  "C-x C-v" 'magit-dispatch
  "C-x M-v" 'magit-file-dispatch)


(provide 'init-vc)

;;; init-vc.el ends here
