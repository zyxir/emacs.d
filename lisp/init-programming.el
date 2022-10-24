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


;;;; Flymake

;; Enable Flymake for certain modes
(mapc (lambda (hook)
	(add-hook hook 'flymake-mode))
      '(emacs-lisp-mode))

(zy/defsnip 'snip-flymake
  (require 'flymake)
  (setq-default elisp-flymake-byte-compile-load-path
		`("./" ,load-path))
  (zy/define-key :keymap 'flymake-mode-map
    "M-p" 'flymake-goto-prev-error
    "M-n" 'flymake-goto-next-error
    "M-g f" 'consult-flymake))

(zy/lload-register 'snip-flymake 'flymake)
(zy/incload-register 'snip-flymake)


;;;; Eglot

(straight-use-package 'eglot)

;; Enable Eglot in certain modes

(mapc (lambda (hook)
	(add-hook hook 'eglot-ensure))
      '(tex-mode-hook))

;; My own LSP server preferences

(with-eval-after-load 'eglot
  ;; Use Texlab for LaTeX
  (add-to-list 'eglot-server-programs
	       '((tex-mode context-mode texinfo-mode bibtex-mode)
		 "texlab")))


(provide 'init-programming)

;;; init-programming.el ends here
