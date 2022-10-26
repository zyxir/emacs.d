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
(eval-when-compile (require 'init-macros))


;;;; Flycheck

(straight-use-package 'flycheck)

(add-hook! 'emacs-lisp-mode-hook 'flycheck-mode)

(with-eval-after-load 'flycheck
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
    (zy/define-key :keymap 'flycheck-mode-map
    "M-p" 'flycheck-previous-error
    "M-n" 'flycheck-next-error))

(zy/incload-register 'flycheck)


;;;; Flymake

;; Currently I only use Flymake alongside Eglot

(with-eval-after-load 'flymake
  (setq-default elisp-flymake-byte-compile-load-path
		`("./" ,load-path))
  (zy/define-key :keymap 'flymake-mode-map
    "M-p" 'flymake-goto-prev-error
    "M-n" 'flymake-goto-next-error
    "M-g f" 'consult-flymake))

(zy/incload-register 'flymake)


;;;; Eglot

(straight-use-package 'eglot)

;; Enable Eglot in certain modes

(add-hook! '(TeX-mode-hook) 'eglot-ensure)

;; My own LSP server preferences

(with-eval-after-load 'eglot
  ;; Use Texlab for LaTeX
  (defvar eglot-server-programs)
  (add-to-list 'eglot-server-programs
	       '((tex-mode context-mode texinfo-mode bibtex-mode)
		 "texlab")))


;;;; Tree-Sitter

(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)

(zy/defsnip 'snip-tree-sitter
  (require 'tree-sitter)
  (require 'tree-sitter-langs)
  (declare-function global-tree-sitter-mode "tree-sitter")
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook 'tree-sitter-hl-mode))

(zy/incload-register 'snip-tree-sitter :level 4)


(provide 'init-programming)

;;; init-programming.el ends here
