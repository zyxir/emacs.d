;;; init-tex.el --- TeX/LaTeX settings -*- lexical-binding: t -*-


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

;; Settings for TeX/LaTeX editing.

;;; Code:

(require 'init-load)
(eval-when-compile (require 'init-macros))


;;;; TeX and LaTeX with AUCTeX

(straight-use-package 'auctex)

(after! 'tex
  ;; Switches

  (setq-default TeX-auto-save t
		TeX-parse-self t
		TeX-save-query nil
		TeX-engine 'xetex
		TeX-source-correlate-start-server t
		TeX-command-default "XeLaTeX")
  (defvar TeX-command-list)
  (add-to-list 'TeX-command-list
	       '("XeLaTeX"
                 "%`xelatex%(mode)%' --synctex=1%(mode)%' %t"
                 TeX-run-TeX
                 nil
                 t))
  (add-hook 'LaTeX-mode-hook
	    (lambda ()
	      (setq-local TeX-command-default "XeLaTeX")
	      (TeX-source-correlate-mode 1)))

  ;; Easier key to set font

  (zy/define-key
    :keymap 'LaTeX-mode-map
    "M-o" 'TeX-font)

  ;; RefTeX

  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (with-eval-after-load 'reftex
    (setq-default reftex-plug-into-AUCTeX t
		  reftex-enable-partial-scans t
		  reftex-save-parse-info t
		  reftex-use-multiple-selection-buffers t)))


(provide 'init-tex)

;;; init-tex.el ends here
