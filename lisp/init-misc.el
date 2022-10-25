;;; init-misc.el --- Miscellaneous modes -*- lexical-binding: t -*-


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

;; This is for various major modes that don't need much configuration.

;;; Code:

(require 'init-load)

;;;; Markdown

(straight-use-package 'markdown-mode)


;;;; PDF

(straight-use-package 'pdf-tools)

(zy/defsnip 'snip-pdf
  (pdf-loader-install))

(zy/incload-register 'snip-pdf :level 3)

(add-hook 'pdf-view-mode-hook
	  (lambda ()
	    (display-line-numbers-mode -1)))


(provide 'init-misc)

;;; init-misc.el ends here
