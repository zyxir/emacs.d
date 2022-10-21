;;; init-lingual.el --- Language-related settings -*- lexical-binding: t -*-


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

;; Settings about character inputing, language conversion, etc.

;;; Code:

(require 'init-keybinding)


;;;; Insersion of Special Symbols

(defmacro zy/define-inserters (&rest args)
  "Define multiple character inserters.

ARGS comes in pair with KEY and CHAR.  For each KEY and CHAR,
`C-x 8 KEY' will be defined as the shortcut to insert CHAR."
  (declare (indent defun))
  (let ((result-sexp '("C-x 8" :prefix zy/define-key))
	key char)
    (while (and (car args)
		(cadr args))
      (setq key (car args)
	    char (cadr args)
	    args (cddr args))
      (push key result-sexp)
      (push `(lambda nil (interactive) (insert ,char))
	    result-sexp))
    (reverse result-sexp)))

(zy/define-inserters
  ;; Zero width space
  "s" #x200b)


;;;; Input Method

(straight-use-package 'rime)

(zy/snip-from-feature 'rime :weight 0 :dependencies 'dash)

(setq-default
 default-input-method "rime"
 rime-user-data-dir (expand-file-name "etc/rime" user-emacs-directory)
 rime-show-candidate 'minibuffer)

(zy/defsnip snip-im-color
    (:lazyload '(init-ui rime))
  "Change cursor color based on the current IM."
  (when (fboundp 'modus-themes-color)
    (defun zy/set-cursor-color-on-im ()
      "Set cursor color based on the current input method."
      (if current-input-method
	  (set-frame-parameter nil
			       'cursor-color
			       (modus-themes-color 'orange-intense))
	(set-frame-parameter nil
			     'cursor-color
			     (modus-themes-color 'fg-main))))

    (add-hook 'post-command-hook 'zy/set-cursor-color-on-im)))


(provide 'init-lingual)

;;; init-lingual.el ends here
