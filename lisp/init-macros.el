;;; init-macros.el --- Handy macros -*- lexical-binding: t -*-


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

;; This file provides some handy macros that can be used throughout the
;; configuration.

;;; Code:

(defmacro add-hook! (hook &rest body)
  "A extended version of `add-hook'.

HOOK is the hook or list of hooks you would like to add to.  It
should be a quoted hook symbol, or a quoted list of hook symbols.

If BODY only contains a single quoted symbol, the symbol is then
regarded as a function, and is added to hook HOOK, or every hook
in HOOK, if it is a list of hooks.

If BODY contains one or multiple Lisp expressions, they will be
wrapped by a lambda function and added to HOOK."
  (declare (indent 1))
  (let* ((hooks-p (listp (cadr hook)))
	 (func-p (and (not (cdr body))
		      (eq (car (car body)) 'quote)
		      (symbolp (cadr (car body)))))
	 (func (if func-p (car body) `(lambda nil ,@body))))
    (if hooks-p
	(let ((result (list 'progn)))
	  (dolist (h (cadr hook) (nreverse result))
	    (push `(add-hook ',h ,func) result)))
      `(add-hook ,hook ,func))))


(provide 'init-macros)

;;; init-macros.el ends here
