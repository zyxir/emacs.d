;;; zyutils.el --- autoloaded utilities -*- lexical-binding: t -*-

;; Copyright (C) 2022-2022 Eric Zhuo Chen

;; Author: Eric Zhuo Chen <zyxirchen@outlook.com>
;; Maintainer: Eric Zhuo Chen <zyxirchen@outlook.com>
;; Created: 2022-10-29


;; This file is not part of GNU Emacs.

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

;; This file extends init.el by providing additional functionalities for many of
;; its sections.  However, this file should not be loaded at startup, which
;; creates additional load time.  Autoload each function needed explicitly at
;; every section instead.

;; Magic comment (;;;###autoload) is tagged to those functions that should be
;; autoloaded, but just as a hint.  Autoloads are actually explicitly written in
;; init.el.

;; This file is sectioned just like init.el, providing better correspondence.

;;; Code:

;;;; Text-editing

;;;;; Line filling

;;;###autoload
(defun zy/unfill-paragraph ()
  "Do the inverse of `fill-paragraph'."
  (interactive)
  (dlet ((fill-column most-positive-fixnum))
    (call-interactively 'fill-paragraph)))

;;;; File type specific settings

;;;;; Emacs Lisp

;;;###autoload
(defun zy-lisp-indent-function (indent-point state)
  "Replacement for the function `lisp-indent-function'.

Adapted from Fuco1's `Fuco1/lisp-indent-function'.

The function `calculate-lisp-indent' calls this to determine if
the arguments of a Lisp function call should be indented
specially.

INDENT-POINT is the position at which the line being indented
begins.  Point is located at the point to indent under (for
default indentation); STATE is the `parse-partial-sexp' state for
that position.

If the current line is in a call to a Lisp function that has a
non-nil property symbol `lisp-indent-function' (or the deprecated
`lisp-indent-hook'), it specifies how to indent.  The property
value can be:

* `defun', meaning indent `defun'-style
  (this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more
  arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  Function `lisp-indent-function' calls this function with the
  same two arguments that it itself received.

This function returns either the indentation to use, or nil if
the Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (defvar calculate-lisp-indent-last-sexp)
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))

(provide 'zyutils)
;;; zyutils.el ends here
