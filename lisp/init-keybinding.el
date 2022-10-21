;;; init-keybinding.el --- Setup key bindings -*- lexical-binding: t -*-

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

;; Key-binding related utilities and settings.

;;; Code:

(require 'init-load)


;;;; Key-Binding Utility

(defun zy/-normalize-key (prefix key)
  "Normalize KEY and PREFIX to vectors and combine them.

If KEY is [remap ...] or [t], don't prepend PREFIX to it."
  ;; Normalize KEY and PREFIX with `kbd', and make sure they are vectors
  (when (stringp key) (setq key (kbd key)))
  (when (stringp key) (setq key (string-to-vector key)))
  (when (and prefix (stringp prefix)) (setq prefix (kbd prefix)))
  (when (and prefix (stringp prefix)) (setq prefix (string-to-vector prefix)))
  ;; Maybe prepend PREFIX
  (if (or (not prefix)
	  (and (vectorp key)
	       (memq (aref key 0) '(t remap))))
      key
    (vconcat prefix key)))

(defun zy/define-key (&rest args)
  "Define multiple keys via `define-key'.

Each key definition are performed under some keymaps and a
prefix.  By default, `global-map' is the only keymap, and nil is
the prefix, which means no prefix at all.

ARGS is a list of arguments that comes in pairs.  Supported pairs
are:

`:keymap'/':keymaps', KEYMAP: Set the current keymap to KEYMAP.
KEYMAP can be a symbol representing a keymap, or a list of such
symbols.

`:prefix', PREFIX: Set the current prefix to PREFIX.  PREFIX can
be a prefix key, or a list of prefix keys, or nil, which means no
prefix key at all.

KEY, DEF: Bind KEY to DEF with the current prefix PREFIX, under
the current keymap KEYMAP.  KEY will first be normalized with
PREFIX via `zy/-normalize-key'.  If KEY is a list of keys, do
that for all of them.  The result is then defined as DEF under
KEYMAP via `define-key'.  If KEYMAP is a list of keymaps, call
`define-key' for each keymap in the list."
  (declare (indent defun))
  (let ((current-keymap '(global-map))
	(current-prefix '(nil))
	arg1 arg2)
    ;; Parse ARGS in a loop
    (while (and (consp args)
		(cdr args))
       ;; Get the argument pair
      (setq arg1 (car args)
	    arg2 (cadr args)
	    args (cddr args))
      (cond
       ;; Set current keymap
       ((or (equal arg1 ':keymap)
	    (equal arg1 ':keymaps))
	(cond ((symbolp arg2) (setq current-keymap `(,arg2)))
	      ((consp arg2) (setq current-keymap arg2))
	      ((not arg2) (setq current-keymap '(global-map)))
	      (t nil)))
       ;; Set current prefix
       ((equal arg1 ':prefix)
	(cond ((or (stringp arg2) (vectorp arg2)) (setq current-prefix `(,arg2)))
	      ((consp arg2) (setq current-prefix arg2))
	      (t nil)))
       ;; Call `define-key' for KEY DEF pair.
       (t
	(when (or (stringp arg1) (vectorp arg1))
	  (setq arg1 `(,arg1)))
	(mapc
	 (lambda (keymap)
	   (mapc
	    (lambda (prefix)
	      (mapc
	       (lambda (key)
		 (define-key (symbol-value keymap)
			     (zy/-normalize-key prefix key)
			     arg2))
	       arg1))
	    current-prefix))
	 current-keymap))))
    t))


;;;; Default Key Tweaks

(zy/define-key
  [remap just-one-space] 'cycle-spacing
  [remap delete-horizontal-space] 'cycle-spacing)


;;;; Leader Key Setup

(defconst zy/leader-keys '("C-c" "C-z" "M-m")
  "Leader keys of ZyEmacs.")

(zy/define-key
  (remove "C-c" zy/leader-keys) nil
  :prefix zy/leader-keys
  "[" '("Backward page" . backward-page)
  "]" '("Forward page" . forward-page))

(defmacro zy/define-leader-submap
    (keymap key &optional desc docstring)
  "Define KEYMAP as a sub-keymap of the leader keys.

KEYMAP is a unquoted symbol name.  A new sparse keymap is stored
as KEYMAP's function definition and its value.

KEYMAP is bound to KEY with any of the leader key prefix.  KEY
can be a key, or a list of keys.

DESC is a short descriptive text that is used by Which-key.

DOCSTRING is used as the docstring of the variable definition of
KEYMAP."
  (declare (doc-string 4) (indent 3))
  `(let ((map (make-sparse-keymap)))
     (fset ',keymap map)
     (defvar ,keymap map ,docstring)
     (zy/define-key :prefix zy/leader-keys ,key ',(cons desc keymap))))


;;;; Leader Manage Map

(zy/define-leader-submap
    zy/leader-manage-map "m" "manage"
  "Keymap for manage the configuration.")
(zy/define-key
 :keymap 'zy/leader-manage-map
  "b" '("Show benchmark result" . zy-mngt/show-benchmark-result)
  "r" '("Recompile config" . zy-mngt/recompile-config)
  "t" '("Test config" . zy-mngt/test-config)
  "l" '("Regen loaddefs" . zy/regen-loaddefs))


;;;; Leader Toggle Map

(zy/define-leader-submap
    zy/leader-toggle-map "t" "toggle"
  "Keymap for all kinds of toggles.")


;;; Which-Key

;; Provide key hints via Which-key

(straight-use-package 'which-key)

(zy/defsnip snip-which-key
    (:weight 90)
  (require 'which-key)
  (which-key-mode))


(provide 'init-keybinding)

;;; init-keybinding.el ends here.
