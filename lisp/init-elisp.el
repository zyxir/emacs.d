;;; init-elisp.el --- Emacs Lisp utilities -*- lexical-binding: t -*-

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

;; Utilities for coding in Emacs Lisp.

;;; Code:

(require 'init-keybinding)


;; Hide eldoc-mode

(blackout 'eldoc-mode)


;; Function to launch a new instance of Emacs

(defun zy/test-config (&optional recompile)
  "Test the updated config.

Start a new instance of Emacs with \"--debug-init\" argument to
test it.

If RECOMPILE is non-nil, or with prefix argument when called
interactively, recompile the whole config before starting the new
instance."
  (interactive "P")
  (save-some-buffers)
  (when recompile
    (zy/recompile-config))
  (restart-emacs-start-new-emacs '("--debug-init")))

(general-def zy/manage-map
  "t" #'zy/test-config)


(provide 'init-elisp)

;;; init-elisp.el ends here
