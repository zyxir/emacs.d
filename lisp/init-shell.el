;;; init-shell.el --- Shell enhancements -*- lexical-binding: t -*-


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

;; This file provides various shell enhancements.

;;; Code:

(require 'init-load)


;;;; Eshell

;; I want to make eshell as useful as possible, so that it can replace external
;; terminal emulators.

(after! 'eshell
  ;; Switches

  (setq-default eshell-scroll-to-bottom-on-input 'all
		eshell-error-if-no-glob t
		eshell-hist-ignoredups t
		eshell-save-history-on-exit t
		eshell-prefer-lisp-functions nil
		eshell-destroy-buffer-when-process-dies t)

  ;; New command: clear

  (defun eshell/clear()
    "Clear the eshell buffer."
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input))))

(zy/incload-register 'eshell :level 2)


(provide 'init-shell)

;;; init-shell.el ends here
