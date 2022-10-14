;;; init-file.el --- File and buffer utilities -*- lexical-binding: t -*-

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

;; Commands and settings about file and buffer management.

;;; Code:

(require 'init-keybinding)
(require 'init-loaddefs)


;; Advanced scratch buffers with Scratch.el

(with-eval-after-load 'scratch
  (defvar scratch-mode-alist)
  (dolist (pair '((fundamental-mode . lisp-interaction-mode)
		  (emacs-lisp-mode . lisp-interaction-mode)))
    (add-to-list 'scratch-mode-alist pair)))


;; Advanced buffer switching

(zy/define-key
  "C-x b" 'consult-buffer
  :prefix zy/leader-keys
  "b" '("Switch buffer" . consult-buffer))


;; Leader file commands

(zy/define-leader-submap
    zy/leader-file-map "f" "file"
  "Keymap for file and buffer management.")
(zy/define-key :keymap 'zy/leader-file-map
  ;; Jump across files
  "b" '("Bookmarks" . consult-bookmark)
  "f" '("Find file" . find-file)
  "r" '("Recent file" . consult-recent-file)
  ;; File operations
  "d" '("Delete current file" . crux-delete-file-and-buffer)
  "R" '("Rename current file" . crux-rename-file-and-buffer)
  "s" '("Save current file" . save-buffer)
  "S" '("Save multiple files" . save-some-buffers)
  "w" '("Save as" . write-file)
  ;; Special buffers
  ";" '("Scratch" . scratch))


(provide 'init-file)

;;; init-file.el ends here.
