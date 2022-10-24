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


;;;; Switching Buffer

(zy/define-key
  "C-x b" 'consult-buffer
  :prefix zy/leader-keys
  "b" 'consult-buffer)


;;;; Leader File Commands

(zy/define-leader-submap
    zy/leader-file-map "f" "file"
  "Keymap for file and buffer management.")

(zy/define-key :keymap 'zy/leader-file-map
  ;; Jump across files
  "b" '("bookmarks" . consult-bookmark)
  "f" '("find file" . find-file)
  "r" '("recent file" . consult-recent-file)
  ;; File operations
  "d" '("delete current file" . crux-delete-file-and-buffer)
  "R" '("rename current file" . crux-rename-file-and-buffer)
  "s" '("save file" . save-buffer)
  "S" '("save multiple files" . save-some-buffers)
  "w" '("save as" . write-file))


;;;; Scratch Buffer

;; Omni-scratch provides advanced scratch buffers

(straight-use-package 'omni-scratch)

(zy/define-leader-submap
    zy/leader-scratch-map ";" "scratch"
  "Keymap for scratch-related commands.")

(setq-default omni-scratch-default-mode 'lisp-interaction-mode)

(zy/define-key :keymap 'zy/leader-scratch-map
  ";" 'omni-scratch
  "m" 'omni-scratch-major
  "b" 'omni-scratch-buffer
  "j" 'omni-scratch-goto-latest
  "q" 'omni-scratch-quit)


;;;; Treemacs

(straight-use-package 'treemacs)
(straight-use-package 'treemacs-magit)

(zy/defsnip 'snip-treemacs
  (require 'treemacs)
  (with-eval-after-load 'magit
    (require 'treemacs-magit))
  (treemacs-filewatch-mode t)
  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(zy/lload-register 'snip-treemacs 'treemacs)
(zy/incload-register 'snip-treemacs :level 3)

(zy/define-key
  "C-x C-d" 'treemacs-select-window)


(provide 'init-file)

;;; init-file.el ends here.
