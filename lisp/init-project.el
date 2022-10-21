;;; init-project.el --- Project utilities -*- lexical-binding: t -*-

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

;; Commands and settings about project management.

;;; Code:

(require 'init-common)
(require 'init-keybinding)
(require 'init-load)


;;;; Project Detection
;; Detect projects in Zyprojects

(when (and (boundp 'zy/zyprojects-path) zy/zyprojects-path)
  (zy/defsnip snip-zyprojects
      (:weight 10)
    (require 'project)
    (when (fboundp 'project-remember-projects-under)
      (project-remember-projects-under zy/zyprojects-path))))


;;;; Inbuilt Project Features

(setq-default
 project-switch-commands '((project-find-file "Find file" "f")
			   (consult-ripgrep "Ripgrep" "g")
			   (magit-project-status "Magit" "m")
			   (project-find-dir "Find directory" "d")
			   (project-eshell "Eshell" "s")))


;;;; Leader Project Commands

(zy/define-leader-submap
    zy/leader-project-map "p" "project"
  "Keymap for project management.")
(zy/define-key
  :keymap 'zy/leader-project-map
  ;; Shell command
  "!" '("Run command" . project-shell-command)
  "&" '("Run command async" . project-async-shell-command)
  "s" '("Shell" . project-shell)
  "e" '("Eshell" . project-eshell)
  ;; Find file, buffer or directory
  "D" '("Dired" . project-dired)
  "d" '("Find dir" . project-find-dir)
  "f" '("Find file" . project-find-file)
  "F" '("Find file or ext" . project-or-external-find-file)
  "b" '("Switch to buffer" . consult-project-buffer)
  "k" '("Kill buffers" . project-kill-buffers)
  ;; Grep
  "G" '("Advanced grep" . rg-project)
  "g" '("Consult grep" . consult-ripgrep)
  ;; Version control
  "v" '("Git status" . magit-project-status)
  "V" '("Git dispatch" . magit-dispatch)
  ;; Others
  "p" '("Switch project" . project-switch-project)
  "c" '("Compile" . project-compile)
  "x" '("M-x on project" . project-execute-extended-command))


(provide 'init-project)

;;; init-project.el ends here.
