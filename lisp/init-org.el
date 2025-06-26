;;; init-org.el --- Org-mode and friends  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Eric Zhuo Chen

;; Author: Eric Zhuo Chen <ericzhuochen@outlook.com>

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file configures Org-mode and the GTD and PKM systems built
;; upon it.

;;; Code:

(require 'init-util)

(require-package 'org-appear)
(require-package 'org-modern)
(require-package 'org-roam)

;; Tune Org-mode.
(zy-eval-after-load 'org
  ;; Use special ctrl-a/e.
  (setq org-special-ctrl-a/e t)

  ;; Follow links with RET.
  (setq org-return-follows-link t)

  ;; Show entities as UTF-8 characters.
  (setq org-pretty-entities t)

  ;; Hide emphasis markers.
  (setq org-hide-emphasis-markers t))

;; Use variable-pitch fonts in Org-mode.
(add-hook 'org-mode-hook #'variable-pitch-mode)

;; Use `org-appear' to auto hide/show emphasis markers.
(add-hook 'org-mode-hook #'org-appear-mode)

;; Use Org-modern prettification.
(add-hook 'org-mode-hook #'org-modern-mode)

;; Customize the Unicode symbols Org-modern uses, otherwise some
;; characters don't display properly on macOS.
(zy-eval-after-load 'org-modern
  (setq org-modern-fold-stars
	'(("▶" . "▼") ("▷" . "▽") ("⏵" . "⏷") ("▹" . "▿") ("▸" . "▾"))))

;;;; Org-Roam

;; Leader keychords for Org-roam related commands.
(keymap-set zy-leader-map "s-f" #'org-roam-node-find)
(keymap-set zy-leader-map "s-i" #'org-roam-node-insert)
(keymap-set zy-leader-map "s-j" #'org-roam-dailies-goto-today)
(keymap-set zy-leader-map "s-d" #'org-roam-dailies-goto-date)

(zy-eval-after-load 'org-roam
  ;; Automatically sync the database.
  (org-roam-db-autosync-mode 1))

(provide 'init-org)

;;; init-org.el ends here
