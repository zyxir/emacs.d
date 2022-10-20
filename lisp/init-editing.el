;;; init-editing.el --- Editing enhancements -*- lexical-binding: t -*-


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

;; Commands and packages that enhances the editing experience.

;;; Code:

(require 'init-keybinding)


;; Leader edit map

(zy/define-leader-submap
    zy/leader-edit-map "e" "edit"
  "Keymap for a set of editing utilities.")


;; Clipboard integration in TTY

(straight-use-package 'clipetty)

(unless (display-graphic-p)
  (zy/defsnip snip-tty-clipboard
      (:weight 77)
    "Enable clipboard in TTY."
    (require 'clipetty)
    (clipetty-mode 1)))


;; Setup Mwim

(straight-use-package 'mwim)

(zy/defsnip snip-mwim
    (:weight 70)
  "Mwim is for move what I mean."
  (require 'mwim)
  (zy/define-key
    [remap move-beginning-of-line] 'mwim-beginning-of-code-or-line
    [remap move-end-of-line] 'mwim-end-of-line-or-code))


;; Enhance scroll experience.

(straight-use-package 'beacon)

(zy/defsnip snip-scroll
    (:weight 75)
  "Enhance scroll experience.

Scroll only 0.618 page with C-v and M-v, and highlight the cursor
with a beam after each scroll, via the package Beacon."
  ;; Redefine "near full screen" for scroll commands

  (setq scroll-error-top-bottom t)
  (defun zy/-around-scroll (oldfun &rest arg)
    "Wrap scroll command OLDFUN so that it only scroll 0.618 screen.

ARG is the arguments passed to OLDFUN."
    (dlet ((next-screen-context-lines
	    (round (* 0.382 (window-body-height)))))
      (apply oldfun arg)))
  (advice-add 'scroll-up-command :around 'zy/-around-scroll)
  (advice-add 'scroll-down-command :around 'zy/-around-scroll)

  ;; Highlight the cursor with Beacon after each scroll

  (require 'beacon)
  (beacon-mode 1))


;; Setup Smartparens

(straight-use-package 'smartparens)

(zy/defsnip snip-smartparens
    (:events 'find-file :weight 0 :dependencies 'dash)
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook 'smartparens-mode))


;; Setup Yasnippet

(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)
(straight-use-package 'consult-yasnippet)

(zy/defsnip snip-yasnippet
    (:events 'find-file :weight 0)
  (require 'yasnippet)
  (require 'yasnippet-snippets)
  (setq-default yas-snippet-dirs
		(list
		 (expand-file-name "etc/snippets"
				   user-emacs-directory)
		 yasnippet-snippets-dir))
  (yas-global-mode +1)
  ;; Do not auto expand snippet
  (zy/define-key :keymap 'yas-minor-mode-map
    "<tab>" nil
    "TAB" nil)
  ;; Insert snippet with the leader key
  (zy/define-key :keymap 'zy/leader-edit-map
    "s" '("Insert snippet" . consult-yasnippet)
    "S" '("Visit snippet file" . consult-yasnippet-visit-snippet-file)))


(provide 'init-editing)

;;; init-editing.el ends here
