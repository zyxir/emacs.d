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
(eval-when-compile (require 'init-macros))


;;;; Leader Edit Map

(zy/define-leader-submap
    zy/leader-edit-map "e" "edit"
  "Keymap for a set of editing utilities.")


;;;; Zyutils commands

(zy/define-key
  "M-Q" 'zy/unfill-paragraph)


;;;; Clipboard
;; Clipboard integration in TTY

(straight-use-package 'clipetty)

(unless (display-graphic-p)
  (zy/defsnip 'snip-tty-clipboard
    (require 'clipetty)
    (clipetty-mode 1))
  (zy/incload-register 'snip-tty-clipboard :priority 77))


;;;; Outline

(setq-default outline-minor-mode-cycle t
	      outline-minor-mode-highlight 'append
	      outline-minor-mode-use-margins t)

;; Toggle Outline minor mode with "leader t o"

(zy/define-key
  :keymap 'zy/leader-toggle-map
  "o" 'outline-minor-mode)


;;;; Mwim

(straight-use-package 'mwim)

(zy/defsnip 'snip-mwim
  (require 'mwim)
  (zy/define-key
    [remap move-beginning-of-line] 'mwim-beginning-of-code-or-line))

(zy/incload-register 'snip-mwim :priority 70)


;;;; Scrolling

(zy/defsnip 'snip-scroll
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

  ;; Enable pixel scroll if available

  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode)))

(zy/incload-register 'snip-scroll :priority 75)


;;;; Parentheses

(straight-use-package 'smartparens)

(add-hook 'text-mode-hook 'smartparens-mode)
(add-hook 'prog-mode-hook 'smartparens-mode)

(after! 'smartparens
  (require 'smartparens-config)
  (setq-default sp-highlight-pair-overlay nil
		sp-autoinsert-pair t))

(zy/incload-register 'smartparens :after 'dash)


;;;; Yasnippet

(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)
(straight-use-package 'consult-yasnippet)

(setq-default yas-snippet-dirs
	      (list
	       (expand-file-name "etc/snippets"
				 user-emacs-directory)))
(after! 'yasnippet
  (require 'yasnippet-snippets)
  (add-to-list 'yas-snippet-dirs
	       yasnippet-snippets-dir)
  (yas-global-mode +1)
  ;; Do not auto expand snippet
  (zy/define-key :keymap 'yas-minor-mode-map
    "<tab>" nil
    "TAB" nil)
  ;; Insert snippet with the leader key
  (zy/define-key :keymap 'zy/leader-edit-map
    "s" '("Insert snippet" . consult-yasnippet)
    "S" '("Visit snippet file" . consult-yasnippet-visit-snippet-file)))

(add-hook! '(prog-mode-hook text-mode-hook) 'yas-global-mode)
(zy/incload-register 'yasnippet :level 4)


(provide 'init-editing)

;;; init-editing.el ends here
