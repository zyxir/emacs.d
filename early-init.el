;;; early-init.el --- Pre-initialization config -*- lexical-binding: t -*-


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

;; This file is read before the GUI is initialized.

;;; Code:

(setq default-frame-alist '(;; Maximize Emacs by default
			    (fullscreen . maximized)
			    ;; Disable menu bar.
                            (menu-bar-lines . nil)
                            ;; Disable scroll bars
                            (horizontal-scroll-bars . nil)
                            (vertical-scroll-bars . nil)
                            ;; Disable tool bar
                            (tool-bar-lines . 0))
      ;; Configure GUI features early for faster startup
      menu-bar-mode nil
      scroll-bar-mode nil
      tool-bar-mode nil
      frame-resize-pixelwise t
      ;; This disables package.el once and for all
      package-enable-at-startup nil)

;; Things that I no longer use

;; Don't waste time to check file modification time
;; (setq load-prefer-newer t)


(provide 'early-init)

;;; early-init.el ends here
