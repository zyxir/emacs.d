;;; early-init.el --- Pre-initialization file  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Eric Zhuo Chen

;; Author: Eric Zhuo Chen <ericzhuochen@outlook.com>
;; Created: 09 Jun 2025

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs 27+ loads this file before calling `package-initialize' and
;; initializing GUI.

;;; Code:

;; Garbage collection reduces startup time.  This line inhibits GC
;; during startup.  Once the GCMH package is loaded it will take over
;; GC.
(setq gc-cons-threshold most-positive-fixnum)

;; However, if the GCMH mode is not activated after startup for some
;; reason, set the threshold to a appropriate value, otherwise Emacs
;; might crash.  This should be a rare case, possibly due to some
;; accidental code edits, therefore these lines only serve as the last
;; resort.
(add-hook 'emacs-startup-hook
          (defun zy-emergency-fix-gc-h (&rest _)
            "Fix GC if GCMH is not loaded."
            (unless (bound-and-true-p gcmh-mode)
              (setq gc-cons-threshold (* 16 1024 1024)))))

;; Set default frame size.
(setq initial-frame-alist
      '((width . 86)
	(height . 38)))

;; Disable the tool bar and scroll bar for a cleaner UI.
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Unlike the tool bar or scroll bar, the menu bar is not disabled
;; because it does not occupy window space on macOS, and it provides
;; discoverability for some less-used commands.

;; Enable the quickstart feature.  This feature concatenates all
;; autoload statements emitted by installed packages into a single
;; file, the `package-quickstart-file', thus speeding up startup
;; greatly if there are a lot of packages.  However, if the quickstart
;; file is not updated timely for some reason and this causes trouble,
;; run `package-quickstart-refresh' manually to fix it.
(setq-default package-quickstart t)

(provide 'early-init)

;;; early-init.el ends here
