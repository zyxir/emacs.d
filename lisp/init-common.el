;;; init-common.el --- Configure common things -*- lexical-binding: t -*-

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

;; Configure native Emacs settings and common things

;;; Code:

(require 'cl-lib)
(require 'init-load)


;; Set flags

(setq auto-save-default nil
      disabled-command-function nil
      frame-title-format '("" "ZyEmacs" " [%b]")
      inhibit-compacting-font-caches t
      initial-scratch-message ""
      initial-major-mode 'fundamental-mode
      make-backup-files nil
      read-process-output-max (* 1024 1024)
      system-time-locale "C"
      use-dialog-box nil
      word-wrap-by-category t)

(setq-default fill-column 80)

(setq-default native-comp-async-report-warnings-errors nil)


;; Set everything to UTF-8.

(set-language-environment "UTF-8")


;; Inbuilt modes

(mapc (lambda (hook)
	(add-hook hook
		  (lambda ()
		    (setq-local show-trailing-whitespace t)
		    (display-line-numbers-mode +1)
		    (hl-line-mode +1))))
      '(prog-mode-hook text-mode-hook))

(zy/defsnip snip-inbuilt-modes
    (:events 'pre-command :weight 100)
  "Setup inbuilt Emacs minor modes."
  (unless (display-graphic-p)
    (xterm-mouse-mode +1))
  (column-number-mode +1)
  (delete-selection-mode +1)
  (global-subword-mode +1)
  (setq-default recentf-max-saved-items 100)
  (recentf-mode 1)
  (require 'kinsoku))

(zy/defsnip snip-file-inbuilt-modes
    (:events 'find-file :weight 0)
  (global-auto-revert-mode +1)
  (save-place-mode +1))


;; WSL detection

(defun zy/wsl-p ()
  "Return t if ZyEmacs is running on WSL."
  (if (boundp 'zy/wsl-p)
      zy/wsl-p
    (defvar zy/wsl-p
      (and (memq system-type '(gnu/linux linux))
	   (equal 0
		  (call-process "grep" "/proc/version" t nil
				"-q" "[Mm]icrosoft"))))
    zy/wsl-p))


;; Personalization

(setq user-full-name "Eric Zhuo Chen"
      user-mail-address "zyxirchen@outlook.com")

(defgroup zyemacs nil
  "ZyEmacs customization options."
  :group 'emacs)


;; My personal directories

(defcustom zy/zybox-path nil
  "The path of Zybox, where my personal files resides."
  :type 'directory
  :group 'zyemacs)

(defcustom zy/zyprojects-path nil
  "The path of Zyprojects, where my VC projects resides."
  :type 'directory
  :group 'zyemacs)

;; If the path of Zybox is not loaded from custom.el, try to auto-detect it, and
;; save it into custom.el

(unless zy/zybox-path
  (let* (;; OS-dependent possible locations of Zybox
	 (zybox-possible-locs
	  (cond
	   ((eq system-type 'gnu/linux)
	    '("~/Zybox" "~/Documents/Zybox" "/mnt/c/Zybox"
	      "/mnt/c/Users/zyxir/Zybox"
	      "/mnt/c/Users/zyxir/Documents/Zybox"))
	   ((memq system-type '(ms-dos windows-nt cygwin))
	    '("C:\\Zybox" "C:\\Users\\zyxir\\Zybox"
	      "C:\\Users\\zyxir\\Documents\\Zybox"))
	   (t nil)))
	 ;; Use the first search result if there is one
	 (path-found (cl-find-if #'file-directory-p
				 zybox-possible-locs)))
    (when path-found
      (message "Zybox detected at \'%s\'" path-found)
      (customize-save-variable 'zy/zybox-path path-found))))

;; Detect Zyprojects based on Zybox

(when (and zy/zybox-path (not zy/zyprojects-path))
  (let ((maybe-zyprojects
	 (expand-file-name
	  "Zyprojects"
	  (file-name-directory zy/zybox-path))))
    (when (file-directory-p maybe-zyprojects)
      (message "Zyprojects detected at \'%s\'" maybe-zyprojects)
      (customize-save-variable 'zy/zyprojects-path
			       maybe-zyprojects))))


(provide 'init-common)

;;; init-common.el ends here.
