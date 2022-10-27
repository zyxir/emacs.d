;;; init.el --- the main config -*- lexical-binding: t -*-

;; Copyright (C) 2022 Eric Zhuo Chen

;; Author: Eric Zhuo Chen <zyxirchen@outlook.com>
;; Maintainer: Eric Zhuo Chen <zyxirchen@outlook.com>
;; Created: 2022-10-28


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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This file is the main part of my configuration.

;; The load order of Emacs is as follows:

;;   > early-init.el
;;   > init.el
;;   > hook: `after-init-hook'
;;   > hook: `emacs-startup-hook'
;;   > hook: `window-setup-hook'

;;; Code:

(eval-when-compile (require 'subr-x))

;;; Preparations

;;;; Version Check

;; Check version at both compile and runtime.
(eval-and-compile
  ;; The minimum version to run this configuration is 28.1.
  (when (< emacs-major-version 28)
    (user-error
     "Emacs version is %s, but this config requires 28.1 or newer"
     emacs-version)))

;;;; Startup Hacks

;; Some hacks that make startup faster.

(let (;; Store these initial values for latter usage.
      (file-name-handler-alist-initial file-name-handler-alist)
      (mode-line-format-initial mode-line-format))
  (setq
   ;; Inhibit garbage collection at startup.
   gc-cons-threshold most-positive-fixnum
   gc-cons-percentage 0.6
   ;; `file-name-handler-alist' is consulted on each call to `require', `load',
   ;; or various file/io functions (like `expand-file-name' or `file-remote-p').
   ;; Setting its value to nil can make startup time even shorter.
   file-name-handler-alist nil
   ;; Disable the mode line can save some time, too.
   mode-line-format nil
   ;; Disable redisplays and messages can also make things faster.
   inhibit-redisplay t
   inhibit-message t)
  (add-hook
   'emacs-startup-hook
   (lambda ()
     ;; Reconfigure these settings after startup.
     (setq
      ;; 20 MB is an appropriate value, used by Purcell.
      gc-cons-threshold (* 20 1024 1024)
      ;; 0.1 is the default value.
      gc-cons-percentage 0.1
      ;; Merge its initial value with its current value in case it is
      ;; modified during startup.
      file-name-handler-alist (delete-dups
			       (append file-name-handler-alist
				       file-name-handler-alist-initial))
      mode-line-format mode-line-format-initial
      inhibit-redisplay nil
      inhibit-message nil))))

;;;; Globals

(defgroup zyxir nil
  "Zyxir's customization layer over Emacs."
  :group 'emacs)

(provide 'init)
;;; init.el ends here
