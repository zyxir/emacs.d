;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Unclassified general settings.

;;; Code:

;; Always use the ISO C date format.

(setq system-time-locale "C")

;; Use UTF-8 as default in several places.

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(when (boundp 'buffer-file-coding-system)
  (setq-default buffer-file-coding-system 'utf-8-unix))

;; Persoanl information.

(setq user-full-name "Eric Zhuo Chen"
      user-mail-address "zyxirchen@outlook.com")

;; The location of Zybox, my all-in-one file center.

(defvar zy/zybox-path nil
  "The path of Zybox, the collection of all my files.

This value should be manually set in custom.el, as it is
different on different machines.")

;; If Zybox is not set in custom.el, try to guess one based on
;; `system-type', and warn about this.

(defun zy:file-directory-ret (path)
  (when (file-directory-p path)
    path))

(unless zy/zybox-path
  (require 'cl-extra)
  (let* ((zybox-possible-locs-win64
	  '("C:\\Zybox"
	    "C:\\Users\\zyxir\\Documents\\Zybox"))
	 (zybox-possible-locs-linux
	  '("~/Zybox"
	    "~/Documents/Zybox"))
	 (guessed-zybox-path
	  (cl-some
	   #'zy:file-directory-ret
	   (cond
	    (*win64* zybox-possible-locs-win64)
	    (*linux* zybox-possible-locs-linux)))))
    (if guessed-zybox-path
	(progn
	  (setq zy/zybox-path guessed-zybox-path)
	  (warn
	   "Zybox should be manually set instead of auto detected!"))
      (warn "No Zybox path is detected, many features will be unavailable!"))))

;; Make sure the path is absolute and with a slash.

(when zy/zybox-path
  (setq zy/zybox-path
	(file-name-as-directory
	 (file-truename
	  zy/zybox-path))))

;; Other important paths relative to Zybox, which could also be set in
;; custom.el.

(when zy/zybox-path
  (defvar zy/projects-path
    (concat zy/zybox-path "projects/")
    "The directory of all projects.")
  (defvar zy/std-proj-path
    (concat zy/projects-path "ego/std/std-proj/README.org")
    "My project standard specification."))

;; End of config.

(provide 'init-overall)
