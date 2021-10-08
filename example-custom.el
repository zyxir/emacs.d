;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; This is an example custom.el file. Copy it as custom.el, and uncomment the
;; lines you want.

;;; Code:

;;;; Set the path of Zybox.

;; (setq zy/zybox-path "C:\\Zybox")
;; (setq zy/zybox-path "~/Zybox")

;;;; Set default font and pixel size.

;; (setq zy/custom-font "Source Han Sans HW TC")
;; (setq zy/default-font-size 14)

;;;; Configure sis for specific platform.

;; Linux + Fcitx

;; (with-eval-after-load "sis"
;;   (sis-ism-lazyman-config "1" "2" 'fcitx))

;; Linux + Fcitx5

;; (with-eval-after-load "sis"
;;   (sis-ism-lazyman-config "1" "2" 'fcitx5))

;; Linux + IBus

;; (with-eval-after-load "sis"
;;   (sis-ism-lazyman-config "xkb:us::eng" "OTHER_INPUT_SOURCE" 'ibus))

;; Microsoft Windows
;; Make sure that "im-select.exe" is installed in PATH.

;; (with-eval-after-load "sis"
;;   (sis-ism-lazyman-config "1033" "2052" 'im-select))

;;;; OpenCC configuration for Windows.

;; (setq opencc-configuration-files-prefix
;;       "C:/Zybox/projects/windows-portable-tools/opencc/share/opencc/"
;;       opencc-configuration-files-suffix
;;       ".json"
;;       opencc-configuration-files-original
;;       '("s2t" "t2s" "s2tw" "tw2s" "s2hk" "hk2s" "s2twp" "tw2sp")
;;       opencc-configuration-files nil)
;; (dolist (filename opencc-configuration-files-original)
;;   (let ((fullpath
;; 	 (concat opencc-configuration-files-prefix
;; 		 filename
;; 		 opencc-configuration-files-suffix)))
;;     (add-to-list 'opencc-configuration-files fullpath t)))
