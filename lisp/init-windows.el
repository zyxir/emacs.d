;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Settings about Microsoft Windows.

;;; Code:

;; Set encoding to paste Chinese correctly.

(set-selection-coding-system 'utf-16le-dos)
(set-default 'process-coding-system-alist
	     '(("[pP][lL][iI][nN][kK]" gbk-dos . gbk-dos)
	       ("[cC][mM][dD][pP][rR][oO][xX][yY]" gbk-dos . gbk-dos)))

;; End of config.

(provide 'init-windows)
