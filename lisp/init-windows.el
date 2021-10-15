;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Settings about Microsoft Windows.

;;; Code:

;; Run program successfully on Chinese Windows.

(set-language-environment 'Chinese-GB18030)

;; Set encoding to paste Chinese correctly.

(set-next-selection-coding-system 'utf-16-le)
(set-selection-coding-system 'utf-16le-dos)
(set-clipboard-coding-system 'utf-16-le)
(set-default 'process-coding-system-alist
	     '(("[pP][lL][iI][nN][kK]" gbk-dos . gbk-dos)
	       ("[cC][mM][dD][pP][rR][oO][xX][yY]" gbk-dos . gbk-dos)))

;; End of config.

(provide 'init-windows)
