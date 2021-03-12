;; This is an example custom file.

;; Set the path of Zybox.
(setq zy/zybox-path "C:\\Zybox")	; Windows
;; (setq zy/zybox-path "~/Zybox")		;Linux

;; Set default font and size.
(setq zy/main-font-name "Sarasa Mono CL"
      zy/main-font-size 12)
;; Default size is 11.

;; Configure sis for specific platform.
;; For example, on a GNU/Linux system with fcitx5 setup, the config would be:
;; (with-eval-after-load "sis"
;;   (sis-ism-lazyman-config "1" "2" 'fcitx5))

;; Configure LaTeX PDF viewer.
;; (setq TeX-view-program-list
;;       '(("SumatraPDF" "sumatrapdf %o"))
;;       TeX-view-program-selection
;;       '((output-pdf "SumatraPDF")))

;; OpenCC configuration for Windows.
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
