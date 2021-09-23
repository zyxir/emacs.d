;; This is an example custom file.

;; Set the path of Zybox.
(setq my/zybox-path "C:\\Zybox")	; Windows
;; (setq my/zybox-path "~/Zybox")		;Linux

;; Set default font and size.
(setq my/main-font-name "Sarasa Mono TC"
      my/main-font-size 12)
;; Default size is 11.

;; Configure sis for specific platform.
;; For example, on a GNU/Linux system with fcitx5 setup, the config would be:
;; (with-eval-after-load "sis"
;;   (sis-ism-lazyman-config "1" "2" 'fcitx5))
