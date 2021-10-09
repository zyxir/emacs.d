;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Miscellaneous major modes/

;;; Code:

(use-package matlab
  :straight matlab-mode
  :config
  ;; Treat .m files as MATLAB files.
  (add-to-list
   'auto-mode-alist
   '("\\.m\\'" . matlab-mode))
  (add-hook 'matlab-mode-hook
	    (lambda ()
	      (auto-fill-mode t)
	      (display-line-numbers-mode t))))

(use-package plantuml-mode
  :straight t
  :config
  (setq plantuml-jar-path (concat zy/3rd-party-path "plantuml/plantuml.jar")
	plantuml-default-exec-mode 'jar))

;; End of config.

(provide 'init-misc)
