;;; init-ui.el --- User interface setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-load)


;; Setup theme

(setq modus-themes-italic-constructs nil
      modus-themes-lang-checkers '(background)
      modus-themes-bold-constructs t
      modus-themes-mixed-fonts t
      modus-themes-region '(bg-only no-extend)
      modus-themes-prompts '(background)
      modus-themes-syntax '(green-strings yellow-comments))
(load-theme 'modus-vivendi 'no-confirm)


;; Control-L page break lines

(zy/delay-till after-find-file
  (global-page-break-lines-mode +1)
  (blackout 'page-break-lines-mode))


(provide 'init-ui)

;;; init-ui.el ends here.
