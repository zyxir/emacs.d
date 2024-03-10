;;; init-theme.el --- Themes and cosmetics  -*- lexical-binding: t -*-
;;; Commentary:

;; Configure things like themes and cursor blinking here.

;;; Code:

;; Although Modus Themes are included in Emacs 29, the MELPA version has more
;; features and variants.
(require-package 'modus-themes)
(require-package 'solaire-mode)
(require-package 'rainbow-delimiters)

;; Configure Modus Themes.
(setq
 ;; Use more variants to enhance distinguishability.
 modus-themes-italic-constructs t
 modus-themes-bold-constructs t
 modus-themes-mixed-fonts t
 modus-themes-variable-pitch-ui t)

;; Enable Modus Operandi Tinted or the user-preferred theme.
(dolist (theme (or custom-enabled-themes '(modus-operandi-tinted)))
  (load-theme theme 'no-confirm))

;; Enable Solaire mode to distinguish between file and non-file buffers.
(solaire-global-mode 1)

;; Enable rainbow delimeters for all prog modes.
(add-hook! prog-mode #'rainbow-delimiters-mode)

(provide 'init-theme)

;;; init-theme.el ends here
