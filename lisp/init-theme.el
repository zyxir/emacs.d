;;; init-theme.el --- Themes and cosmetics.  -*- lexical-binding: t -*-
;;; Commentary:

;; Configure things like themes and cursor blinking here.

;;; Code:

(eval-and-compile (require 'init-basic))

;; Although Modus Themes are included in Emacs 29, the MELPA version has more
;; features and variants.
(require-package 'modus-themes)
(require-package 'solaire-mode)
(require-package 'rainbow-delimiters)
(require-package 'dashboard)

(defcustom zy/theme 'modus-operandi-tinted
  "The theme to use for Emacs.

Set theme with this variable instead of `custom-enabled-themes'
to make the loading of themes more deterministic."
  :type '(choice (const modus-operandi)
                 (const modus-vivendi)
                 (const modus-operandi-tinted)
                 (const modus-vivendi-tinted)
                 (const modus-operandi-tritanopia)
                 (const modus-vivendi-tritanopia)
                 (const modus-operandi-deuteranopia)
                 (const modus-vivendi-deuteranopia))
  :group 'emacs)

;; Require and configure the user-specified theme.
(cond
 ((string-prefix-p "modus-" (symbol-name zy/theme))
  (require 'modus-themes))
 (t nil))

;; Configure Modus Themes.
(after! 'modus-themes
  (setq
   ;; Use more variants to enhance distinguishability.
   modus-themes-italic-constructs t
   modus-themes-bold-constructs t
   modus-themes-mixed-fonts t
   modus-themes-variable-pitch-ui t)

  ;; Use more prominent faces for errors/warnings/notes on terminal since
  ;; underlines cannot be colored there.
  (eval-and-compile
    (defun zy/-setup-terminal-err-faces-h (&rest _)
      "Setup more prominent error faces for FRAME.
Only works for non-graphical frames."
      (when-let* ((frame (selected-frame)))
        (unless (display-graphic-p frame)
          (set-face-attribute 'modus-themes-lang-error frame
                              :inherit 'error)
          (set-face-attribute 'modus-themes-lang-warning frame
                              :inherit 'warning)
          (set-face-attribute 'modus-themes-lang-note frame
                              :inherit 'note)))))

  (add-hook 'window-setup-hook #'zy/-setup-terminal-err-faces-h)
  (add-to-list 'after-make-frame-functions #'zy/-setup-terminal-err-faces-h))

;; Load the theme.
(load-theme zy/theme 'no-confirm)

;; Enable Solaire mode to distinguish between file and non-file buffers.
(solaire-global-mode 1)

;; Enable rainbow delimeters for all prog modes.
(add-hook! prog-mode #'rainbow-delimiters-mode)

;; Show a beautiful dashboard on entry.
(setq-default
 ;; Use project.el for projects.
 dashboard-projects-backend 'project-el
 ;; Center the dashboard.
 dashboard-center-content t
 ;; Use the alternate banner, and show no banner title.
 dashboard-startup-banner 'logo
 dashboard-banner-logo-title nil
 ;; Use ancient Chinese quotes as footer messages.
 dashboard-footer-messages
 '("學如逆水行舟，不進則退"
   "不積跬步，無以致千里；不積小流，無以成江海"
   "博觀而約取，厚積而薄發"
   "業精於勤，荒於嬉；行成於思，毀於隨"
   "欲窮千里目，更上一層樓")
 ;; Customize dashboard items.
 dashboard-items '((projects . 5)
                   (recents . 5)
                   (bookmarks . 3)))

(dashboard-setup-startup-hook)

;; Also load the dashboard for clients in daemon mode.
(when (daemonp)
  (setq initial-buffer-choice
        (lambda () (get-buffer-create "*dashboard*"))))

(provide 'init-theme)

;;; init-theme.el ends here
