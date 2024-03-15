;;; init-theme.el --- Themes and cosmetics.  -*- lexical-binding: t -*-
;;; Commentary:

;; Configure things like themes and cursor blinking here.

;;; Code:

;; Although Modus Themes are included in Emacs 29, the MELPA version has more
;; features and variants.
(require-package 'modus-themes)
(require-package 'solaire-mode)
(require-package 'rainbow-delimiters)
(require-package 'dashboard)

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

;; Use more prominent faces for errors/warnings/notes on terminal since
;; underlines cannot be colored there.
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
                          :inherit 'note))))
(add-hook 'window-setup-hook #'zy/-setup-terminal-err-faces-h)
(add-to-list 'after-make-frame-functions #'zy/-setup-terminal-err-faces-h)

;; Enable Solaire mode to distinguish between file and non-file buffers.
(solaire-global-mode 1)

;; Enable rainbow delimeters for all prog modes.
(add-hook! prog-mode #'rainbow-delimiters-mode)

;; Show a beautiful dashboard on entry.
(setq
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
