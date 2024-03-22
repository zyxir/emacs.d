;;; zy-dashboard.el --- Dashboard setup. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+dashboard' module of the configuration.

;; Every time a new Emacs instance or client frame is created without a file
;; name argument, a beautiful dashboard is displayed. The dashboard is provided
;; by the Dashboard package, but many customizations are presented here.

;;; Code:

(require 'zylib)

(pkg! 'dashboard)

;; Configure Dashboard in advance.
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
   "靜以修身，儉以養德"
   "業精於勤，荒於嬉；行成於思，毀於隨"
   "欲窮千里目，更上一層樓")
 ;; Customize dashboard items.
 dashboard-items '((projects . 5)
                   (recents . 5)
                   (bookmarks . 3)))

;; Setup normal hooks.
(dashboard-setup-startup-hook)

;; Also load the dashboard for clients in daemon mode.
(when (daemonp)
  (setq initial-buffer-choice
        (lambda () (get-buffer-create "*dashboard*"))))

(provide 'zy-dashboard)

;;; zy-dashboard.el ends here
