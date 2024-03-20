;;; init-org.el --- Org-mode settings.  -*- lexical-binding: t -*-
;;; Commentary:

;; This file configures everything about Org-mode, including the vast collection
;; of third-party packages.

;;; Code:

(eval-and-compile (require 'init-basic))

(pkg! 'org)
(pkg! 'org-modern)

;;;; Custom Files and Directories.

(defvar zy/gtd-inbox-file nil
  "My inbox file of the GTD system.
Automatically set when `zy/zybox-dir' is customized.")

(defvar zy/gtd-gtd-file nil
  "My GTD file of the GTD system.
Automatically set when `zy/zybox-dir' is customized.")

(defvar zy/gtd-someday-file nil
  "My someday file of the GTD system.
Automatically set when `zy/zybox-dir' is customized.")

;;;; Basic Org-mode configuration.

;; Indent sections by depth by default.
(setq-default org-startup-indented t)

;; Configure Org.
(after-or-now! 'org
  (setq
   ;; Make tags next to headlines.
   org-auto-align-tags nil
   org-tags-column 0
   ;; Use special ctrl-a/e.
   org-special-ctrl-a/e t
   ;; Use prettier ellipsis.
   org-ellipsis "\u2026")

  ;; Use Org-tempo for block expansion.
  (require 'org-tempo)

  (add-hook! org-mode
    ;; Use variable-pitch-mode for Org buffers.
    (variable-pitch-mode 1)
    ;; Make Olivetti use a larger width to accomodate `org-indent-mode'.
    (when (boundp 'olivetti-body-width)
      (setq-local olivetti-body-width 90))))

;; Configure Org-attach.
(after-or-now! 'org-attach
  ;; Use my favorite attachment directory. The default ("data") is too generic.
  (setq org-attach-id-dir "_org-att"))

;; Setup Org-babel with some additional languages.
(after-or-now! ob
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages
           '(;; For literate scripting.
             (shell . t)
             ;; Also for literate scripting.
             (python . t)))))

;; Prettify Org buffers with Org-modern.
(add-hook! org-mode (org-modern-mode 1))

(after-or-now! 'org-modern
  ;; Show blocks and symbols with the fixed-pitch face.
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-modern-symbol nil :inherit 'fixed-pitch))

(provide 'init-org)

;;; init-org.el ends here
