;;; zy-roam.el --- PKM via Org-roam. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+roam' module of the configuration.

;; It sets up a PKM (personal knowledge management) system via the package
;; Org-roam. The note-taking method named Zettelkasten is the core of this
;; system. All Org-roam-related commands are prefixed with "<leader> r".

;;; Code:

(require 'zylib)

(pkg! 'org-roam)
(pkg! 'org-roam-ui)

(after! 'org-roam
  ;; Automatically sync the database.
  (org-roam-db-autosync-mode 1)

  ;; Ignore all headlines with the ATTACH tag.
  (setq org-roam-db-node-include-function
        (defun +roam-no-attach-node-fn (&rest _)
          (not (member "ATTACH" (org-get-tags))))))

;; Global keys.
(after! '+leader
  (keybind! nil +leader-r-map
    "f" (cons "Find Node" #'org-roam-node-find)
    "i" (cons "Ins Node" #'org-roam-node-insert)
    "c" (cons "Capture" #'org-roam-capture)
    "s" (cons "Sync" #'org-roam-db-sync)
    "u" (cons "UI" #'org-roam-ui-mode)))

;; Keys inside an Org-roam buffer (added to org-mode-map).
(after! '(+leader +org org-roam-mode)
  (keybind! nil +org-map
    "A" (cons "Roam Alias" #'org-roam-alias-add)
    "l" (cons "Roam Buffer" #'org-roam-buffer-toggle)))

(provide 'zy-roam)

;;; zy-roam.el ends here
