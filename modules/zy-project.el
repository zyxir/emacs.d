;;; zy-project.el --- Project management. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+project' module of the configuration.

;; Project.el is the built-in project management utility of Emacs. It is younger
;; and has less features than Projectile, but I have found it less buggy than
;; Projectile. Since Project.el and Projectile each has its pros and cons, it is
;; adviced to make a module for each of them, and choose one of them manually.

;;; Code:

(require 'zylib)

(after! 'project
  ;; Customize commands used upon switching to another project.
  (setq project-switch-commands '((project-find-file "Find file" ?f)
                                  (project-find-dir "Find directory" ?d)
                                  (project-find-regexp "Find regexp" ?/)
                                  (magit-project-status "Magit" ?g)
                                  (project-vc-dir "VC-Dir" ?v))))

(provide 'zy-project)

;;; zy-project.el ends here
