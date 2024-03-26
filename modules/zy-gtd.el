;;; zy-gtd.el --- Org-based GTD system. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+gtd' module of the configuration.

;; Org-mode can be used as a powerful GTD (getting things done) system via its
;; todo management and agenda features. This file sets up my personal GTD system
;; with processes including idea capturing, entry refiling, and agenda viewing.

;;; Code:

(require 'zylib)

(defvar +gtd-dir nil
  "My directory for the GTD system.")

(defvar +gtd-inbox-file nil
  "My inbox file of the GTD system.
Automatically set when `+personal-zybox-dir' is customized.")

(defvar +gtd-gtd-file nil
  "My GTD file of the GTD system.
Automatically set when `+personal-zybox-dir' is customized.")

(defvar +gtd-someday-file nil
  "My someday file of the GTD system.
Automatically set when `+personal-zybox-dir' is customized.")

(provide 'zy-gtd)

;;; zy-gtd.el ends here
