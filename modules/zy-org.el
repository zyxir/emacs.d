;;; zy-org.el --- Org-mode. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+org' module of the configuration.

;; Org-mode is a very powerful feature of Emacs (may be the most powerful one)
;; for keeping notes, maintaining todo lists, doing project planning, and even
;; publishing contents. It is very versatile and based on plain-text. As someone
;; who uses Org-mode extensively, I have splitted my Org config into several
;; modules, and this module only configures Org as a markup language.

;;; Code:

(require 'zylib)

(pkg! 'org)
(pkg! 'org-modern)

;; Indent sections by depth by default.
(setq-default org-startup-indented t)

;; Require Org features at startup for daemon sessions, since Org is both heavy
;; and frequently-used.
(daemon-require! 'org 'org-attach 'ob 'org-modern)

(after! 'org
  ;; Make tags next to headlines. Org-modern will make tags pretty.
  (setq org-auto-align-tags nil
        org-tags-column 0)

  ;; Use special ctrl-a/e.
  (setq org-special-ctrl-a/e t)

  ;; Use prettier ellipsis.
  (setq org-ellipsis "\u2026")

  ;; Use Org-tempo for block expansion. For instance, "<s" expands to a source
  ;; code block, and "<q" expands to a quote block.
  (require 'org-tempo)

  (add-hook! 'org-mode-hook
    ;; Use variable-pitch-mode for Org buffers.
    (variable-pitch-mode 1)
    ;; Make Olivetti use a larger width to accomodate `org-indent-mode'.
    (when (boundp 'olivetti-body-width)
      (setq-local olivetti-body-width 90))))

;; Configure attachments.
(after! 'org-attach
  ;; Use my favorite attachment directory. The default ("data") is too generic.
  (setq org-attach-id-dir "_org-att"))

;; Setup Org-babel with some additional languages.
(after! 'ob
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages
           '(;; For literate scripting.
             (shell . t)
             ;; Also for literate scripting.
             (python . t)))))

;; Prettify Org buffers with Org-modern.
(add-hook! 'org-mode-hook (org-modern-mode 1))
(add-hook! 'org-agenda-finalize-hook #'org-modern-agenda)

(after! 'org-modern
  ;; Show blocks and symbols with the fixed-pitch face.
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-modern-symbol nil :inherit 'fixed-pitch))

;; Exporting settings.
(after! 'ox
  ;; Do not export TOC or tags, unless asked to.
  (setq org-export-with-toc nil
        org-export-with-tags nil)

  ;; Exporting the characters "_" or "^" will mess up terminologies like
  ;; "Pop!_OS". Since I seldom use subscripts or superscripts, simply turn them
  ;; off. Turn them on file-locally if they are really needed.
  (setq org-export-with-sub-superscripts nil))

;; Exporting to HTML.
(after! 'ox-html
  ;; The following lines enable exporting as an MHTML file that embeds images.
  ;; See URL `https://niklasfasching.de/posts/org-html-export-inline-images'

  (defun +org-html-export-to-mhtml (async subtree visible body)
    (cl-letf
        (((symbol-function 'org-html--format-image)
          (defun +org-format-image-inline (source attributes info)
            (let* ((ext (file-name-extension source))
                   (prefix (if (string= "svg" ext)
                               "data:image/svg+xml;base64,"
                             "data:;base64,"))
                   (data (with-temp-buffer (url-insert-file-contents source)
                                           (buffer-string)))
                   (data-url (concat prefix (base64-encode-string data)))
                   (attributes (org-combine-plists
                                `(:src ,data-url) attributes)))
              (org-html-close-tag
               "img"
               (org-html--make-attribute-string attributes)
               info)))))
      (org-html-export-to-html async subtree visible body)))

  (org-export-define-derived-backend
   'html-inline-images 'html
   :menu-entry '(?h
                 "Export to HTML"
                 ((?m "As MHTML file" +org-html-export-to-mhtml)))))

(provide 'zy-org)

;;; zy-org.el ends here
