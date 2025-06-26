;;; init-prog.el --- Program editing  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Eric Zhuo Chen

;; Author: Eric Zhuo Chen <ericzhuochen@outlook.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file configures Emacs for program editing, primarily focusing
;; on Emacs Lisp.  For other programming languages, I prefer Visual
;; Studio Code.

;;; Code:

(require 'init-util)

(require-package 'consult)
(require-package 'hl-todo)
(require-package 'consult-todo)
(require-package 'corfu)
(require-package 'magit)

;;;; Manage trusted content

;; Trust everything in my Emacs configuration.
(add-to-list 'trusted-content user-emacs-directory)

;;;; Magit

(zy-eval-after-load 'magit
  ;; Show commit time in the status buffer.
  (setq magit-status-margin '(t age magit-log-margin-width nil 18)))

;;;; General programming settings

;; Smart pair matching.
(add-hook 'prog-mode-hook #'electric-pair-local-mode)

;; Highlight and navigate todo keywords.
(add-hook 'prog-mode-hook #'hl-todo-mode)
(keymap-global-set "M-g t" #'consult-todo)
(keymap-global-set "M-g M-t" #'consult-todo)

;; File outlining with `outline-minor-mode'.
(add-hook 'prog-mode-hook #'outline-minor-mode)
(zy-eval-after-load 'outline
  ;; Show buttons in window margins to toggle visibility.
  (setq outline-minor-mode-use-buttons 'in-margins)

  ;; Enable visibility cycling using `TAB' and `S-<TAB>'.
  (setq outline-minor-mode-cycle t)

  ;; Navigate the outline with `consult-outline'.
  (keymap-global-set "M-g o" #'consult-outline)
  (keymap-global-set "M-g M-o" #'consult-outline))

;; Corfu auto completion.
(add-hook 'prog-mode-hook #'corfu-mode)
(zy-eval-after-load 'corfu
  ;; Enable auto completion.
  (setq corfu-auto t)

  ;; Auto-complete for 2+ letters, since the "if" keyword is exactly 2 letters.
  (setq corfu-auto-prefix 2)

  ;; Do not insert for exact match, since this may trigger unwanted snippet
  ;; expansion by some language servers (rust-analyzer, for instance), or
  ;; unwanted spaces when typing in a shell.
  (setq corfu-on-exact-match nil)

  ;; Remember completion history.
  (corfu-history-mode 1)

  ;; Show candidate help message in the echo area after a short delay.
  (setq-default corfu-echo-delay '(1.0 . 0))
  (corfu-echo-mode 1))

;;;; Emacs Lisp settings

;; Use flymake syntax checking for Emacs Lisp.
(use-package flymake
  :hook (emacs-lisp-mode . flymake-mode))

(zy-eval-after-load 'flymake
  ;; Make flymake read `load-path'.
  (setq elisp-flymake-byte-compile-load-path load-path))

;; `auto-insert-mode' inserts skeletons to newly-created files.  Tweak
;; what is inserted for Emacs Lisp files here.
(zy-eval-after-load 'autoinsert
  (setq auto-insert-alist
	(assoc-delete-all '("\\.el\\'" . "Emacs Lisp header")
			  auto-insert-alist))
  (add-to-list
   'auto-insert-alist
   `(("\\.el\\'" . "Emacs Lisp header")
     "Short description: "
     ";;; " (file-name-nondirectory (buffer-file-name)) " --- " str
     "  -*- lexical-binding: t; -*-" '(setq lexical-binding t)
     "

\;; Copyright (C) " (format-time-string "%Y") " " (user-full-name) "

\;; Author: " (user-full-name) " <" (progn user-mail-address) ">

\;; This file is not part of GNU Emacs.

\;; This program is free software: you can redistribute it and/or modify
\;; it under the terms of the GNU General Public License as published by
\;; the Free Software Foundation, either version 3 of the License, or
\;; (at your option) any later version.

\;; This program is distributed in the hope that it will be useful,
\;; but WITHOUT ANY WARRANTY; without even the implied warranty of
\;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\;; GNU General Public License for more details.

\;; You should have received a copy of the GNU General Public License
\;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

\;;; Commentary:

\;; " _ "

\;;; Code:



\(provide '" (file-name-base (buffer-file-name)) ")

\;;; " (file-name-nondirectory (buffer-file-name)) " ends here\n")))

(provide 'init-prog)

;;; init-prog.el ends here
