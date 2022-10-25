;;; init.el --- Load the full configuration -*- lexical-binding: t -*-


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;;;; Minimum Version

(let ((minver "28.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old --- this config requires v%s or higher" minver)))


;;;; Speed Up Startup

(let ((normal-gc-cons-threshold (* 16 1024 1024))
      (normal-gc-cons-percentage gc-cons-percentage)
      (normal-file-name-handler-alist file-name-handler-alist)
      (init-gc-cons-threshold (* 128 1024 1024))
      (init-gc-cons-percentage 0.6)
      (init-file-name-handler-alist nil))
  (setq gc-cons-threshold init-gc-cons-threshold
	gc-cons-percentage init-gc-cons-percentage
	file-name-handler-alist init-file-name-handler-alist)
  (add-hook 'emacs-startup-hook
            (lambda ()
	      (setq gc-cons-threshold normal-gc-cons-threshold
		    gc-cons-percentage normal-gc-cons-percentage
		    file-name-handler-alist (nconc
					     file-name-handler-alist
					     normal-file-name-handler-alist)))))


;;;; Startup Benchmarking

(push (expand-file-name "lisp" user-emacs-directory) load-path)
(require 'init-benchmark)


;;;; Bootstrap Config

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file) (load custom-file))

;; Loading
(require 'init-load)
;; (require 'init-loaddefs)

;; Core
(require 'init-common)
(require 'init-keybinding)

;; Features
(require 'init-completion)
(require 'init-editing)
(require 'init-file)
(require 'init-lingual)
(require 'init-programming)
(require 'init-project)
(require 'init-search)
(require 'init-shell)
(require 'init-tex)
(require 'init-ui)
(require 'init-vc)

;; Major modes
(require 'init-elisp)
(require 'init-misc)
(require 'init-org)

;; Provide the `zyemacs' feature so that my Zyutils package can load, as my
;; configuration is a dependency of it.
(require 'zyemacs)


(provide 'init)

;;; init.el ends here
