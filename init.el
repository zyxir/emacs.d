;;; init.el --- Boostrap the config  -*- lexical-binding: t -*-

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file loads other parts of the configuration.

;;; Code:

;; Check Emacs version.
(let ((minver "30.0"))
  (when (version< emacs-version minver)
    (error "Emacs %s or higher is required to run Zyxir's config" minver)))

;; Make other files of this config available to Emacs.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(let (;; `file-name-handler-alist' is consulted each time a file is loaded.
      ;; Unsetting it speeds up startup notably.
      (file-name-handler-alist nil)
      ;; `load-source-file-function' decides what function to be called to do
      ;; code conversion before reading a source file. Since I do not need code
      ;; conversion while loading the configuration, unsetting it reduces tons
      ;; of startup time.
      (load-source-file-function nil)
      ;; Prefer newer files to prevent inconsistency.
      (load-prefer-newer nil))

  ;; Load the custom file now.
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file))

  ;; Load utility functions first.
  (require 'init-util)

  ;; Then other files.
  (require 'init-defaults)
  (require 'init-theming)
  (require 'init-modeline)
  (require 'init-prog)
  (require 'init-org))

(provide 'init)

;;; init.el ends here
