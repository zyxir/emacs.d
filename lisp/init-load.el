;;; init-load.el --- Configuration code loading -*- lexical-binding: t -*-

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

;; 1. Manage the loading (autoloading) of packages (as Borg drones).
;; 2. Provide advanced lazy-loading macros like `zy/delay-till'.

;;; Code:


;; Autoload Borg functions

(autoload 'borg-assimilate "borg" nil 'interactive)
(autoload 'borg-build "borg" nil 'interactive)
(autoload 'borg-clone "borg" nil 'interactive)
(autoload 'borg-remove "borg" nil 'interactive)
(autoload 'borg-initialize "borg" nil nil)
(autoload 'borg-insert-update-message "borg" nil 'interactive)


;; Loaddef collector

(defvar zy/default-load-path load-path
  "Default `load-path' value of Emacs.")

(defun zy/collect-loaddefs (file)
  "Collect all and load path and loaddefs into a single file FILE."
  (message "Generating single big loaddefs file.")
  ;; Populate `load-path' with borg
  (setq load-path zy/default-load-path)
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (borg-initialize)
  ;; Delete FILE if it already exists
  (when (file-exists-p file)
    (delete-file file))
  ;; Generate the single loaddefs file
  (condition-case-unless-debug e
      (with-temp-file file
	(setq-local coding-system-for-write 'utf-8)
	(let ((standard-output (current-buffer))
	      (print-quoted t)
	      (print-level nil)
	      (print-length nil)
	      (home (expand-file-name "~"))
	      the-load-path
	      drones-path
	      autoloads-file
	      loaddefs-file)
	  (insert ";; -*- lexical-binding: t; coding: utf-8; no-native-compile: t -*-\n"
                  ";; This file is generated from enabled drones.\n")
	  ;; Collect drones' path and full `load-path'
	  (dolist (path load-path)
	    (when (string-prefix-p (expand-file-name user-emacs-directory)
				   (expand-file-name path))
	      (push path drones-path))
	    (if (string-prefix-p home path)
		(push (concat "~" (string-remove-prefix home path)) the-load-path)
	      (push path the-load-path)))
	  (push (expand-file-name "lisp" user-emacs-directory) the-load-path)
	  (setq the-load-path (cl-remove-if #'(lambda (path)
						(file-equal-p path user-emacs-directory))
					    the-load-path))
	  (prin1 `(set `load-path ',(nreverse the-load-path)))
	  (insert "\n")
	  ;; Insert all clone's autoloads.el and loaddefs.el to this file
	  (dolist (path drones-path)
	    (when (file-exists-p path)
	      (setq autoloads-file (car (directory-files path 'full ".*-autoloads.el\\'"))
		    loaddefs-file (car (directory-files path 'full ".*-loaddefs.el\\'")))
	      (when (and autoloads-file
			 (file-exists-p autoloads-file))
		(insert-file-contents autoloads-file))
	      (when (and loaddefs-file
			 (file-exists-p loaddefs-file))
		(insert-file-contents loaddefs-file))))
	  ;; Remove all #$ load cache
	  (goto-char (point-min))
	  (while (re-search-forward "\(add-to-list 'load-path.*#$.*\n" nil t)
	    (replace-match ""))
	  (goto-char (point-min))
	  (while (re-search-forward "\(add-to-list 'load-path.*\n.*#$.*\n" nil t)
	    (replace-match ""))
	  ;; Write local variables region
	  (goto-char (point-max))
	  (insert "\n"
                  "\n;; Local Variables:"
                  "\n;; version-control: never"
                  "\n;; no-update-autoloads: t"
                  "\n;; End:"
		  ))
	t)
    (error (delete-file file)
	   (signal 'zy/collect-loaddefs-error (list file e)))))


;; Collect loaddefs if .gitmodules is newer, otherwise load loaddefs

(defvar zy/loaddefs-path user-emacs-directory
  "Path for the loaddefs file of ZyEmacs.")

(defvar zy/loaddefs-file nil
  "Currently loaded loaddefs file of ZyEmacs.")

(defun zy/genload (&optional no-ensure)
  "Initialize load path and loaddefs.

This function re-generates the loaddefs file for ZyEmacs, and
delete the old one.  If NO-ENSURE is non-nil, only re-generate
when there is no loaddefs file with filename corresponding to the
modification time of .gitmodules.

The loaddefs file, whether newly generated or not, will be loaded
by this function.

Return the path of the loaded file."
  (interactive)
  (let* ((gitmodules-file (expand-file-name ".gitmodules" user-emacs-directory))
	 (gitmodules-modtime (time-convert (file-attribute-modification-time
					    (file-attributes gitmodules-file))
					   'integer)))
    ;; Determine the loaddefs file name
    (setq zy/loaddefs-file (expand-file-name (format "loaddefs-%d.el" gitmodules-modtime)
					     zy/loaddefs-path))
    ;; Re-generate the loaddefs file in need
    (when (or (not no-ensure)
	      (not (file-exists-p zy/loaddefs-file)))
      ;; Re-generate the loaddefs file
      (dolist (file (directory-files zy/loaddefs-path 'full "loaddefs-[0-9]+\\.elc?\\'"))
	(delete-file file))
      (zy/collect-loaddefs zy/loaddefs-file)
      ;; Compile the newly generated file
      (with-no-warnings
	(byte-compile-file zy/loaddefs-file)
	(native-compile zy/loaddefs-file)))
    ;; Load the loaddefs file
    (load zy/loaddefs-file)
    ;; Return the path of the loaddefs file
    zy/loaddefs-file))

(zy/genload 'no-ensure)


;; Delayed loading

;; Delayed till the first user input

(defvar zy/delayed-till-user-input-funcs '()
  "A list of delayed functions.

These functions are executed before the first user input.")

(defun zy/-delayed-till-user-input (&rest ignored)
  "This function will be executed at the first user input.

IGNORED means that all input arguments will be ignored."
  (dolist (func zy/delayed-till-user-input-funcs)
    (funcall func))
  (remove-hook 'pre-command-hook #'zy/-delayed-till-user-input))

(add-hook 'pre-command-hook #'zy/-delayed-till-user-input)

(defmacro zy/delay-till-user-input (&rest body)
  "Execute BODY after the first user input."
  (declare (debug (form def-body)))
  `(add-to-list (quote zy/delayed-till-user-input-funcs)
		(lambda () ,@body)))

;; Delayed till the first time a function is executed

(defvar zy/--delay-cnt-- 0
  "Counter for delayed functions.")

(defmacro zy/delay-till (func &rest body)
  "Execute BODY after FUNC is executed."
  (declare (indent 1) (debug (form def-body)))
  (let* ((delayed-func-name (format "zy/-delayed-func-%d" zy/--delay-cnt--))
	 (delayed-func (make-symbol delayed-func-name))
	 (defun-sexp
	  `(defun ,delayed-func (&rest ignored)
	     ,@body
	     (advice-remove #',func #',delayed-func)))
	 (advice-sexp
	  `(advice-add #',func :before #',delayed-func)))
    (setq zy/--delay-cnt-- (+ zy/--delay-cnt-- 1))
    `(prog1 ,defun-sexp ,advice-sexp)))


;; Function to recompile the config

(defun zy/recompile-config ()
  "Recompile the ZyEmacs config."
  (interactive)
  (let* ((files (cl-mapcar
		 #'(lambda (dir)
		     (expand-file-name dir user-emacs-directory))
		 '("early-init.el" "init.el")))
	 (loaddefs-file (zy/genload 'no-ensure))
	 (files (cons loaddefs-file files))
	 (dirs (cl-mapcar
		#'(lambda (dir)
		    (file-name-as-directory
		     (expand-file-name dir user-emacs-directory)))
		'("lisp")))
	 (inhibit-message t))
    (dolist (file files)
      (when (native-comp-available-p)
	(native-compile file))
      (byte-recompile-file file 'force 0))
    (dolist (dir dirs)
      (dolist (file (directory-files dir 'full ".*\\.el\\'" 'nosort))
	(when (native-comp-available-p)
	  (native-compile file))
	(byte-recompile-file file 'force 0))))
  t)


(provide 'init-load)

;;; init-load.el ends here
