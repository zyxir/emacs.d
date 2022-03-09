;;; zy-block.el --- configurable code blocks.

(require 'zy-lisp-utils)


;; Keyword management.

(defvar zb-keyword-list nil
  "List of available keywords for zy-block.")

(defvar zb-keyword-type-plist nil
  "List of keywords and their types for zy-block.")

(defvar zb-keyword-func-plist nil
  "List of keywords and their wrapper cuntions for zy-block.")

(defvar zb-flag-list nil
  "List of all flag keywords for zy-block.")

(defvar zb-global-flag-alist nil
  "Alist of globally enabled flags and their default values.

These flag keywords will be enabled for every zy-blocks, even if
they are not explicitly given. However, if a global flag is
explicitly given, the given value will be used instead of the
default value.")

(defun zb-define-keyword (keyword type func &rest args)
  "Define a new keyword KEYWORD for zy-block.

KEYWORD is the keyword, which is a symbol starts with the
comma (:) sign, like ':when' or ':pkg'.

TYPE is the type of the keyword, determing the way `zb' parses
its argument. Possible types are:

'single' -- the keyword reads a single s-expression as its
argument.

'multiple' -- the keyword reads multiple s-expressions, which are
then stored in a list to form its argument.

'flag' -- the keyword is a flag that reads zero or one
s-expression as its argument. If no s-expression is given, its
argument would be 't'. Additionally, flag keywords can be
globally enabled by adding them and their default values to
`zb-global-flag-alist'.

FUNC is the wrapper function for the keyword.

KEYWORD are added to corresponding lists via the function
`zl-list-insert'. Optional ARGS can be provided, to control the
behavior of `zl-list-insert'."
  (apply #'zl-list-insert keyword 'zb-keyword-list args)
  (when (equal type 'flag)
    (apply #'zl-list-insert keyword 'zb-flag-list args))
  (setq zb-keyword-type-plist
	(plist-put zb-keyword-type-plist keyword type)
	zb-keyword-func-plist
	(plist-put zb-keyword-func-plist keyword func))
  keyword)

(defun zb-keyword-p (sexp)
  "Determine if SEXP is a valid keyword.

If it is, return its type. Otherwise, return nil.

Additionally, if SEXP is '--eol--', which is used by zb-parse to
indicate the end of the parsed list, the function will return
'--eol--'."
  (cond
   ((equal sexp '--eol--) '--eol--)
   ((and (symbolp sexp)
	 (equal (substring (symbol-name sexp) 0 1) ":")
	 (member sexp zb-keyword-list))
    (plist-get zb-keyword-type-plist sexp))
   (t nil)))


;; Debugging utilities.

(defun zb-warn (name level message &rest args)
  "Display a zy-block warning message.

NAME is the name of the current zy-block name.

All other arguments correspond to those of `lwarn'."
  (apply #'lwarn
	 (format "zb %s" name)
	 level
	 message
	 args))

(defun zb-report-ex (name level ex)
  "Report an error cons cell EX.

NAME is the current zy-block name. LEVEL is the error level.

EX is (ERROR-SYMBOL . SIGNAL-DATA) representing an error."
  (let* ((ex-str (substring (pp-to-string ex) 0 -1)))
    (lwarn (format "zb %s" name) level ex-str)))


;; Keyword parsing and form formatting.

(defun zb-parse (name body)
  "Parse BODY, return two plists (KPLIST FPLIST).

The first plist KPLIST is of non-flag keywords and their
arguments. If there are objects that do not belong to any
keyword, they will be stored with the keyword 'nil'.

The second plist FPLIST is of flag keywords and their arguments.

NAME is just used for proper warning display."
  (let* (kplist				; plist of non-flags
	 fplist				; plist of flags
	 (body
	  (append body '(--eol--)))	; append '--eol--' to body
	 curkey				; current keyword
	 curkwp				; (zb-keyword-p curkey)
	 curarg				; current argument
	 newobj				; new object to parse
	 newkwp				; (zb-keyword-p newobj)
	 (kwcnt 0))			; keyword counter
    (while body
      ;; Get a new sexp to parse.
      (setq newobj (car body)
	    newkwp (zb-keyword-p newobj)
	    body (cdr body))
      ;; If `newobj' is a sub-block, format it.
      (when (and (consp newobj)
		 (zb-keyword-p (car newobj)))
	(setq newobj (zb-format name newobj 'noflags))
	(setq newobj
	      (cond
	       ((symbolp newobj) newobj)
	       ((cdr newobj) (append '(progn) newobj))
	       (t (car newobj)))))
      ;; Parsing argument based on the current keyword.
      (cond
       ;; If there is no active keyword, but the new object is still
       ;; not a keyword, collect the object as a free sexp.
       ((and (not curkwp) (not newkwp))
	(push newobj curarg))
       ;; If there is no active keyword, and the new object is a valid
       ;; keyword, store all free sexp.
       ((and (not curkwp) newkwp)
	(when curarg
	  (push (nreverse curarg) kplist)
	  (push nil kplist)))
       ;; If there is an active 'multiple' typed keyword, and the new
       ;; object is not a keyword, just collect the new object as ar
       ;; part of its argument.
       ((and (equal curkwp 'multiple) (not newkwp))
	(push newobj curarg))
       ;; If there is an active 'multiple' typed keyword, and the new
       ;; object is an valid keyword, stop the parsing of the current
       ;; keyword.
       ((and (equal curkwp 'multiple) newkwp)
	(push (nreverse curarg) kplist)
	(push curkey kplist))
       ;; If there is an active 'single' typed keyword, and the new
       ;; object is not a keyword, store the object as its argument.
       ((and (equal curkwp 'single) (not newkwp))
	(push newobj kplist)
	(push curkey kplist))
       ;; If there is an active 'single' typed keyword, and the new
       ;; object is an valid keyword, ignore the current keyword, and
       ;; issue an warning, as no argument has been passed to the
       ;; current keyword.
       ((and (equal curkwp 'single) newkwp)
	(zb-warn name :warning
		 "%dth keyword %s ignored."
		 kwcnt curkey))
       ;; If there is an active 'flag' typed keyword, and the new
       ;; object is not a keyword, store the object as its argument.
       ((and (equal curkwp 'flag) (not newkwp))
	(push newobj fplist)
	(push curkey fplist))
       ;; If there is an active 'flag' typed keyword, and the new
       ;; object is a valid keyword, store the flag as 't'.
       ((and (equal curkwp 'flag) newkwp)
	(push t fplist)
	(push curkey fplist))
       ;; For other circumstances, only issue an warning.
       (t
	(zb-warn name :warning
		 "unknown circumstance met at %dth keyword %s."
		 kwcnt curkey)))
      ;; Change the parsing state.
      (if newkwp
	  (setq curkey newobj
		curkwp newkwp
		curarg nil)
	(when (or (equal curkwp 'single)
		  (equal curkwp 'flag))
	  (setq curkey nil
		curkwp nil
		curarg nil)))
      ;; Increment keyword counter if necessary.
      (when newkwp
	(setq kwcnt (+ kwcnt 1))))
    ;; Return the two parsed plists.
    `(,kplist ,fplist)))

(defun zb-format (name body &optional noflags)
  "Return a formatted form  of BODY.

BODY will firstly be parsed with `zb-parse', then formatted with
the keyword functions. NAME is just used for warning issuing.

If NOFLAGS is non-nil, ignore flag keywords."
  (let* ((parse-result (zb-parse name body))
	 (kplist (car parse-result))
	 (fplist (cadr parse-result))
	 keyword
	 func
	 arg
	 body)
    ;; Apply non-flag keyword functions.
    (while kplist
      (setq keyword (car kplist)
	    arg (cadr kplist)
	    kplist (cddr kplist)
	    body
	    (if keyword
		(funcall (plist-get zb-keyword-func-plist
				    keyword)
			 name arg body)
	      (append arg body))))
    (unless noflags
      ;; Add global flag keywords.
      (dolist (fd zb-global-flag-alist)
	(let ((flag (car fd))
	      (default (cdr fd)))
	  (when (not (member flag fplist))
	    (push default fplist)
	    (push flag fplist))))
      ;; Apply flag keywords in order.
      (dolist (flag zb-flag-list)
	(when (member flag fplist)
	  (setq arg (plist-get fplist flag)
		body
		(funcall (plist-get zb-keyword-func-plist
				    flag)
			 name arg body)))))
    body))


;; The `zb' macro.

(defmacro zb (name &rest body)
  "Define a block of code as a zy-block."
  (declare (indent 1) (doc-string 2))
  ;; Format body.
  (setq body (zb-format name body))
  ;; Wrap body into `prog1' with `name'.
  (cond
   ((symbolp body)
    `(prog1 ',name ,body))
   ((not (cdr body))
    `(prog1 ',name ,(car body)))
   (t
    (append `(prog1 ',name) body))))


;; Keyword :protect.

(defun zb-wrapper-protect (name arg body)
  "Wrap BODY with protecting code if ARG is non-nil.

The code prevent any warning or error from stopping the whole
configuration from executing. Warnings or errors issued inside
BODY will be reported by the zy-block."
  (if arg
      `((condition-case --ex--
	    ,(if (cdr body) `(progn ,@body) (car body))
	  ('warning
	   (zb-report-ex ',name :warning --ex--))
	  ('error
	   (zb-report-ex ',name :error --ex--))))
    body))


;; Keyword :provide.

(defun zb-wrapper-provide (name arg body)
  "Provide feature ARG after BODY.

If ARG is t, provide feature NAME."
  (if arg
      (if (equal arg t)
	  (append body `((provide ',name)))
	(append body `((provide ,arg))))
    body))


;; Keyword :when and :unless.

(defun zb-wrapper-when (name arg body)
  "Execute BODY when ARG is non-nil."
  `((when ,arg ,@body)))

(defun zb-wrapper-unless (name arg body)
  "Execute BODY when ARG is nil."
  `((unless ,arg ,@body)))


;; Keyword :after-load.

(defun zb-wrapper-after-load (name arg body)
  "Execute BODY after ARG is loaded.

ARG is a feature name, or a list of feature names."
  ;; Reorganize lists.
  (when (and (consp arg)
	     (equal (car arg) 'quote)
	     (consp (cadr arg)))
    (setq arg (nreverse
	       (mapcar
		(lambda (elt) `(quote ,elt))
		(cadr arg)))))
  ;; Wrap body.
  (if (and (consp arg)
	   (not (equal (car arg) 'quote)))
      (dolist (f arg body)
	(setq body `((eval-after-load ,f (lambda () ,@body)))))
    `((eval-after-load ,arg (lambda () ,@body)))))


;; Keyword :idle.

(defun zb-wrapper-idle (name arg body)
  "Execute BODY after being idle for ARG seconds."
  `((run-with-idle-timer ,arg nil (lambda () ,@body))))


;; Keyword :hook-into

(defun zb-wrapper-hook-into (name arg body)
  "Add BODY to hook or hooks ARG.

BODY will be wraped in a lambda function before added to ARG."
  ;; Remove quote in arg.
  (when (equal (car arg) 'quote)
    (setq arg (cadr arg)))
  ;; Wrap body.
  (if (symbolp arg)
      `((add-hook ',arg (lambda () ,@body)))
    `((dolist (hook ',arg)
	(add-hook hook (lambda () ,@body))))))


;; Keyword :enable

(defun zb-wrapper-enable (name arg body)
  "Enable BODY if ARG is non-nil."
  (if arg
      body
    nil))


;; Keyword :sub-block

(defun zb-wrapper-sub-block (name arg body)
  "Return ARG and BODY.

This keyword is just a indicator of a sub-block."
  (append arg body))


;; Setup keywords.

;; Setup flag keywords in order: the early a flag keyword is
;; defined, the outer it will be wrapped.

(zb-define-keyword ':enable 'flag #'zb-wrapper-enable)
(zb-define-keyword ':protect 'flag #'zb-wrapper-protect)
(zb-define-keyword ':provide 'flag #'zb-wrapper-provide)

;; Setup non-flag keywords. The order is not important here.

(zb-define-keyword ':when 'single #'zb-wrapper-when)
(zb-define-keyword ':unless 'single #'zb-wrapper-unless)
(zb-define-keyword ':after-load 'single #'zb-wrapper-after-load)
(zb-define-keyword ':idle 'single #'zb-wrapper-idle)
(zb-define-keyword ':hook-into 'single #'zb-wrapper-hook-into)
(zb-define-keyword ':sub-block 'multiple #'zb-wrapper-sub-block)

;; Setup zy-benchmark.el.

(load "zy-benchmark" nil t)


(provide 'zy-block)

;;; end of zy-block.el
