;;; zy-lisp-utils.el --- personal lisp utilities of Zyxir.

(defun zl-list-insert (newelt list &rest args)
  "Insert NEWELT into LIST according to ARGS.

LIST is a symbol representing a cons cell.

If ARGS is nil, omitted or ':prepend', prepend NEWELT to LIST.

If ARGS is ':append', append NEWELT to LIST.

If ARGS is ':before ELT', insert NEWELT into the position before
the first occurence of ELT (comparisons are made with
`equal'). If ELT does not exist, NEWLIST will be appended.

If ARGS is ':after ELT', insert NEWELT into the position after
the first occurence of ELT (comparisons are made with
`equal'). If ELT does not exist, NEWLIST will be appended.

Otherwise, do nothing.

Return the new cons cell represented by LIST."
  (let ((list-var (symbol-value list)))
    (unless (listp list-var)
      (signal 'wrong-type-argument `(listp ,list-var)))
    (cond
     ;; If ARGS is nil, omitted or ':prepend'.
     ((or (not args)
	  (equal args '(:prepend)))
      (set list `(,newelt . ,list-var)))
     ;; If ARGS is ':append'.
     ((equal args '(:append))
      (set list (append list-var `(,newelt))))
     ;; If ARGS is ':before ELT'.
     ((and (equal (car args) ':before)
	   (cdr args))
      (if (or (not list-var)
	      (equal (car list-var) (cadr args)))
	  (set list `(,newelt . ,list-var))
	(let ((tail list-var)
	      (elt (cadr args)))
	  (while (and (cdr tail)
		      (not (equal (cadr tail) elt)))
	    (setq tail (cdr tail)))
	  (setcdr tail `(,newelt . ,(cdr tail))))
	(symbol-value list)))
     ;; If ARGS is ':after ELT'.
     ((and (equal (car args) ':after)
	   (cdr args))
      (if (not list-var)
	  (set list `(,newelt))
	(let ((tail list-var)
	      (elt (cadr args)))
	  (while (and tail
		      (not (equal (car tail) elt)))
	    (setq tail (cdr tail)))
	  (if (not tail)
	      (set list (append list-var `(,newelt)))
	    (setcdr tail `(,newelt . ,(cdr tail)))
	    (symbol-value list)))))
     ;; Otherwise, do nothing.
     (t list-var))))


(provide 'zy-lisp-utils)

;;; end of zy-lisp-utils.el
