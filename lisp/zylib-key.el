;;; zylib-key.el --- Keybinding utility. -*- lexical-binding: t -*-
;;; Commentary:

;; This file defines the keybinding framework of the configuration. It provides
;; two simple macros, `keybind!' and `defprefix!', for an efficient and
;; consistent keybinding interface and automatic loading management.

;;; Code:

(require 'zylib-pkg)

(defun zy--normalize-bindings (bindings)
  "Normalize BINDINGS as proper KEY and DEF pairs.

BINDINGS come in pairs with KEY and DEF, which under most cases
are the same as in `define-key', except that if KEY is a string,
it is wrapped with `kbd' (therefore normalized) before being used
as a KEY.

There can also be KEYWORD and FORM pairs occurring after the KEY
and DEF pair, where KEYWORD is a Lisp symbol whose name starts
with a colon (`:'), and FORM is an arbitrary Lisp form. They will
be regarded as a property list PLIST for the KEY and DEF pair.

In conclusion, BINDINGS is a list like this:

  ([KEY DEF [[KEYWORD FORM] ...]] ...)

Return a normalized list of keybinding descriptors DESCS, where
each element has the form of (KEY DEF . PLIST)."
  (let* (descs cur-key cur-def cur-rev-plist arg1 arg2)
    (cl-labels ((collect-desc ()
                  (when cur-key
                    (push (append (list cur-key cur-def)
                                  (reverse cur-rev-plist))
                          descs))
                  (setq cur-key nil
                        cur-def nil
                        cur-rev-plist nil)))
      (while bindings
        (setq arg1 (pop bindings)
              arg2 (when bindings (pop bindings)))
        (if (keywordp arg1)
            ;; Update current (reversed) plist.
            (progn (push arg1 cur-rev-plist)
                   (push arg2 cur-rev-plist))
          ;; Collect the last descriptor and prepare for the next one.
          (collect-desc)
          (setq cur-key (if (stringp arg1) `(kbd ,arg1) arg1)
                cur-def arg2)))
      ;; Collect the last descriptor.
      (collect-desc)
      ;; Return the descriptors in correct order.
      (reverse descs))))

(defun zy--after-keymap-form (keymap body)
  "Return a form where BODY if run after KEYMAP.
BODY is a list of forms. If KEYMAP is already a valid keymap, run
BODY now. Otherwise, check whether KEYMAP is availableis every
time a file is loaded."
  (if (equal keymap '(quote global))
      (macroexp-progn body)
    (let ((fn-name (cl-gensym "after-keymap-")))
      `(if (and (boundp ',keymap) (keymapp ,keymap))
           ,(macroexp-progn body)
         (add-hook 'after-load-functions
                   (defun ,fn-name (&rest _)
                     (when (and (boundp ',keymap) (keymapp ,keymap))
                       (remove-hook 'after-load-functions ',fn-name)
                       ,@body)))))))

(defun zy--keybind-form (keymap descs)
  "Return a single keybinding form.
KEYMAP is like that used by `evil-define-key*', and DESCS is the
list of keybinding descriptors returned by
`zy--normalize-bindings'."
  (let* ((keymap (if (equal keymap '(quote global))
                     'global-map
                   keymap))
         (forms (seq-map (lambda (desc)
                           `(define-key ,keymap
                                        ,(car desc)
                                        ,(cadr desc)))
                         descs)))
    (zy--after-keymap-form keymap forms)))

(defun zy--evil-keybind-form (state keymap descs)
  "Get a single Evil keybinding form.
STATE and KEYMAP are like those used by `evil-define-key', and
DESCS is the list of keybinding descriptors returned by
`zy--normalize-bindings'."
  (let* ((bindings (mapcan (lambda (desc)
                             (list (car desc) (cadr desc)))
                           descs))
         (key (pop bindings))
         (def (pop bindings)))
    `(when (modulep! '+evil)
       (after! 'evil
         (when (fboundp 'evil-define-key)
           (evil-define-key ,state ,keymap ,key ,def ,@bindings))))))

(defmacro keybind! (state keymap &rest bindings)
  "Create some keybindings according to BINDINGS.

STATE is one of the Evil states, or a list of one or more of
them. When STATE is omitted by using nil, create standard Emacs
keybindings using ‘define-key’；otherwise create Evil keybindings
using `evil-define-key*'.

If the `+evil' module is disabled altogether, a `keybind!' form
with a non-nil STATE argument does not have any effect.

KEYMAP is either an unquoted symbol representing a keymap, or one
of the following quoted symbols:

  `global' the global keymap `global-map', or the state-specific
           global map.

If KEYMAP is not loaded yet, the keys will not be binded until
KEYMAP is ready.

BINDINGS comes in pairs with KEY and DEF, where KEY and DEF are
like those in `define-key', except that if KEY is a string, it is
always wrapped in `kbd' before being used."
  (declare (indent 2))
  (let* ((descs (zy--normalize-bindings bindings)))
    (if state
        (zy--evil-keybind-form state keymap descs)
      (zy--keybind-form keymap descs))))

(defmacro defprefix! (command name state keymap key &rest bindings)
  "Define COMMAND as a prefix command. COMMAND should be a symbol.

A new sparse keymap is stored as COMMAND's function definition
and its value. NAME is the menu name string for the map.

Bind this prefix command to KEY in STATE and KEYMAP as per
`keybind!'. If KEY is a string, it will be wrapped in `kbd'
before being used to bind the key. If KEYMAP is a unquoted list,
bind this prefix command to KEY in each KEYMAP.

You may continue to define keybindings using KEY and DEF pairs as
per `keybind!' in BINDINGS."
  (declare (indent 5))
  (let* ((keybind-form-upper
          (if (or (symbolp keymap)
                  (eq (car-safe keymap) 'quote))
              `(keybind! ,state ,keymap ,key '(,name . ,command))
            (macroexp-progn
             (seq-map (lambda (x)
                        `(keybind! ,state ,x ,key '(,name . ,command)))
                      keymap))))
         (form `(prog1
                    (define-prefix-command ',command)
                  (defvar ,command)
                  ,keybind-form-upper))
         (keybind-form-lower (when bindings
                               `(keybind! nil ,command ,@bindings))))
    (append form `(,keybind-form-lower))))

(define-minor-mode zy-leader-mode
  "Global minor mode for enabling leader keybindings."
  :global t
  :interactive nil
  :group 'zyemacs)

(defvar zy-leader-mode-map (make-sparse-keymap)
  "High precedence keymap for leader keybindings.")

(add-to-list 'emulation-mode-map-alists
             `((zy-leader-mode . ,zy-leader-mode-map)))

;; The *Messages* buffer is strange. It requires additional configuration for
;; the leader key to work.
(add-hook! 'window-setup-hook
  (when-let ((messages-buffer (get-buffer "*Messages*")))
    (with-current-buffer messages-buffer
      (when (fboundp 'evil-normalize-keymaps)
        (evil-normalize-keymaps)))))

(defun set-leader! (state key &optional localleader)
  "Set KEY to trigger leader bindings in STATE.
KEY should be in the form produced by `kbd'. STATE is one of the
Evil states, a list of one or more of them, or nil, which means
all states. If LOCALLEADER is non-nil, set the local leader
instead.

This is almost identical to `evil-set-leader' from the package
Evil, except that when Evil is absent, this still works if STATE
is nil."
  (let* ((leaderkey (if localleader [localleader] [leader]))
         (binding
          `(menu-item "" nil :filter ,(lambda (_) (key-binding leaderkey)))))
    (if (modulep! '+evil)
        ;; When the module `+evil' is available, bind according to `state'.
        (let* ((all-states
                '(normal insert visual replace operator motion emacs))
               (states (cond ((null state) all-states)
                            ((consp state) state)
                            (t (list state)))))
          (with-eval-after-load 'evil
            (when (fboundp 'evil-define-key*)
              (dolist (state states)
                (evil-define-key* state zy-leader-mode-map key binding)))))
      ;; Otherwise, bind globally if it is not state-specific.
      (unless state
        (define-key zy-leader-mode-map key binding)))
    ;; Enable the minor mode if not yet.
    (unless zy-leader-mode
      (zy-leader-mode 1))))

(defun zy--other-window-prefix ()
  "Display the buffer of the next command in a new window.
This is like `other-window-prefix', but it echoes nothing, and
returns a function used to undo the prefix."
  (display-buffer-override-next-command
   (lambda (buffer alist)
     (let ((alist (append '((inhibit-same-window . t)) alist))
           window type)
       (if (setq window (display-buffer-pop-up-window buffer alist))
           (setq type 'window)
         (setq window (display-buffer-use-some-window buffer alist)
               type 'reuse))
       (cons window type)))))

(defun zy--other-frame-prefix ()
  "Display the buffer of the next command in a new frame.
This is like `other-frame-prefix', but it echoes nothing, and
returns a function used to undo the prefix."
  (display-buffer-override-next-command
   (lambda (buffer alist)
     (cons (display-buffer-pop-up-frame
            buffer (append '((inhibit-same-window . t))
                           alist))
           'frame))))

(defun zy--other-tab-prefix ()
  "Display the buffer of the next command in a new tab.
This is like `other-tab-prefix', but it echoes nothing, and
returns a function used to undo the prefix."
  (display-buffer-override-next-command
   (lambda (buffer alist)
     (cons (progn
             (display-buffer-in-tab
              buffer (append alist '((inhibit-same-window . nil))))
             (selected-window))
           'tab))))

(defmacro other-placed! (place string keymap)
  "Get the \"other place\" version of KEYMAP.
The \"other place\" version of KEYMAP is an extended menu item
definition which can be used in `define-key'. It behaves like
KEYMAP, but displays the buffer of the subsequent command in
another place, in accordance with PLACE, which have the following
possible values:

`window' a new window
`frame'  a new frame
`tab'    a new tab

STRING is used as the menu item name for the definition."
  (let ((fn (intern (format "%s-other-%s-%s" keymap place (cl-gensym)))))
    `(progn
       ;; This function acts as the `:filter' property of the extended menu
       ;; item definition. It is called every time the menu item is accessed
       ;; to dynamically compute the keymap for the menu item. What the
       ;; function does is: (a) Call `zy--other-window-prefix' so that the
       ;; next command displays its buffer in another window; (b) Compose an
       ;; `aux-map' as the backup map; (c) Return the (STRING . DEFN) pair
       ;; used by `define-key', where STRING is the menu item name extracted
       ;; from `keymap', and `defn' is the keymap composed from `keymap' and
       ;; `aux-map'.
       ;;
       ;; Using `aux-map' as a backup keymap here ensures that if a key not in
       ;; `keymap' is hit, the effect of `zy--other-window-prefix' is
       ;; cancelled, and an "X is undefined" message is displayed.
       (defun ,fn (_cmd)
         ,(format "Trigger `%s' in another window." keymap)
         (let* ((exitfun (,(pcase (unquote! place)
                             ('window 'zy--other-window-prefix)
                             ('frame 'zy--other-frame-prefix)
                             ('tab 'zy--other-tab-prefix)
                             (_ (error "%s is not a valid place" place)))))
                (aux-map (make-sparse-keymap)))
           (define-key aux-map [t]
                       (lambda ()
                         (interactive)
                         (funcall exitfun)
                         (funcall 'undefined)))
           (cons ,string (make-composed-keymap ,keymap aux-map))))
       ;; Now compose and return the extended menu item definition.
       '(menu-item "" nil :filter ,fn))))

(defmacro other-windowed! (string keymap)
  "Get the \"other window\" version of KEYMAP.
The \"other window\" version of KEYMAP is an extended menu item
definition which can be used in `define-key'. It behaves like
KEYMAP, but displays the buffer of the sebsequent command in
another window.

STRING is used as the menu item for the definition."
  `(other-placed! 'window ,string ,keymap))

(defmacro other-framed! (string keymap)
  "Get the \"other frame\" version of KEYMAP.
The \"other frame\" version of KEYMAP is an extended menu item
definition which can be used in `define-key'. It behaves like
KEYMAP, but displays the buffer of the sebsequent command in
another frame.

STRING is used as the menu item for the definition."
  `(other-placed! 'frame ,string ,keymap))

(defmacro other-tabbed! (string keymap)
  "Get the \"other tab\" version of KEYMAP.
The \"other tab\" version of KEYMAP is an extended menu item
definition which can be used in `define-key'. It behaves like
KEYMAP, but displays the buffer of the sebsequent command in
another tab.

STRING is used as the menu item for the definition."
  `(other-placed! 'tab ,string ,keymap))

(provide 'zylib-key)

;;; zylib-key.el ends here
