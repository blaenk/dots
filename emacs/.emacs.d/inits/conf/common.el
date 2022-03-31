(require 'straight)
(require 'use-package)
(require 'general)

(defconst my--dots-path (getenv "DOTSPATH")
  "The DOTSPATH environment variable.

This is read once when initializing to avoid the cost of calling
`getenv' multiple times. It also allows for a consistent value of
it throughout the lifetime of the emacs session.

In the unlikely event that the value is changed, simply restart
emacs.")

(defconst my--is-within-vm (getenv "VM")
  "Whether or not we're within a virtual machine.

I set this environment variable within ~/.zsh.local to represent
that we are withing a virtual machine.")

(function-put #'general-define-key 'lisp-indent-function 'defun)
(function-put #'general-create-definer 'lisp-indent-function 'defun)

(defalias 'bind #'general-define-key)
(function-put #'bind 'lisp-indent-function 'defun)

(general-create-definer bind-local :keymaps 'local)
(function-put #'bind-local 'lisp-indent-function 'defun)

;; On gnome, have to unbind M-SPC, known as Alt+Space in
;; settings → windows → activate the window menu.
(general-define-key
  :states '(emacs normal visual insert)
  :prefix-map 'my-prefix-map
  :global-prefix "C-c"
  :non-normal-prefix "M-SPC"
  :prefix "SPC")

(general-create-definer my-map
  :keymaps 'my-prefix-map)

(function-put #'my-map 'lisp-indent-function 'defun)

;; Alias :general-config to :general
(defalias 'use-package-normalize/:general-config #'use-package-normalize/:general)
(defalias 'use-package-handler/:general-config #'use-package-handler/:general)

;; Add :general-config to `use-package-keywords` after :config, so that it processes
;; after the package has loaded.
(setq use-package-keywords
  (-insert-at (+ 1 (-elem-index :config use-package-keywords))
    :general-config use-package-keywords))

(defun my-emacs-dir (&optional path)
  "Return a path relative to the emacs directory."

  (expand-file-name (or path "") user-emacs-directory))

(defun my-cache-dir (&optional path)
  "Return a path relative to the cache directory."

  (my-emacs-dir (concat "cache/" path)))

(defun my-inits-dir (&optional path)
  "Return a path relative to the inits directory."

  (my-emacs-dir (concat "inits/" path)))

(defun my-is-fullscreen-p ()
  (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth)))

(defun my-turn-on-fullscreen ()
  "Unconditionally enable fullscreen on this frame."
  (interactive)

  (set-frame-parameter nil 'fullscreen 'fullboth))

(defun my-turn-off-fullscreen ()
  "Unconditionally disable fullscreen on this frame."
  (interactive)

  (set-frame-parameter nil 'fullscreen nil))

(defmacro my-after-frame (&rest body)
  `(if (daemonp)
       (add-hook 'after-make-frame-functions
                 (lambda (frame)
                   (with-selected-frame frame
                     ,@body)))
     ;; can get current frame with
     ;; (window-frame (get-buffer-window))
     (progn ,@body)))

(defun my-force-eval-buffer ()
  "Execute the current buffer as Lisp code.
Top-level forms are evaluated with `eval-defun' so that `defvar'
and `defcustom' forms reset their default values."
  (interactive)

  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-sexp)
      (eval-defun nil))))

(defun my-define-repeatable-command (alist &optional exit-func)
  "Return a lambda that calls the first function of ALIST.
It sets the transient map to all functions of ALIST,
allowing you to repeat those functions as needed."
  (lexical-let ((keymap (make-sparse-keymap))
                (exit-func exit-func))
    (mapc (lambda (x)
            (when x
              (define-key keymap (kbd (car x)) (cdr x))))
          alist)
    (lambda (arg)
      (interactive "p")
      (fset exit-func (set-transient-map keymap t)))))

(defmacro csetq (variable value)
  "Uses variable's custom-set property if it has one.

Otherwise it uses `set-default'."
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

(defmacro my-setq (&rest args)
  "setq variant that warns if the variable has a :custom-set property.

It takes arguments in the same manner as `setq'. It goes through
each variable to see if it has a :custom-set property and if so
it warns about it, then it passes the arguments to `setq'
verbatim."
  (if (= (% (length args) 2) 1)
      (error "Wrong number of arguments, %s" (length args))
    `(let ((pairs (-partition 2 ',args)))
       (dolist (pair pairs)
         (-let (((name value) pair))
           (when (get name 'custom-set)
             (warn "Variable `%s' has a :custom-set property!" name))
           )
         )
       (setq ,@args))))

(defmacro my-with-atom-one-dark-colors (&rest body)
  `(eval-after-load 'atom-one-dark-theme
     (lambda ()
       (eval-when-compile
         (require 'atom-one-dark-theme))

       (atom-one-dark-with-color-variables ,@body))))

(defmacro my-with-last-used-window (&rest body)
  "Perform BODY within the context of the last used window."
  `(let ((win (get-mru-window t t t)))
     (unless win (error "Last window not found."))
     (let ((frame (window-frame win)))
       (with-selected-frame frame
         (with-selected-window win ,@body)))))

(defmacro my-advise-to-insert-after (fn)
  "Advice the FN to enter Evil insert state after it executes."
  `(define-advice ,fn
       (:after (&rest args) enter-insert-state)
     ,(format "Call `%s', then enter Evil insert state." (symbol-name fn))
     (evil-insert-state)))

(defmacro my-create-evil-toggle-for-mode (mode)
  "Defines an evil toggle function and attaches it to the mode's hook.

Defines a function which enters Emacs state when the given MODE is entered,
then enters normal state when the MODE is exited.

This function is then added to MODE hook."
  (let*
      ((name (symbol-name mode))
       (hook-name (intern (concat name "-hook")))
       (func (intern (concat "my--evil-toggle-for-" name))))
    `(progn
       (defun ,func ()
         (if ,mode (evil-emacs-state) (evil-normal-state)))

       (add-hook ',hook-name ',func))))

(defun my--exit-insert-state (arg)
  (when (and (fboundp 'evil-insert-state-p)
             (evil-insert-state-p))
    (evil-normal-state)))

(defun my--use-node-modules-binary (name &optional directory)
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (directory (or directory name))
         (binary (and root
                      (expand-file-name (concat "node_modules/" name "/bin/" name ".js")
                                        root))))
    (when (and binary (file-executable-p binary))
      binary)))

(defun my--set-char-widths (alist)
  (while (char-table-parent char-width-table)
    (setq char-width-table (char-table-parent char-width-table)))
  (dolist (pair alist)
    (let ((width (car pair))
          (chars (cdr pair))
          (table (make-char-table nil)))
      (dolist (char chars)
        (set-char-table-range table char width))
      (optimize-char-table table)
      (set-char-table-parent table char-width-table)
      (setq char-width-table table))))

(provide 'conf/common)
