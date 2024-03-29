(setq gc-cons-threshold most-positive-fixnum)

;; reset gc-cons-threshold
;; idle timer suggested by vermiculus
(run-with-idle-timer
  10 nil
  (lambda ()
    ;; (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))
    ;; https://github.com/emacs-lsp/lsp-mode#performance
    ;; TODO try out different values
    (setq gc-cons-threshold 100000000)
    (message "gc-cons-threshold restored to %S" gc-cons-threshold)))

;; frame
(defun my--set-frame-options (frame)
  "Set the options for the current frame."

  (cond
   ((eq system-type 'gnu/linux)
    (set-frame-font "DejaVu Sans Mono-10.5" nil t)

    (set-fontset-font "fontset-default" nil
                      (font-spec :name "Symbola") nil 'prepend))

   ((eq system-type 'windows-nt)
    (set-frame-font "Consolas-10.5" nil t))

   ((eq system-type 'darwin)
    (set-frame-font "Monaco-12" nil t)

    (set-fontset-font t 'symbol
                      (font-spec :family "Apple Color Emoji") nil 'prepend)
    (set-fontset-font t 'symbol
                      (font-spec :family "Apple Symbols") nil 'prepend)

    ;; Make the titlebar transparent, effectively making it the same color as
    ;; the background.
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . dark)))))

(my--set-frame-options nil)

(add-hook 'after-make-frame-functions #'my--set-frame-options)

(defvar bootstrap-version)
(defvar straight-repository-branch "develop")
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  ; uncomment to install
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defun my-patch-package-find-file-visit-truename (oldfun &rest r)
  (let ((find-file-visit-truename nil))
    (apply oldfun r)))

(advice-add #'straight--build-autoloads :around
            #'my-patch-package-find-file-visit-truename)

(defun my-reload-init ()
  "Reload init.el."
  (interactive)
  (straight-transaction
    (straight-mark-transaction-as-init)
    (message "Reloading init.el...")
    (load user-init-file nil 'nomessage)
    (message "Reloading init.el... done.")))

(straight-use-package 'use-package)

(setq backup-by-copying t

      straight-use-package-by-default t
      straight-cache-autoloads t
      use-package-check-before-init t
      use-package-always-defer t
      )

(eval-when-compile
  (require 'use-package))

(customize-set-variable 'use-package-enable-imenu-support t)

(use-package benchmark-init
  :defer t
  :straight (:host github :repo "404cn/benchmark-init-el")

  :init
  (defvar my--is-benchmarking-p
    (prog1
        (member "--benchmark" command-line-args)
      (setq command-line-args (delete "--benchmark" command-line-args)))
    "Whether emacs should run benchmark-init"))

(eval-and-compile (setq general-use-package-emit-autoloads nil))

; NOTE: For some reason, having :general maps increases the time it takes to load evil
(use-package general :demand t)
(use-package dash :demand t)
(use-package f :demand t)
(use-package s :demand t)

(add-to-list 'load-path (expand-file-name "inits/" user-emacs-directory) t)

(require 'conf/common)

(add-to-list 'custom-theme-load-path (my-inits-dir "conf/"))

(let* ((auto-save-dir (my-cache-dir "autosaves/")))
  (setq auto-save-list-file-prefix (expand-file-name "saves-" auto-save-dir)
        auto-save-file-name-transforms `((".*" ,auto-save-dir t))))

(define-advice emacs-session-filename
    (:override (session-id) store-in-cache-dir)
  "Store the session files in the cache directory."

  (my-cache-dir (concat "sessions/" session-id)))

(when my--is-within-vm
  (setq browse-url-browser-function #'kill-new))

(setq delete-old-versions t
      version-control t

      auth-sources '("~/.authinfo.gpg")

      backup-directory-alist `((".*" . ,(my-cache-dir "backups/")))

      use-dialog-box nil

      enable-recursive-minibuffers t

      inhibit-x-resources t
      x-underline-at-descent-line t

      save-interprogram-paste-before-kill t
      select-enable-clipboard t
      select-enable-primary t

      inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t

      initial-scratch-message nil

      mouse-wheel-follow-mouse t
      mouse-wheel-progressive-speed t
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-yank-at-point t

      help-window-select t

      custom-file (my-cache-dir "custom.el")

      scroll-conservatively 101
      scroll-preserve-screen-position t
      scroll-step 1

      visible-bell t
      ring-bell-function #'ignore

      history-delete-duplicates t

      tab-always-indent nil
      standard-indent 2

      load-prefer-newer t
      sentence-end-double-space nil
      uniquify-buffer-name-style 'forward)

(setq-default fill-column 80
              indent-tabs-mode nil
              tab-width 2
              cursor-type 'box
              echo-keystrokes 0.1
              show-trailing-whitespace t
              indicate-empty-lines t)

(fset #'yes-or-no-p #'y-or-n-p)
(add-to-list 'auto-coding-alist '("\\.nfo\\'" . ibm437))

;; Don't let the cursor go into the minibuffer prompt
;; Source: https://lists.gnu.org/archive/html/emacs-devel/2016-04/msg00857.html
(let ((default (eval (car (get 'minibuffer-prompt-properties 'standard-value))))
      (dont-touch-prompt-prop '(cursor-intangible t)))
  (setq minibuffer-prompt-properties (append default dont-touch-prompt-prop))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(defun my-kill-this-buffer ()
  "Kill this buffer regardless of whether or not it's modified."
  (interactive)

  (let ((buffer-modified-p nil))
    (kill-buffer (current-buffer))))

(defun my-switch-to-previous-buffer ()
  "Switch to the previously used buffer."
  (interactive)

  (switch-to-buffer (other-buffer)))

(defun my-split-with-previous-buffer ()
  "Split this window and open the previous buffer in it."
  (interactive)

  (select-window (split-window-below))
  (my-switch-to-previous-buffer))

(defun my-pop-to-frame ()
  "Pop the current window and its buffer into a new frame."
  (interactive)

  (unless (one-window-p)
    (let ((buffer (current-buffer)))
      (delete-window)
      (select-window (display-buffer-pop-up-frame buffer nil)))))

(defun my-force-save ()
  "Force a save regardless of whether or not the buffer is modified.

This may be useful for situations in which other applications are
waiting to react to the save."
  (interactive)

  (set-buffer-modified-p t)
  (save-buffer))

(defun my-touch-buffer ()
  "Perform a 'touch' on the file backing the buffer."
  (interactive)

  (when buffer-file-name
    (shell-command (concat "touch " (shell-quote-argument (buffer-file-name))))))

(defun my-scroll-mru-window-up ()
  "Scroll up the last window."
  (interactive)

  (my-with-last-used-window (scroll-down)))

(defun my-scroll-mru-window-down ()
  "Scroll down the last window."
  (interactive)

  (my-with-last-used-window (scroll-up)))

(bind
  [remap eval-last-sexp] 'pp-eval-last-sexp
  [remap eval-expression] 'pp-eval-expression

  "C-z" nil

  "M-K" 'my-scroll-mru-window-up
  "M-J" 'my-scroll-mru-window-down

  "C-S-x C-S-s" 'my-force-save)

(defun my-backward-kill-line ()
  "Kill a line backwards."
  (interactive)

  (kill-line 0))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.

With argument ARG, do this that many times."
  (interactive "p")

  (delete-region (point) (progn (backward-word arg) (point))))

(bind :keymaps 'minibuffer-local-map
  "C-u" 'my-backward-kill-line
  "C-w" 'my-backward-delete-word)

(bind :keymaps 'minibuffer-inactive-mode-map
  [mouse-1] nil)

(defun my-switch-to-last-window ()
  "Focus the most recently used window."
  (interactive)

  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found."))
    (let ((frame (window-frame win)))
      (raise-frame frame)
      (select-frame frame)
      (select-window win))))

(defun my-copy-region-unindented (pad beginning end)
  "Copy the region, un-indented by the length of its minimum indent.

If numeric prefix argument PAD is supplied, indent the resulting
text by that amount."
  (interactive "P\nr")

  (let ((text (buffer-substring beginning end))
        (indent nil)
        (itm indent-tabs-mode)
        (tw tab-width)
        (st (syntax-table)))
    (with-temp-buffer
      (insert text)
      (set-syntax-table st)
      (setq indent-tabs-mode itm
            tab-width tw)
      ;; Establish the minimum level of indentation.
      (goto-char (point-min))
      (while (and (re-search-forward "^[[:space:]\n]*" nil :noerror)
                  (not (eobp)))
        (let ((length (current-column)))
          (when (or (not indent) (< length indent))
            (setq indent length)))
        (forward-line 1))
      (if (not indent)
          (error "Region is entirely whitespace")
        ;; Un-indent the buffer contents by the length of the minimum
        ;; indent level, and copy to the kill ring.
        (when pad
          (setq indent (- indent (prefix-numeric-value pad))))
        (indent-rigidly (point-min) (point-max) (- indent))
        (copy-region-as-kill (point-min) (point-max))))))

;; based on: https://emacs.stackexchange.com/a/19585/13444
(defun my-describe-char-at-mouse-click (click-event)
  "`describe-char' at CLICK-EVENT's position.
CLICK-EVENT should be a mouse-click event."
  (interactive "e")
  (run-hooks 'mouse-leave-buffer-hook)
  (let ((pos (cadr (event-start click-event))))
    (describe-char pos)))

(my-map
  "e" (cons "emacs" (make-sparse-keymap))

  "e f" (cons "find" (make-sparse-keymap))
  "e f f" 'find-function
  "e f l" 'find-library
  "e f v" 'find-variable
  "e f <down-mouse-1>" 'my-describe-char-at-mouse-click

  "e e" (cons "eval" (make-sparse-keymap))
  "e e b" 'eval-buffer
  "e e d" 'eval-defun
  "e e r" 'eval-region
  "e e e" 'pp-eval-expression
  "e e s" 'pp-eval-last-sexp
  "e e p" 'eval-print-last-sexp
  "e e M" 'pp-macroexpand-last-sexp

  "e h" (cons "help" help-map)

  "e q" 'save-buffers-kill-terminal
  "e c" 'save-buffers-kill-terminal

  "." (cons "dots" (make-sparse-keymap))

  "i" (cons "insert" (make-sparse-keymap))
  "s" (cons "search" (make-sparse-keymap))

  "o" (cons "open" (make-sparse-keymap))
  "o a" (cons "all" (make-sparse-keymap))

  "o o" (cons "other" (make-sparse-keymap))
  "o o f" 'other-frame
  "o o b" 'my-switch-to-previous-buffer
  "o o w" 'my-switch-to-last-window

  "t" (cons "toggle" (make-sparse-keymap))

  "m" (cons "mode" (make-sparse-keymap))

  ;; kill things
  "k" (cons "kill" (make-sparse-keymap))
  "k e" 'save-buffers-kill-terminal
  "k b" 'my-kill-this-buffer
  "k f" 'delete-frame

  "k o" (cons "other" (make-sparse-keymap))
  "k o f" 'delete-other-frames
  "k o w" 'delete-other-windows

  ;; frames
  "f" (cons "frame" (make-sparse-keymap))
  "f o" 'other-frame
  "f f" 'toggle-frame-fullscreen

  ;; buffers
  "b" (cons "buffer" (make-sparse-keymap))
  "b h" 'bury-buffer
  "b o" 'my-switch-to-previous-buffer
  "b s" 'save-buffer
  "b S" 'my-force-save
  "b t" 'my-touch-buffer
  )

(my-map :infix "w"
  :prefix-map 'my-window-prefix-map
  :prefix-command 'my-window-prefix-command

  "" (cons "window" (make-sparse-keymap))

  "o" 'my-switch-to-last-window
  "=" 'balance-windows
  "p" 'my-pop-to-frame
  "b" 'balance-windows

  "s" (cons "split" (make-sparse-keymap))
  "s p" 'my-split-with-previous-buffer
  )

(defvar-local my--display-column-number nil
  "Whether or not to display the column number in the mode-line

This is defined instead of using column-number-mode because
column-number-mode is global, and we want to control this on a
buffer-local basis.")

(when (>= emacs-major-version 26)
  (setq display-line-numbers-width 3))

(defun my-enable-line-numbers ()
  (if (< emacs-major-version 26)
      (linum-relative-mode)
    (setq-local display-line-numbers 'visual))

  (setq-local my--display-column-number t))

(add-hook 'prog-mode-hook #'my-enable-line-numbers)

(defun my-toggle-line-numbers ()
  "Toggle the visibility of line and column numbers."
  (interactive)

  (if (< emacs-major-version 26)
      (linum-relative-mode)
    (setq-local display-line-numbers (if display-line-numbers nil 'visual)))

  (setq-local my--display-column-number (not my--display-column-number)))

(my-map "t n" 'my-toggle-line-numbers)

(when my--is-benchmarking-p
  (benchmark-init/activate))

;; We use use-package instead of a plain `require' to allow us to use
;; use-package's profiling instrumentation if necessary, otherwise it seems like
;; the use-package forms within these required files don't show up.
(use-package conf/theme :straight nil :demand t)
(use-package conf/evil :straight nil :demand t)
(use-package conf/git :straight nil :demand t)
(use-package conf/built-in :straight nil :demand t)
(use-package conf/mode-line :straight nil :demand t)

(use-package conf/helm :straight nil :demand t)
(use-package conf/utilities :straight nil :demand t)
(use-package conf/company :straight nil :defer 5)

(load custom-file 'noerror)
