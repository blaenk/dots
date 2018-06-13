(require 'package)

;; frame
(defun my--set-frame-options (frame)
  "Set the options for the current frame."

  (add-to-list 'default-frame-alist '(width . 101))
  (add-to-list 'default-frame-alist '(height . 36))

  (cond
   ((eq system-type 'gnu/linux)
    (set-frame-font "DejaVu Sans Mono-10.5" nil t)

    (set-fontset-font "fontset-default" nil
                      (font-spec :name "Symbola") nil 'prepend))

   ((eq system-type 'windows-nt)
    (set-frame-font "Consolas-10.5" nil t))

   ((eq system-type 'darwin)
    (set-frame-font "Monaco-10.5" nil t)

    (set-fontset-font t 'symbol
                      (font-spec :family "Apple Color Emoji") nil 'prepend)
    (set-fontset-font t 'symbol
                      (font-spec :family "Apple Symbols") nil 'prepend))))

(my--set-frame-options nil)

(add-hook 'after-make-frame-functions #'my--set-frame-options)

;; packages
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(add-to-list 'package-pinned-packages '(sql-indent . "gnu"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq load-prefer-newer t
      backup-by-copying t

      use-package-always-ensure t
      use-package-check-before-init t
      use-package-enable-imenu-support t)

(eval-when-compile
  (require 'use-package))

(use-package benchmark-init
  :defer t

  :init
  (defvar my--is-benchmarking-p
    (prog1
        (member "--benchmark" command-line-args)
      (setq command-line-args (delete "--benchmark" command-line-args)))
    "Whether emacs should run benchmark-init"))

(use-package auto-compile
  :unless (eq system-type 'windows-nt)

  :defines
  auto-compile-display-buffer

  :init
  (setq auto-compile-display-buffer nil)

  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package dash
  :config
  (dash-enable-font-lock))

(use-package f)
(use-package s)

(use-package general)

(add-to-list 'load-path (expand-file-name "inits/" user-emacs-directory) t)
(byte-recompile-directory (expand-file-name "inits/" user-emacs-directory) 0)

(require 'conf/common)

(add-to-list 'custom-theme-load-path (my-inits-dir "conf/"))

(when (not (file-exists-p (my-cache-dir)))
  (make-directory (my-cache-dir)))

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

      backup-directory-alist `((".*" . ,(my-cache-dir "backups/")))

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
              echo-keystrokes 0.1)

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

(my-map
  "e" '(:ignore t :which-key "emacs")

  "e f" '(:ignore t :which-key "find")
  "e f f" 'find-function
  "e f l" 'find-library
  "e f v" 'find-variable

  "e e" '(:ignore t :which-key "eval")
  "e e b" 'eval-buffer
  "e e d" 'eval-defun
  "e e r" 'eval-region
  "e e e" 'pp-eval-expression
  "e e s" 'pp-eval-last-sexp
  "e e p" 'eval-print-last-sexp
  "e e M" 'pp-macroexpand-last-sexp

  "e h" '(:keymap help-map :which-key "help")

  "e q" 'save-buffers-kill-terminal
  "e c" 'save-buffers-kill-terminal

  "." '(:ignore t :which-key "dots")

  "i" '(:ignore t :which-key "insert")
  "s" '(:ignore t :which-key "search")

  "o" '(:ignore t :which-key "open")
  "o a" '(:ignore t :which-key "all")

  "o o" '(:ignore t :which-key "other")
  "o o f" 'other-frame
  "o o b" 'my-switch-to-previous-buffer
  "o o w" 'my-switch-to-last-window

  "t" '(:ignore t :which-key "toggle")

  "m" '(:ignore t :which-key "mode")

  ;; kill things
  "k" '(:ignore t :which-key "kill")
  "k e" 'save-buffers-kill-terminal
  "k b" 'my-kill-this-buffer
  "k f" 'delete-frame

  "k o" '(:ignore t :which-key "other")
  "k o f" 'delete-other-frames
  "k o w" 'delete-other-windows

  ;; frames
  "f" '(:ignore t :which-key "frame")
  "f o" 'other-frame
  "f f" 'toggle-frame-fullscreen

  ;; buffers
  "b" '(:ignore t :which-key "buffer")
  "b h" 'bury-buffer
  "b o" 'my-switch-to-previous-buffer
  "b s" 'save-buffer
  "b S" 'my-force-save
  "b t" 'my-touch-buffer
  "w" '(:ignore t :which-key "window")
  )

(my-map :infix "w"
  :prefix-map 'my-window-prefix-map
  :prefix-command 'my-window-prefix-command

  "o" 'my-switch-to-last-window
  "=" 'balance-windows
  "p" 'my-pop-to-frame
  "b" 'balance-windows

  "s" '(:ignore t :which-key "split")
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
(use-package conf/built-in :ensure nil)
(use-package conf/theme :ensure nil)
(use-package conf/mode-line :ensure nil)
(use-package conf/evil :ensure nil)

(use-package conf/helm :ensure nil)
(use-package conf/utilities :ensure nil)
(use-package conf/languages :ensure nil)
(use-package conf/git :ensure nil)

(use-package conf/flycheck :ensure nil)
(use-package conf/company :ensure nil)
(use-package conf/smartparens :ensure nil)

(when my--is-benchmarking-p
  (benchmark-init/deactivate))

(load custom-file 'noerror)
