;; # Emacs Daemon
;;
;; If the emacs daemon is used, certain environment variables won't be
;; available, so they should be redefined here or exec-path-from-shell should be
;; used, which spawns a shell in order to copy select environment variables.

(require 'package)

;; frame
(defun my-frame-options (frame)
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

(my-frame-options nil)

(add-hook 'after-make-frame-functions #'my-frame-options)

;; packages
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

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

(require 'diminish)
(require 'bind-key)

(use-package benchmark-init
  :defer t

  :init
  (defvar my-benchmarking-p
    (prog1
        (member "--benchmark" command-line-args)
      (setq command-line-args (delete "--benchmark" command-line-args)))
    "Whether emacs should run benchmark-init"))

(use-package auto-compile
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

(when (not (file-exists-p (my-cache-dir)))
  (make-directory (my-cache-dir)))

(let* ((auto-save-dir (my-cache-dir "autosaves/")))
  (setq auto-save-list-file-prefix (expand-file-name "saves-" auto-save-dir)
        auto-save-file-name-transforms `((".*" ,auto-save-dir t))))

(defun emacs-session-filename (session-id)
  (my-cache-dir (concat "sessions/" session-id)))

(when (getenv "VM")
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

      confirm-kill-emacs #'y-or-n-p
      custom-file (my-cache-dir "custom.el")

      scroll-conservatively 10000
      scroll-preserve-screen-position t
      scroll-step 1

      visible-bell t
      ring-bell-function #'ignore

      delete-by-moving-to-trash t

      history-delete-duplicates t

      tab-always-indent nil

      display-line-numbers-width 3

      load-prefer-newer t
      sentence-end-double-space nil
      uniquify-buffer-name-style 'forward
      visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(setq-default fill-column 80
              indent-tabs-mode nil
              tab-width 2
              cursor-type 'box
              echo-keystrokes 0.1)

(fset #'yes-or-no-p #'y-or-n-p)
(add-to-list 'auto-coding-alist '("\\.nfo\\'" . ibm437))

(defun my-kill-this-buffer ()
  (interactive)
  (let ((buffer-modified-p nil))
    (kill-buffer (current-buffer))))

(defun my-switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun my-split-with-previous-buffer ()
  (interactive)
  (select-window (split-window-below))
  (my-switch-to-previous-buffer))

(defun my-pop-to-frame ()
  (interactive)
  (unless (one-window-p)
    (let ((buffer (current-buffer)))
      (delete-window)
      (select-window (display-buffer-pop-up-frame buffer nil)))))

(defun my-force-save ()
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(defun my-touch-buffer ()
  (interactive)
  (shell-command (concat "touch " (shell-quote-argument (buffer-file-name)))))

(bind
  [remap eval-last-sexp] 'pp-eval-last-sexp
  [remap eval-expression] 'pp-eval-expression

  "C-z" nil

  "C-S-x C-S-s" 'my-force-save)

;; C-c prefix:
;;
;; @ hs
;; $ flyspell
;; , semantic

(my-map
  "e" '(:ignore t :which-key "emacs")
  "o" '(:ignore t :which-key "open")
  "o a" '(:ignore t :which-key "all")
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
  "b b" 'bury-buffer
  "b o" 'my-switch-to-previous-buffer
  "b s" 'my-force-save
  "b t" 'my-touch-buffer

  ;; windows
  "w" '(:ignore t :which-key "window")
  "w f" 'my-pop-to-frame

  "w s" '(:ignore t :which-key "split")
  "w s v" 'evil-window-vsplit
  "w s h" 'evil-window-split
  "w s p" 'my-split-with-previous-buffer)

(defun my-enable-line-numbers ()
  (setq-local display-line-numbers 'visual))

(add-hook 'prog-mode-hook #'my-enable-line-numbers)

(defun my-toggle-line-numbers ()
  (interactive)

  (setq-local display-line-numbers (if display-line-numbers nil 'visual)))

(my-map "t n" 'my-toggle-line-numbers)

(when my-benchmarking-p
  (benchmark-init/activate))

(require 'conf/built-in)
(require 'conf/theme)
(require 'conf/mode-line)
(require 'conf/evil)

(require 'conf/helm)
(require 'conf/utilities)
(require 'conf/languages)
(require 'conf/git)

(require 'conf/flycheck)
(require 'conf/company)
(require 'conf/smartparens)

(when my-benchmarking-p
  (benchmark-init/deactivate))

(load custom-file 'noerror)
