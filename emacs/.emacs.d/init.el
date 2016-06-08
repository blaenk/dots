(require 'package)

;; frame
(defun my-frame-options (frame)
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10.5"))
  (add-to-list 'default-frame-alist '(width . 86))
  (add-to-list 'default-frame-alist '(height . 36))

  (cond
   ((eq system-type 'gnu/linux)
    (set-fontset-font "fontset-default" nil
                      (font-spec :name "Symbola") nil 'prepend))
   ((eq system-type 'darwin)
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

(use-package benchmark-init)

(setq load-prefer-newer t
      backup-by-copying t

      use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package auto-compile
  :defines
  auto-compile-display-buffer

  :init
  (setq auto-compile-display-buffer nil))

(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(use-package dash)
(use-package f)
(use-package s)

(use-package general)

(add-to-list 'load-path (expand-file-name "inits/" user-emacs-directory) t)

(byte-recompile-directory (expand-file-name "inits/" user-emacs-directory) 0)

(require 'conf/common)

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

(bind
  [remap eval-last-sexp] 'pp-eval-last-sexp
  [remap eval-expression] 'pp-eval-expression)

(bind "TAB" (lambda () (interactive) (insert-tab)))

;; unicode mappings
(require 'iso-transl)
(bind :keymaps 'iso-transl-ctl-x-8-map
  "<right>" "→"
  "<left>" "←"
  "n" "ñ")

(bind "C-c u" 'paradox-list-packages)

(defun my-kill-this-buffer ()
  (interactive)
  (let ((buffer-modified-p nil))
    (kill-buffer (current-buffer))))

(bind "C-c k" 'my-kill-this-buffer)

(defun my-switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(bind "C-c o" 'my-switch-to-previous-buffer)

(defun my-split-with-previous-buffer ()
  (interactive)
  (select-window (split-window-below))
  (my-switch-to-previous-buffer))

(bind "C-c x" 'my-split-with-previous-buffer)
(bind "C-c b" 'bury-buffer)

(defun my-frame-options (frame)
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10.5"))
  (add-to-list 'default-frame-alist '(width . 86))
  (add-to-list 'default-frame-alist '(height . 36))

  (cond
   ((eq system-type 'gnu/linux)
    (set-fontset-font "fontset-default" nil
                      (font-spec :name "Symbola") nil 'prepend))
   ((eq system-type 'darwin)
    (set-fontset-font t 'symbol
                      (font-spec :family "Apple Color Emoji") nil 'prepend)
    (set-fontset-font t 'symbol
                      (font-spec :family "Apple Symbols") nil 'prepend))))

(my-frame-options nil)

(add-hook 'after-make-frame-functions #'my-frame-options)

(defun my-pop-to-frame ()
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

(bind "C-c f" 'my-pop-to-frame)

(defun my-force-save ()
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(bind "C-S-x C-S-s" 'my-force-save)

;; (benchmark-init/activate)

(require 'conf/built-in)
(require 'conf/mode-line)
(require 'conf/theme)
(require 'conf/evil)

(require 'conf/helm)
(require 'conf/utilities)
(require 'conf/languages)
(require 'conf/git)

(require 'conf/flycheck)
(require 'conf/company)
(require 'conf/smartparens)

;; (benchmark-init/deactivate)

(load custom-file 'noerror)
