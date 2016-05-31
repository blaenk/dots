(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'auto-compile)
  (package-install 'benchmark-init))

(require 'benchmark-init)

(setq load-prefer-newer t)
(setq backup-by-copying t)

(require 'auto-compile)
(setq auto-compile-display-buffer nil)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(setq use-package-always-ensure t)

(require 's)
(require 'f)
(require 'dash)

(add-to-list 'load-path (expand-file-name "inits/" user-emacs-directory) t)

(require 'conf/common)

(let* ((auto-save-dir (blaenk/cache-dir "autosaves/")))
  (setq auto-save-list-file-prefix (expand-file-name "saves-" auto-save-dir))
  (setq auto-save-file-name-transforms `((".*" ,auto-save-dir t))))

(setq backup-directory-alist `((".*" . ,(blaenk/cache-dir "backups/"))))

(defun emacs-session-filename (session-id)
  (blaenk/cache-dir (concat "sessions/" session-id)))

(when (getenv "VM")
  (setq browse-url-browser-function 'kill-new))

(setq version-control t)
(setq delete-old-versions t)

(setq enable-recursive-minibuffers t)

(setq inhibit-x-resources t)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(setq x-underline-at-descent-line t)
(setq save-interprogram-paste-before-kill t)

(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

(setq mouse-yank-at-point t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed t)
(setq mouse-wheel-follow-mouse 't)

(setq help-window-select t)

(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)

(setq-default fill-column 80)

(setq visible-bell t)
(setq ring-bell-function 'ignore)

(setq delete-by-moving-to-trash t)

(setq history-delete-duplicates t)

(setq tab-always-indent nil)
(setq-default indent-tabs-mode nil)

(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)

(fset 'yes-or-no-p 'y-or-n-p)

(setq load-prefer-newer t)

(setq sentence-end-double-space nil)
(setq-default cursor-type 'box)
(setq-default echo-keystrokes 0.1)
(setq uniquify-buffer-name-style 'forward)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(add-to-list 'auto-coding-alist '("\\.nfo\\'" . ibm437))

(bind [remap eval-last-sexp] 'pp-eval-last-sexp)
(bind [remap eval-expression] 'pp-eval-expression)

(bind "TAB" (lambda () (interactive) (insert-tab)))

;; unicode mappings
(require 'iso-transl)
(bind :keymaps 'iso-transl-ctl-x-8-map
  "<right>" "→"
  "<left>" "←"
  "n" "ñ")

(bind "C-c u" 'paradox-list-packages)

(defun blaenk/kill-this-buffer ()
  (interactive)
  (let ((buffer-modified-p nil))
    (kill-buffer (current-buffer))))

(bind "C-c k" 'blaenk/kill-this-buffer)

(defun blaenk/switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(bind "C-c o" 'blaenk/switch-to-previous-buffer)

(defun blaenk/split-with-previous-buffer ()
  (interactive)
  (select-window (split-window-below))
  (blaenk/switch-to-previous-buffer))

(bind "C-c x" 'blaenk/split-with-previous-buffer)
(bind "C-c b" 'bury-buffer)

(defun blaenk/frame-options (frame)
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

(blaenk/frame-options nil)

(add-hook 'after-make-frame-functions 'blaenk/frame-options)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)

(savehist-mode)
(visual-line-mode)
(column-number-mode)
(winner-mode)
(show-paren-mode)
(which-function-mode)

(defun blaenk/pop-to-frame ()
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

(bind "C-c f" 'blaenk/pop-to-frame)

(defun blaenk/force-save ()
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(bind "C-S-x C-S-s" 'blaenk/force-save)

(byte-recompile-directory (blaenk/inits-dir "") 0)

(benchmark-init/activate)

(require 'conf/theme)
(require 'conf/built-in)
(require 'conf/utilities)
(require 'conf/languages)
(require 'conf/flycheck)
(require 'conf/mode-line)
(require 'conf/evil)
(require 'conf/helm)
(require 'conf/magit)
(require 'conf/company)
(require 'conf/smartparens)

(benchmark-init/deactivate)

(setq custom-file (blaenk/cache-dir "custom.el"))
(load custom-file 'noerror)
