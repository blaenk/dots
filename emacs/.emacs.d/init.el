(require 'package)

(setq load-prefer-newer t)
(setq backup-by-copying t)

(defun blaenk/emacs-dir (path)
  (expand-file-name path user-emacs-directory))

(defun blaenk/cache-dir (path)
  (blaenk/emacs-dir (concat "cache/" path)))

(defun blaenk/inits-dir (path)
  (blaenk/emacs-dir (concat "inits/" path)))

(defun blaenk/load-inits (names)
  (dolist (name names)
      (load (blaenk/inits-dir name))))

(let* ((auto-save-dir (blaenk/cache-dir "autosaves/")))
  (setq auto-save-list-file-prefix (expand-file-name "saves-" auto-save-dir))
  (setq auto-save-file-name-transforms `((".*" ,auto-save-dir t))))

(setq backup-directory-alist `((".*" . ,(blaenk/cache-dir "backups/"))))

(defun emacs-session-filename (session-id)
  (blaenk/cache-dir (concat "sessions/" session-id)))

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(setq use-package-always-ensure t)

(use-package s)
(use-package dash)

(defun blaenk/edit-init ()
  (interactive)
  (find-file (blaenk/emacs-dir "init.el")))

(global-set-key (kbd "C-c e") 'blaenk/edit-init)

(when (getenv "VM")
  (setq browse-url-browser-function 'kill-new))

(setq version-control t)
(setq delete-old-versions t)

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

(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)

(setq-default fill-column 80)

(setq visible-bell t)
(setq ring-bell-function 'ignore)

(setq delete-by-moving-to-trash t)

(setq history-delete-duplicates t)

(setq-default indent-tabs-mode nil)

(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)

(fset 'yes-or-no-p 'y-or-n-p)

(setq load-prefer-newer t)

(setq sentence-end-double-space nil)
(setq-default cursor-type 'box)
(setq-default echo-keystrokes 0.1)
(setq gc-cons-threshold 64000000)
(setq uniquify-buffer-name-style 'forward)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(add-to-list 'auto-coding-alist '("\\.nfo\\'" . ibm437))
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10.5"))
(add-to-list 'default-frame-alist '(width . 86))
(add-to-list 'default-frame-alist '(height . 36))

(global-set-key [remap eval-expression] 'pp-eval-expression)

;; unicode mappings
(define-key 'iso-transl-ctl-x-8-map "l" "→")
(define-key 'iso-transl-ctl-x-8-map "h" "←")

(define-key global-map (kbd "M-u") 'universal-argument)

(defun blaenk/kill-this-buffer ()
  (interactive)
  (let ((buffer-modified-p nil))
    (kill-buffer (current-buffer))))

(define-key global-map (kbd "C-c k") 'blaenk/kill-this-buffer)
(define-key global-map (kbd "C-c s") 'save-buffer)

(defun blaenk/q-switch-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(define-key global-map (kbd "C-c o") 'blaenk/q-switch-buffer)
(define-key global-map (kbd "C-c b") 'bury-buffer)
(define-key universal-argument-map (kbd "M-u") 'universal-argument-more)

(cond
 ((eq system-type 'gnu/linux)
  (set-fontset-font "fontset-default" nil
                    (font-spec :name "Symbola") nil 'append))
 ((eq system-type 'darwin)
  (set-fontset-font t 'symbol
                    (font-spec :family "Apple Color Emoji") nil 'prepend)
  (set-fontset-font t 'symbol
                    (font-spec :family "Apple Symbols") nil 'append)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)

(savehist-mode)
(recentf-mode)
(visual-line-mode)
(column-number-mode)
(winner-mode)
(electric-pair-mode)
(show-paren-mode)
(which-function-mode)

(defun blaenk/pop-to-frame ()
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

(global-set-key (kbd "C-c f") 'blaenk/pop-to-frame)

(defun blaenk/force-save ()
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(global-set-key (kbd "C-S-x C-S-s") 'blaenk/force-save)

(defun blaenk/get-faces (pos)
  "Get the font faces at POS."
  (remq nil
        (list
         (get-char-property pos 'read-face-name)
         (get-char-property pos 'face)
         (plist-get (text-properties-at pos) 'face))))

(defun blaenk/what-face (pos)
  (interactive "d")
  (let ((face (blaenk/get-faces pos)))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(byte-recompile-directory (blaenk/inits-dir "") 0)

(blaenk/load-inits
 '(
   "built-in"
   "theme"
   "utilities"
   "languages"
   "flycheck"
   "mode-line"
   "evil"
   "helm"
   "magit"
   "company"
   "smartparens"
   ))

(setq custom-file (blaenk/cache-dir "custom.el"))
(load custom-file 'noerror)
