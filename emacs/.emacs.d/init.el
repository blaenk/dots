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

(defun blaenk/is-fullscreen ()
  (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth)))

(defun blaenk/go-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen 'fullboth))

(defun blaenk/un-fullscreen ()
  (set-frame-parameter nil 'fullscreen nil))

(defun blaenk/fullscreen-if-wasnt ()
  (if (blaenk/is-fullscreen)
      (setq blaenk/was-fullscreen t)
    (progn
      (setq blaenk/was-fullscreen nil)
      (blaenk/go-fullscreen))))

(defun blaenk/unfullscreen-if-wasnt ()
  (when (not blaenk/was-fullscreen)
    (blaenk/un-fullscreen)))

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

(defmacro blaenk/setq-append (var &rest elems)
  `(setq ,var (append ,var '(,@elems))))

(use-package s)
(use-package dash)

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

(setq tab-always-indent nil)
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

(bind-key [remap eval-expression] 'pp-eval-expression)

;; unicode mappings
(require 'iso-transl)
(bind-keys :map iso-transl-ctl-x-8-map
           ("<right>" . "→")
           ("<left>" . "←")
           ("n" . "ñ"))

;; TODO why this, then rebind to universal-argument-more?
(bind-key "M-u" 'universal-argument)

(defun blaenk/kill-this-buffer ()
  (interactive)
  (let ((buffer-modified-p nil))
    (kill-buffer (current-buffer))))

(bind-key "C-c k" 'blaenk/kill-this-buffer)
(bind-key "C-c s" 'save-buffer)

(defun blaenk/q-switch-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(bind-key "C-c o" 'blaenk/q-switch-buffer)
(bind-key "C-c b" 'bury-buffer)
(bind-key "M-u" 'universal-argument-more universal-argument-map)

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
(electric-pair-mode)
(show-paren-mode)
(which-function-mode)

(defun blaenk/pop-to-frame ()
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

(bind-key "C-c f" 'blaenk/pop-to-frame)

(defun blaenk/force-save ()
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(bind-key "C-S-x C-S-s" 'blaenk/force-save)

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
