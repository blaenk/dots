(require 'package)

;; TODO
;;
;; * check out eldoc mode

;; backups

(setq backup-by-copying t)

(require 'saveplace)
(setq-default save-place t)

(let* ((backup-dir (expand-file-name "backups/" user-emacs-directory))
       (undo-history-dir (expand-file-name "undos/" user-emacs-directory))
       (auto-save-dir (expand-file-name "autosaves/" user-emacs-directory))
       (auto-save-list-prefix (expand-file-name "saves-" auto-save-dir))
       (place-dir (expand-file-name "saved-places" user-emacs-directory)))
  (setq backup-directory-alist `((".*" . ,backup-dir)))
  (setq version-control t)
  (setq delete-old-versions t)
  (setq undo-tree-history-directory-alist `((".*" . ,undo-history-dir)))
  (setq auto-save-list-file-prefix auto-save-list-prefix)
  (setq auto-save-file-name-transforms `((".*" ,auto-save-dir t)))
  (setq save-place-file place-dir))

;; TODO
;; * continue comment on newline

(setq debug-on-error t)
;; (setq-default show-trailing-whitespace t)
;; FIXME
;; use space-after-tab and vice versa instead of tabs
;; this will make it possible to view tab-only files too?
(setq whitespace-style '(face trailing tabs lines-tail empty))

(when window-system (set-frame-size (selected-frame) 96 41))

(setq whitespace-line-column nil)
(setq-default fill-column 80)
(setq-default cursor-type 'box)
(setq inhibit-startup-message t)
(setq show-paren-delay 0)
(setq ring-bell-function 'ignore)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed t)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)
(setq gdb-many-windows t)
(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default echo-keystrokes 0.5)
;; 64 mb before gc kicks in
(setq gc-cons-threshold 64000000)
(fset 'yes-or-no-p 'y-or-n-p)
(setq split-height-threshold 0)
(setq split-width-threshold 0)
(setq uniquify-buffer-name-style 'forward)
(add-to-list 'auto-coding-alist '("\\.nfo\\'" . ibm437))
(setq frame-title-format
      '((:eval (replace-regexp-in-string "^ +" "" (buffer-name)))))
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq eldoc-idle-delay 0.1)

(set-frame-font "DejaVu Sans Mono-11")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(savehist-mode)
(recentf-mode)
(global-whitespace-mode)
(global-visual-line-mode)
(column-number-mode)
(flyspell-prog-mode)
(winner-mode)
(goto-address-mode)
(electric-pair-mode)
(show-paren-mode)

(defvaralias 'c-basic-offset 'tab-width)

;; ediff

(setq ediff-split-window-function 'split-window-horizontally)

(defun my-toggle-ediff-wide-display ()
  "Turn off wide-display mode (if was enabled) before quitting ediff."
  (when ediff-wide-display-p
    (ediff-toggle-wide-display)))

(add-hook 'ediff-cleanup-hook 'my-toggle-ediff-wide-display)

(global-unset-key (kbd "C-x C-c"))

(global-set-key (kbd "C-x 2")
                (lambda ()
                  (interactive)
                  (select-window (split-window-below))))

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

;; commands

(defun get-faces (pos)
  "Get the font faces at POS."
  (remq nil
        (list
         (get-char-property pos 'read-face-name)
         (get-char-property pos 'face)
         (plist-get (text-properties-at pos) 'face))))

(defun what-face (pos)
  (interactive "d")
  (let ((face (get-faces pos)))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; packages

(add-to-list 'package-archives
             '("elpa" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(package-install 'use-package)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light t)

  (solarized-with-color-variables 'light
    (custom-theme-set-faces
     'solarized-light
     `(show-paren-match ((,class (:foreground unspecified
                                  :background ,base02
                                  :weight normal)))))

    (setq evil-emacs-state-cursor `(,red box))
    (setq evil-normal-state-cursor `(,green box))
    (setq evil-visual-state-cursor `(,orange box))
    (setq evil-insert-state-cursor `(,red box))
    (setq evil-replace-state-cursor `(,red (hbar . 4)))
    (setq evil-operator-state-cursor `(,green (hbar . 4)))
    ))

(use-package auto-package-update
  :ensure t)

(use-package lua-mode
  :ensure t
  :mode "\\.lua$"
  :interpreter "lua")

(use-package ag
  :ensure t
  :config
  (setq ag-highlight-search t))

(use-package anzu
  :ensure t
  :config
  (global-anzu-mode +1))

(use-package browse-at-remote
  :ensure t
  :bind
  ("C-x g o" . browse-at-remote/to-clipboard))

(use-package cider
  :ensure t
  :init
  (setq cider-auto-mode nil)
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode))

(use-package company
  :ensure t)

(use-package clojure-mode
  :ensure t)

(use-package diminish
  :ensure t)

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode))

(use-package evil
  :ensure t

  :init
  ;; (setq evil-search-module 'evil-search)

  :config
  (with-eval-after-load 'ggtags
    (evil-make-overriding-map ggtags-mode-map)

    ;; force update evil keymaps after ggtags-mode loaded
    (add-hook 'ggtags-mode-hook #'evil-normalize-keymaps))

  (define-key evil-normal-state-map (kbd "gp") 'exchange-point-and-mark)

  (define-key evil-insert-state-map (kbd "M-v") 'evil-paste-before)

  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

  (define-key evil-normal-state-map (kbd "<kp-add>") 'evil-numbers/inc-at-pt)
  (define-key evil-visual-state-map (kbd "<kp-add>") 'evil-numbers/inc-at-pt)

  (define-key evil-normal-state-map (kbd "<kp-subtract>") 'evil-numbers/dec-at-pt)
  (define-key evil-visual-state-map (kbd "<kp-subtract>") 'evil-numbers/dec-at-pt)

  ;; unmap these so they could be used as prefix keys
  ;; this is useful for smartparens
  (define-key evil-normal-state-map (kbd "<") nil)
  (define-key evil-normal-state-map (kbd ">") nil)

  ;; still able to shift things in normal mode
  (define-key evil-normal-state-map (kbd "< <") 'evil-shift-left-line)
  (define-key evil-normal-state-map (kbd "> >") 'evil-shift-right-line)

  (evil-define-operator visual-shift-left (beg end type)
    "shift text to the left"
    :keep-visual t
    :motion evil-line
    :type line
    (interactive "<r><vc>")
    (evil-shift-left beg end)
    (evil-normal-state)
    (evil-visual-restore))

  (evil-define-operator visual-shift-right (beg end type)
    "shift text to the right"
    :keep-visual t
    :motion evil-line
    :type line
    (interactive "<r><vc>")
    (evil-shift-right beg end)
    (evil-normal-state)
    (evil-visual-restore))

  (define-key evil-visual-state-map (kbd "<") 'visual-shift-left)
  (define-key evil-visual-state-map (kbd ">") 'visual-shift-right)

  (defun my-evil-off ()
    (interactive)
    (turn-off-evil-mode))

  (setq my-evil-blacklist '(magit-mode-hook))

  (dolist (mode-hook my-evil-blacklist)
    (add-hook mode-hook 'my-evil-off))

  (evil-mode 1))

(use-package evil-anzu
  :ensure t
  :no-require t
  :config
  (with-eval-after-load 'evil
    (require 'evil-anzu)))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

(use-package evil-exchange
  :ensure t
  :config
  (evil-exchange-install))

(use-package evil-leader
  :ensure t

  :init
  (setq evil-leader/in-all-states 1)

  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")

  (evil-leader/set-key
    "n" 'relative-line-numbers-mode
    "l" 'evil-ex-nohighlight
    "m" 'evil-visual-mark-mode))

(use-package evil-numbers
  :ensure t)

(use-package evil-paredit
  :ensure t)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-visual-mark-mode
  :ensure t)

(use-package evil-visualstar
  :ensure t
  :config
  (global-evil-visualstar-mode))

(use-package flycheck
  :ensure t)

(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))

(use-package ggtags
  :ensure t

  :config
  (add-hook 'prog-mode-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1)))))

(use-package gist
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package gitconfig-mode
  :ensure t)

(use-package gitignore-mode
  :ensure t)

(use-package gitattributes-mode
  :ensure t)

(use-package git-gutter
  :ensure t
  :bind
  (("C-x g g t" . git-gutter:toggle)
   ("C-x g g n" . git-gutter:next-hunk)
   ("C-x g g p" . git-gutter:previous-hunk)))

(use-package markdown-mode
  :ensure t
  :mode ("\\.markdown\\'" "\\.md\\'"))

(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode))

(use-package helm
  :ensure t
  :bind
  (("M-x" . helm-M-x)
   ("M-i" . helm-imenu)
   ("C-x b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files)
   ("C-x r" . helm-recentf))

  :config
  (require 'helm-config)
  (helm-autoresize-mode t)

  (helm-mode 1))

(use-package helm-descbinds
  :ensure t
  :config
  (helm-descbinds-mode))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on)

  (defun my-helm-projectile ()
    (interactive)
    (helm :sources '(helm-source-projectile-buffers-list
                     helm-source-projectile-files-list
                     helm-source-projectile-recentf-list)
          :buffer "*my helm projectile*"
          :ff-transformer-show-only-basename nil
          :truncate-lines helm-buffers-truncate-lines))

  (defmacro if-projectile (is-projectile is-not)
    `(lambda ()
       (interactive)
       (if (projectile-project-p)
           (,is-projectile)
         (,is-not))))

  (define-key projectile-mode-map (kbd "M-p") 'my-helm-projectile)

  (evil-leader/set-key
    "f" (if-projectile my-helm-projectile helm-find-files)
    "b" (if-projectile helm-projectile-switch-to-buffer helm-buffers-list)))

(use-package visual-regexp
  :ensure t)

(use-package multiple-cursors
  :ensure t)

(use-package sx
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package erlang
  :ensure t)

(use-package scala-mode2
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package less-css-mode
  :ensure t)

(use-package robe
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'robe-mode) )

(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'")

(use-package elixir-mode
  :ensure t)

(use-package alchemist
  :ensure t)

(use-package racket-mode
  :ensure t)

(use-package helm-flyspell
  :ensure t)

(use-package swiper
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t)

  :bind
  (("C-s" . swiper)
   ("C-r" . swiper)
   ("C-c C-r" . ivy-resume)
   ([f6] . ivy-resume)))

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)

  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package guide-key
  :ensure t
  :init
  (setq guide-key/guide-key-sequence t)
  (setq guide-key/popup-window-position 'bottom)
  :config
  (guide-key-mode 1))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :interpreter "node")

(use-package cmake-mode
  :ensure t)

(use-package cmake-font-lock
  :ensure t
  :config
  (autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
  (add-hook 'cmake-mode-hook 'cmake-font-lock-activate))

(use-package magit
  :ensure t

  :bind
  (("C-x g s" . magit-status)
   ("C-x g p" . magit-dispatch-popup))

  :config
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))

(use-package magit-filenotify
  :ensure t
  :config
  (add-hook 'magit-status-mode-hook 'magit-filenotify-mode))

(use-package magit-gh-pulls
  :ensure t
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

(use-package magit-gitflow
  :ensure t)

(use-package multiple-cursors
  :ensure t)

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'helm)
  :config
  (projectile-global-mode))

(use-package racer
  :ensure t
  :no-require t

  :init
  (setq racer-rust-src-path "~/code/rust/rust/src")
  (setq racer-cmd "~/code/rust/racer/target/release/racer")
  (add-to-list 'load-path "~/code/rust/racer/editors/emacs")

  :config
  (with-eval-after-load 'rust-mode
    (require 'racer))

  (add-hook 'rust-mode-hook
            '(lambda ()
               (racer-activate)
               ;; (local-set-key (kbd "M-.") #'racer-find-definition)
               (local-set-key (kbd "TAB") #'racer-complete-or-indent))))

(use-package rainbow-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package relative-line-numbers
  :ensure t
  :init
  (defun abs-rel-numbers (offset)
    (if (= offset 0)
        (format "%3d " (line-number-at-pos))
      (format "%3d " (abs offset))))

  (setq relative-line-numbers-format #'abs-rel-numbers)

  :config
  (add-hook 'prog-mode-hook 'relative-line-numbers-mode))

(use-package rust-mode
  :ensure t)

(use-package on-parens
  :ensure t)

(use-package smartparens
  :ensure t

  :init
  (setq sp-show-pair-from-inside t)
  
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)

  (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
  (add-hook 'clojure-mode-hook 'smartparens-mode)
  (add-hook 'racket-mode-hook 'smartparens-mode)
  (add-hook 'scheme-mode-hook 'smartparens-mode)

  ;; (add-hook 'smartparens-enabled-hook #'smartparens-strict-mode)

  (define-key smartparens-mode-map (kbd "M-S") 'sp-split-sexp)
  (define-key smartparens-mode-map (kbd "M-J") 'sp-join-sexp)

  ;; TODO bind % to jump toggle matching pair

  (with-eval-after-load 'on-parens
    (with-eval-after-load 'evil
      ;; https://github.com/tpope/vim-sexp-mappings-for-regular-people

      (defun my-evil-goto-char (pos)
        (when (evil-normal-state-p) (decf pos))
        (goto-char pos))

      ;; TODO evil-commentary delegating sp-comment wrapper?

      (define-key evil-normal-state-map (kbd "> )")
        (lambda ()
          (interactive)
          (on-parens-forward-slurp)
          ;; get back on paren
          (sp-get (sp-get-enclosing-sexp) (my-evil-goto-char :end))))

      (define-key evil-normal-state-map (kbd "< )")
        (lambda ()
          (interactive)
          (on-parens-forward-barf)
          ;; get back on paren
          (sp-restrict-to-object 'sp-prefix-pair-object 'sp-backward-down-sexp)))

      (define-key evil-normal-state-map (kbd "> (")
        (lambda ()
          (interactive)
          (on-parens-backward-barf)
          ;; get back on paren
          (sp-restrict-to-object 'sp-prefix-pair-object 'sp-next-sexp)))

      (define-key evil-normal-state-map (kbd "< (")
        (lambda ()
          (interactive)
          (on-parens-backward-slurp)
          ;; get back on paren
          (sp-get (sp-get-enclosing-sexp) (my-evil-goto-char (+ :beg 1)))))

      ;; NOTE can use evil-define-motion to create motions out of these
      (define-key evil-normal-state-map (kbd "W") 'on-parens-forward-sexp)
      (define-key evil-normal-state-map (kbd "E") 'on-parens-forward-sexp-end)
      (define-key evil-normal-state-map (kbd "g E") 'on-parens-backward-sexp-end)
      (define-key evil-normal-state-map (kbd "B") 'on-parens-backward-sexp)

      (define-key evil-normal-state-map (kbd "< u") 'sp-splice-sexp-killing-backward)
      (define-key evil-normal-state-map (kbd "> u") 'sp-splice-sexp-killing-forward)

      (define-key evil-normal-state-map (kbd "< d")
        (lambda ()
          (interactive)
          (sp-kill-sexp '(-4))))

      (define-key evil-normal-state-map (kbd "> d")
        (lambda ()
          (interactive)
          (sp-kill-sexp '(4))))
      
      (defun sp-get-current-non-string-sexp (pos)
        "get the enclosing, non-string sexp"
        (let ((current-sexp (sp-get-sexp)))
          (if (or (eq pos (sp-get current-sexp :beg))
                  (eq pos (sp-get current-sexp :end)))
              current-sexp
            (let* ((enclosing-sexp (sp-get-enclosing-sexp))
                   (op (sp-get enclosing-sexp :op))
                   (end (sp-get enclosing-sexp :end)))
              (when enclosing-sexp
                (if (string-equal op "\"")
                    (sp-get-current-non-string-sexp (goto-char end))
                  enclosing-sexp))))))

      (defun sp-end-of-current-sexp (pos)
        "jump to the end of the current, non-string sexp"
        (interactive "d")

        (let ((end (sp-get (sp-get-current-non-string-sexp pos) :end)))
          (when end
            (my-evil-goto-char end))))

      (defmacro my-save-position (&rest body)
        "restore column and form-relative line number"
        `(let* ((column (current-column))
                (pos (point))
                (begin-line (line-number-at-pos pos)))
           (sp-end-of-current-sexp pos)
           (when (evil-normal-state-p) (forward-char))

           (let* ((end-line (line-number-at-pos (point))))
             ,@body
             (forward-line (- begin-line end-line))
             (move-to-column column))))

      (defun move-form-forward (pos &optional arg)
        "move a form forward"
        (interactive "d *p")

        (my-save-position
          (sp-transpose-sexp)))

      (defun move-form-backward (pos &optional arg)
        "move a form backward"
        (interactive "d *p")

        (my-save-position
         (sp-transpose-sexp -1)))

      (define-key evil-normal-state-map (kbd "< f")
        (sp-restrict-to-object-interactive 'sp-prefix-pair-object 'move-form-backward))

      (define-key evil-normal-state-map (kbd "> f")
        (sp-restrict-to-object-interactive 'sp-prefix-pair-object 'move-form-forward))

      (defun move-symbol-backward (&optional arg)
        "move a symbol backward"
        (interactive "*p")

        (unless (looking-at-p "\)\\|\(")
          (evil-forward-word-end)
          (evil-backward-WORD-begin))

        (sp-transpose-sexp)
        (backward-char)
        (on-parens-backward-sexp 2))
          
      (defun move-symbol-forward (&optional arg)
        "move a symbol forward"
        (interactive "*p")
        (on-parens-forward-sexp arg)
        (sp-transpose-sexp)
        (backward-char)
        (on-parens-backward-sexp arg))

      (define-key evil-normal-state-map (kbd "< s") 'move-symbol-backward)
      (define-key evil-normal-state-map (kbd "> s") 'move-symbol-forward)

      (defun insert-before-form ()
        "jump to the beginning of the sexp and go into insert mode"
        (interactive)
        (sp-beginning-of-sexp)
        (insert " ")
        (evil-backward-char)
        (evil-insert 0))

      (defun insert-after-form ()
        "jump to the end of the sexp and go into insert mode"
        (interactive)
        (sp-end-of-sexp)
        (evil-insert 0))

      (define-key evil-normal-state-map (kbd "< i") 'insert-before-form)
      (define-key evil-normal-state-map (kbd "> i") 'insert-after-form))))

(use-package toml-mode
  :ensure t)

(use-package undo-tree
  :ensure t
  :init
  (setq undo-tree-auto-save-history t)

  :config
  (global-undo-tree-mode)

  (defadvice undo-tree-make-history-save-file-name
      (after undo-tree activate)
    (setq ad-return-value (concat ad-return-value ".gz"))))

(use-package web-mode
  :ensure t)

(use-package wgrep
  :ensure t)
