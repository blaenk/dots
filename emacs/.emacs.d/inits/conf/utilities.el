(require 'use-package)
(require 'general)
(require 'conf/common)

(use-package pcre2el :defer t)

(use-package discover-my-major
  :general
  ([remap describe-mode] 'discover-my-major
   "C-h M-m" 'discover-my-mode))

(use-package eyebrowse
  :demand t

  :general
  (my-map :keymaps 'eyebrowse-mode-map :infix "w w"
    "" '(:ignore t :which-key "eyebrowse")
    "w" 'eyebrowse-switch-to-window-config
    "l" 'eyebrowse-next-window-config
    "h" 'eyebrowse-prev-window-config
    "o" 'eyebrowse-last-window-config
    "k" 'eyebrowse-close-window-config
    "r" 'eyebrowse-rename-window-config
    "n" 'eyebrowse-create-window-config

    "0" 'eyebrowse-switch-to-window-config-0
    "1" 'eyebrowse-switch-to-window-config-1
    "2" 'eyebrowse-switch-to-window-config-2
    "3" 'eyebrowse-switch-to-window-config-3
    "4" 'eyebrowse-switch-to-window-config-4
    "5" 'eyebrowse-switch-to-window-config-5
    "6" 'eyebrowse-switch-to-window-config-6
    "7" 'eyebrowse-switch-to-window-config-7
    "8" 'eyebrowse-switch-to-window-config-8
    "9" 'eyebrowse-switch-to-window-config-9)

  :init
  (setq eyebrowse-switch-back-and-forth t
        eyebrowse-wrap-around t
        eyebrowse-new-workspace t

        eyebrowse-mode-line-separator ""
        eyebrowse-mode-line-left-delimiter ""
        eyebrowse-mode-line-right-delimiter "")

  :config
  (eyebrowse-mode t))

(use-package beginend
  :config
  (beginend-global-mode))

(use-package sudo-edit
  :defer t

  :general
  (my-map
    "o s" 'sudo-edit)

  :init
  (setq sudo-edit-user "root")

  (with-eval-after-load 'evil-ex
    (evil-ex-define-cmd "w!!" 'sudo-edit)))

(use-package imenu-list
  :defer t

  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state #'imenu-list-major-mode 'emacs)))

(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

(use-package which-key
  :diminish which-key-mode

  :init
  (setq which-key-idle-delay 0.3
        which-key-idle-secondary-delay 0.3
        ;; which-key-echo-keystrokes 0.01
        which-key-use-C-h-commands nil
        which-key-side-window-max-height 1.0)

  (defun my-which-key-delay (prefix length)
    (unless (or (> length 1)
                (string-match-p "^\\(SPC\\|M-SPC\\|C-c\\)" prefix))
      1.0))

  (add-hook 'which-key-delay-functions 'my-which-key-delay)

  :config
  (which-key-mode))

(use-package help-fns+
  :general
  (:keymaps 'help-map
   "M-k" 'describe-keymap))

;; TODO
;; use (member "Symbola" (font-family-list))
;; to fall back on unicode icons
(use-package fontawesome
  :if (not (eq system-type 'windows-nt))
  :defer t

  :config
  (defun my-set-char-widths (alist)
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

  (my-set-char-widths
   `((2 . (,(string-to-char (fontawesome "cloud"))
           ,(string-to-char (fontawesome "refresh")))))))

(use-package dtrt-indent
  :defer t

  :init
  (setq dtrt-indent-verbosity 0))

(use-package vdiff :defer t)

(use-package easy-escape :defer t)

(use-package paradox
  :general
  (my-map
    "e u" 'paradox-list-packages)

  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state #'paradox-menu-mode 'emacs)
    (evil-set-initial-state #'paradox-commit-list-mode 'emacs)))

(use-package ag
  :if (executable-find "ag")
  :defer t

  :init
  (defun my-ag-root-function (file-or-dir-name)
    (let ((default-directory file-or-dir-name))
      (projectile-project-root)))

  (setq ag-highlight-search t
        ;; needed for wgrep-ag. preferably this would be set as-needed
        ag-group-matches nil
        ag-project-root-function #'my-ag-root-function))

(use-package projectile
  :general
  (my-map
    "p" '(:keymap projectile-command-map
          :which-key "projectile"))

  :init
  ;; consider files ending in _test to be tests
  (defun my-projectile-test-suffix-function (project-type)
    (or (projectile-test-suffix project-type) "_test"))

  (setq projectile-sort-order 'recently-active
        projectile-completion-system 'helm
        projectile-cache-file (my-cache-dir "projectile.cache")
        projectile-known-projects-file (my-cache-dir "projectile-bookmarks.eld")
        projectile-test-suffix-function #'my-projectile-test-suffix-function)

  (when (eq system-type 'windows-nt)
    (setq projectile-indexing-method 'alien))

  :config
  (projectile-global-mode)

  (add-to-list 'projectile-other-file-alist '("cc" "h" "hpp" "hh"))
  (add-to-list 'projectile-other-file-alist '("h" "c" "cpp" "cc")))

(use-package term-projectile :disabled t)

(use-package anzu
  :diminish anzu-mode

  :init
  (defun my-anzu-update (here total)
    (when anzu--state
      (let ((status
             (cond
              ((eq anzu--state 'search)
               (format " %s/%d%s "
                       (anzu--format-here-position here total)
                       total (if anzu--overflow-p "+" "")))
              ((eq anzu--state 'replace-query) (format " %d replace " total))
              ((eq anzu--state 'replace) (format " %d of %d " here total)))))
        (propertize status 'face 'anzu-mode-line))))

  (defun my-anzu-hook ()
    (make-local-variable 'anzu--state))

  (setq anzu-mode-line-update-function #'my-anzu-update
        anzu-cons-mode-line-p nil)

  :config
  (add-hook 'anzu-mode-hook #'my-anzu-hook)
  (global-anzu-mode +1))

(use-package buffer-move
  :general
  (my-map :infix "w m"
    "" '(:ignore t :which-key "move")
    "k" 'buf-move-up
    "j" 'buf-move-down
    "h" 'buf-move-left
    "l" 'buf-move-right)

  (my-map :infix "b m"
    "" '(:ignore t :which-key "move")
    "k" 'buf-move-up
    "j" 'buf-move-down
    "h" 'buf-move-left
    "l" 'buf-move-right))

(use-package frame-cmds
  :general
  (:keymaps 'global
   "C-M-S-h" 'shrink-frame-horizontally
   "C-M-S-l" 'enlarge-frame-horizontally
   "C-M-S-k" 'shrink-frame
   "C-M-S-j" 'enlarge-frame))

(use-package olivetti :defer t)

(use-package link-hint
  :general
  (my-map
    "o l" 'link-hint-open-link)

  :init
  (setq link-hint-avy-style 'post))

(use-package hl-todo
  :defer t

  :init
  (add-hook 'prog-mode-hook #'hl-todo-mode))

(use-package highlight-escape-sequences
  :defer t

  :init
  (add-hook 'prog-mode-hook #'hes-mode))

(use-package highlight-quoted
  :defer t

  :init
  (setq highlight-quoted-highlight-symbols nil)

  (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode))

(use-package undo-tree
  :diminish undo-tree-mode

  :init
  ;; NOTE
  ;; undo-tree breaks sometimes, some people think it may be
  ;; the persistent history feature that is causing this
  ;;
  ;; it's a huge pain when it breaks because I lose a lot of
  ;; undo history, so I'm gonna try to disable the persistent
  ;; feature for a while to see if the problem goes away
  ;;
  ;; https://github.com/syl20bnr/spacemacs/issues/298
  ;; https://github.com/syl20bnr/spacemacs/issues/774
  ;; https://github.com/syl20bnr/spacemacs/commit/885d092e72aeaa470253c19831ba42e2eecf3514
  ;; http://comments.gmane.org/gmane.emacs.vim-emulation/2079
  (setq undo-tree-history-directory-alist
        `((".*" . ,(my-cache-dir "undos/"))))
  (setq undo-tree-auto-save-history nil)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff nil)

  :config
  (define-advice undo-tree-make-history-save-file-name
      (:filter-return (file) compress-undo-history)
    "Compress the persisted undo-tree history."
    (concat file ".gz"))

  (define-advice undo-tree-visualize
      (:around (old-func) vertical-split)
    "Force undo-tree-visualize to show up on the right."
    (let ((split-height-threshold nil)
          (split-width-threshold 0))
      (funcall old-func)))

  (global-undo-tree-mode))

(use-package multi-term
  :defer t

  :init
  (setq multi-term-buffer-name "term"
        multi-term-program "/usr/bin/zsh"))

(use-package visual-regexp
  :defer t

  :init
  (setq vr/default-replace-preview t))

(use-package swiper
  :general
  ("C-s" 'swiper
   ;; "M-/" 'swiper
   "M-?" 'swiper-all
   [f6] 'ivy-resume)

  :init
  (setq swiper-action-recenter t
        ivy-use-virtual-buffers t))

(use-package rainbow-mode
  :diminish rainbow-mode

  :general
  (my-map
    "t r" 'rainbow-mode)

  :init
  ;; Disable highlighting color names.
  (setq rainbow-x-colors nil))

(use-package rainbow-delimiters
  :defer t

  :init
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

(use-package linum-relative
  :if (< emacs-major-version 26)

  :init
  (setq linum-relative-current-symbol ""
        linum-relative-format "%4s ")

  (add-hook 'prog-mode-hook #'linum-relative-mode))

(use-package wgrep :defer t)

;; TODO
;; this only works when ag-group-matches is nil
;; preferably keep it on unless necessary, so perhaps
;; if wgrep-ag is invoked, rerun ag with it off?
(use-package wgrep-ag :defer t)

(use-package fill-column-indicator
  :general
  (my-map "t c" 'fci-mode)

  :init
  (setq fci-rule-use-dashes t
        fci-dash-pattern 0.50))

(use-package ace-window
  :defer t

  :general
  (my-map
    "k o W" 'ace-delete-window
    "w m m" 'ace-swap-window
    "o w" 'ace-window)

  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-dispatch-alist
        '((?d aw-delete-window " Ace - Delete Window")
          (?m aw-swap-window " Ace - Swap Window")
          (?M aw-move-window " Ace - Move Window")
          (?n aw-flip-window)
          (?c aw-split-window-fair " Ace - Split Fair Window")
          (?b aw-split-window-vert " Ace - Split Vert Window")
          (?v aw-split-window-horz " Ace - Split Horz Window")
          (?i delete-other-windows " Ace - Delete Other Windows")
          (?o delete-other-windows))))

(use-package shackle
  :config
  (setq shackle-rules
        '((help-mode :select t)
          (compilation-mode :select t)
          (cargo-process-mode :select t)
          ("*Diff*" :select t :frame t)
          ("*Flycheck errors*" :select t)
          ("*Package Commit List*" :select t)))

  (shackle-mode))

(use-package reveal-in-osx-finder
  :if (eq system-type 'darwin)
  :defer t)

(use-package highlight-numbers
  :defer t

  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode))

(use-package know-your-http-well :defer t)

(use-package emojify
  :general
  (my-map
    "t e" 'emojify-mode)

  :init
  (setq emojify-program-contexts '(comments)
        emojify-point-entered-behaviour 'uncover
        emojify-emojis-dir (my-cache-dir "emojis"))

  :config
  (setq emojify-inhibit-major-modes
        (append emojify-inhibit-major-modes
                '(flycheck-error-list-mode
                  magit-status-mode
                  magit-revision-mode))))

(use-package list-environment :defer t)

(use-package narrow-indirect :defer t)

(use-package yasnippet
  :defer t

  :general
  (:keymaps 'yas-minor-mode-map
   "<tab>" nil
   "TAB" nil
   "M-n" 'my-yasnippet)

  :init
  (setq yas-indent-line 'auto
        yas-wrap-around-region t
        yas-also-auto-indent-first-line t)

  (defun my-yasnippet ()
    (interactive)

    ;; If there region is active or there's nothing to expand, use completing
    ;; read to select the snippet. Otherwise expand.
    (if (or (region-active-p)
            (not (yas--maybe-expand-key-filter 'yas-expand)))
        (yas-insert-snippet)
      (yas-expand)))

  (add-hook 'yas-before-expand-snippet-hook #'evil-insert-state)

  (add-hook 'after-init-hook 'yas-global-mode))

(use-package mocha-snippets :defer t)

(use-package react-snippets :defer t)

(provide 'conf/utilities)
