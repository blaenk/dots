(require 'straight)
(require 'use-package)
(require 'general)
(require 'conf/common)

(use-package pcre2el :defer t)

(use-package macrostep
  :defer t

  :general
  (my-map
    "e e m" 'macrostep-expand)

  :init
  (add-hook 'macrostep-mode-hook (my-create-evil-toggle-for-mode macrostep-mode)))

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

(use-package which-key
  :init
  (setq which-key-idle-delay 0.3
        which-key-idle-secondary-delay 0.3
        ;; which-key-echo-keystrokes 0.01
        which-key-use-C-h-commands nil
        which-key-side-window-max-height 1.0)

  (defun my--which-key-delay (prefix length)
    (unless (or (> length 1)
                (string-match-p "^\\(SPC\\|M-SPC\\|C-c\\)" prefix))
      1.0))

  (add-hook 'which-key-delay-functions #'my--which-key-delay)

  :config
  (which-key-mode))

(use-package fontawesome
  :if (not (eq system-type 'windows-nt))
  :defer t

  :config
  (my--set-char-widths
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
        ;; This is needed for wgrep-ag.
        ag-group-matches nil
        ag-project-root-function #'my-ag-root-function))

(use-package projectile
  :demand t

  :general
  (my-map
    "p" '(:keymap projectile-command-map
          :which-key "projectile"))

  :init
  ;; Consider files ending in _test to be tests.
  (defun my--projectile-test-suffix-function (project-type)
    (or (projectile-test-suffix project-type) "_test"))

  (setq projectile-sort-order 'recently-active
        projectile-completion-system 'helm
        projectile-cache-file (my-cache-dir "projectile.cache")
        projectile-known-projects-file (my-cache-dir "projectile-bookmarks.eld")
        projectile-test-suffix-function #'my--projectile-test-suffix-function
        projectile-indexing-method 'alien)

  :config
  (projectile-mode)

  (add-to-list 'projectile-other-file-alist '("cc" "h" "hpp" "hh"))
  (add-to-list 'projectile-other-file-alist '("h" "c" "cpp" "cc")))

(use-package term-projectile :disabled t)

(use-package anzu
  :init
  (setq anzu-mode-line-update-function #'my--anzu-update
        anzu-cons-mode-line-p nil)

  (defun my--anzu-update (here total)
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

  (setq anzu-mode-line-update-function #'my--anzu-update
        anzu-cons-mode-line-p nil)

  :config
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

(use-package olivetti :defer t)

(use-package link-hint
  :general
  (my-map
    "o l" 'link-hint-open-link)

  :init
  (setq link-hint-avy-style 'post))

(use-package hl-todo
  :defines hl-todo-keyword-faces

  :init
  (setq hl-todo-activate-in-modes '(prog-mode text-mode))

  (my-with-solarized-colors
   (setq hl-todo-keyword-faces
         `(("TODO"  . ,blue-lc)
           ("NOTE"  . ,yellow-lc)
           ("FIXME" . ,red-lc))))

  :config
  (global-hl-todo-mode))

(use-package highlight-escape-sequences
  :defer t

  :hook
  (prog-mode . hes-mode))

(use-package highlight-quoted
  :defer t

  :hook
  (emacs-lisp-mode . highlight-quoted-mode)

  :init
  (setq highlight-quoted-highlight-symbols nil))

(use-package undo-tree
  :init
  ;; undo-tree breaks sometimes. Some people think the persistent history
  ;; feature may be to blame.
  ;;
  ;; It's a huge pain when it breaks because I lose a lot of undo history, so
  ;; I'm gonna try to disable the persistent feature for a while to see if the
  ;; problem goes away.
  ;;
  ;; https://github.com/syl20bnr/spacemacs/issues/298
  ;; https://github.com/syl20bnr/spacemacs/issues/774
  ;; https://github.com/syl20bnr/spacemacs/commit/885d092e72aeaa470253c19831ba42e2eecf3514
  ;; http://comments.gmane.org/gmane.emacs.vim-emulation/2079
  (setq undo-tree-history-directory-alist `((".*" . ,(my-cache-dir "undos/")))
        undo-tree-visualizer-timestamps t undo-tree-visualizer-diff nil
        undo-tree-auto-save-history nil)

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

(use-package stripe-buffer
  :defer t

  :hook
  (profiler-report-mode . turn-on-stripe-buffer-mode))

(use-package rainbow-mode
  :general
  (my-map
    "t r" 'rainbow-mode)

  :init
  ;; Disable highlighting color names.
  (setq rainbow-x-colors nil))

(use-package rainbow-delimiters
  :defer t

  :hook
  ((lisp-mode emacs-lisp-mode) . rainbow-delimiters-mode))

(use-package linum-relative
  :if (< emacs-major-version 26)

  :hook
  (prog-mode . linum-relative-mode)

  :init
  (setq linum-relative-current-symbol ""
        linum-relative-format "%4s "))

(use-package wgrep
  :defer t

  :config
  (evil-define-key nil wgrep-mode-map
    [remap evil-write] #'wgrep-finish-edit))

(use-package wgrep-ag :defer t)

(use-package fill-column-indicator
  :general
  (my-map "t c" 'fci-mode)

  :init
  (setq fci-rule-use-dashes t
        fci-dash-pattern 0.50))

(use-package ace-window
  :general
  (my-map
    "k o W" 'ace-delete-window
    "w m m" 'ace-swap-window
    "o w" 'ace-window)

  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-dispatch-alist
        '((?x aw-delete-window " Ace - Delete Window")
          (?m aw-swap-window " Ace - Swap Window")
          (?M aw-move-window " Ace - Move Window")
          (?n aw-flip-window)
          (?c aw-split-window-fair " Ace - Split Fair Window")
          (?b aw-split-window-vert " Ace - Split Vert Window")
          (?v aw-split-window-horz " Ace - Split Horz Window")
          (?i delete-other-windows " Ace - Delete Other Windows")
          (?o delete-other-windows)))

  :config
  (define-advice ace-delete-window
      (:after (&rest args) balance-windows)
    "Balance the windows after deleting a window."
    (balance-windows)))

(use-package zoom-window
  :general
  (my-map :infix "w"
    "f" 'zoom-window-zoom)

  :init
  (setq zoom-window-mode-line-color nil))

(use-package shackle
  :config
  (setq shackle-rules
        '((help-mode :select t)
          (compilation-mode :select t)
          (cargo-process-mode :select t)
          ("*Diff*" :select t :frame t)
          ("*Package Commit List*" :select t)))

  (shackle-mode))

(use-package highlight-numbers
  :defer t

  :hook
  (prog-mode . highlight-numbers-mode))

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

  (when (eq system-type 'darwin)
    (setq emojify-display-style "unicode"))

  :config
  (setq emojify-inhibit-major-modes
        (append emojify-inhibit-major-modes
                '(flycheck-error-list-mode
                  magit-status-mode
                  magit-revision-mode))))

(use-package yasnippet
  :general
  (:keymaps 'yas-minor-mode-map
   "<tab>" nil
   "TAB" nil
   "M-n" 'my-yasnippet)

  (my-map
    "i s" 'my-yasnippet)

  :hook
  ((yas-before-expand-snippet . evil-insert-state)
   (after-init . yas-global-mode))

  :init
  (setq yas-indent-line 'auto
        yas-wrap-around-region t
        yas-also-auto-indent-first-line t)

  (defun my-yasnippet ()
    "Expand partial snippet or choose a snippet with helm.

If a region is active, it'll be used to \"wrap\" the selection."
    (interactive)

    ;; If there region is active or there's nothing to expand, use completing
    ;; read to select the snippet. Otherwise expand.
    (if (or (region-active-p)
            (not (yas--maybe-expand-key-filter 'yas-expand)))
        (yas-insert-snippet)
      (yas-expand))))

(use-package mocha-snippets :defer t)

(use-package react-snippets :defer t)

(use-package hydra
  :general
  (my-map
    "f r" 'my-frame-resizer/body)

  :init
  (setq hydra-lv nil)

  ;; Taken from the emacswiki frame-cmds package
  (defun my-enlarge-frame (&optional increment frame) ; Suggested binding: `C-M-down'.
    "Increase the height of FRAME (default: selected-frame) by INCREMENT.
INCREMENT is in lines (characters).
Interactively, it is given by the prefix argument."
    (interactive "p")
    (set-frame-height frame (+ (frame-height frame) increment)))

  (defun my-enlarge-frame-horizontally (&optional increment frame) ; Suggested binding: `C-M-right'.
    "Increase the width of FRAME (default: selected-frame) by INCREMENT.
INCREMENT is in columns (characters).
Interactively, it is given by the prefix argument."
    (interactive "p")
    (set-frame-width frame (+ (frame-width frame) increment)))

  (defun my-shrink-frame (&optional increment frame) ; Suggested binding: `C-M-up'.
    "Decrease the height of FRAME (default: selected-frame) by INCREMENT.
INCREMENT is in lines (characters).
Interactively, it is given by the prefix argument."
    (interactive "p")
    (set-frame-height frame (- (frame-height frame) increment)))

  (defun my-shrink-frame-horizontally (&optional increment frame) ; Suggested binding: `C-M-left'.
    "Decrease the width of FRAME (default: selected-frame) by INCREMENT.
INCREMENT is in columns (characters).
Interactively, it is given by the prefix argument."
    (interactive "p")
    (set-frame-width frame (- (frame-width frame) increment)))

  (defun my--hydra-cycle-verbosity (hydra)
    (hydra-set-property hydra :verbosity
                        (if (= (hydra-get-property hydra :verbosity) 0) 1 0)))

  (with-eval-after-load 'hydra
    (defhydra my-frame-resizer ()
      "Resize frame."

      ("j" my-enlarge-frame "🡇")
      ("k" my-shrink-frame "🡅")
      ("h" my-shrink-frame-horizontally "🡄")
      ("l" my-enlarge-frame-horizontally "🡆")

      ("f" toggle-frame-fullscreen)

      ("q" nil "quit")
      ("," nil "quit")
      ("?" (my--hydra-cycle-verbosity 'my-frame-resizer) "± verbosity"))

    (hydra-set-property 'my-frame-resizer :verbosity 0)))

(use-package mwim :defer t)

(use-package avy
  :defer t

  :init
  (setq avy-style 'pre))

(use-package helpful :defer t)

(use-package edit-indirect
  :defer t

  :general
  (:keymaps 'edit-indirect-mode-map
   [remap save-buffer] 'edit-indirect-commit))

(use-package xterm-color
  :when (eq system-type 'gnu/linux)

  :config
  (add-hook 'comint-preoutput-filter-functions #'xterm-color-filter)

  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))

  (setq compilation-environment '("TERM=xterm-256color"))

  (defun my--xterm-color-compilation ()
    ;; We need to differentiate between compilation-mode buffers
    ;; and running as part of comint (which at this point we assume
    ;; has been configured separately for xterm-color)
    (when (eq (process-filter proc) 'compilation-filter)
      ;; This is a process associated with a compilation-mode buffer.
      ;; We may call `xterm-color-filter' before its own filter function.
      (set-process-filter
       proc
       (lambda (proc string)
         (funcall 'compilation-filter proc (xterm-color-filter string))))))

  (add-hook 'compilation-start-hook #'my--xterm-color-compilation))

(use-package diredfl
  :config
  (diredfl-global-mode))

(use-package rg
  :if (executable-find "rg")
  :defer t

  :init
  (setq rg-hide-command nil))

(use-package deadgrep
  :if (executable-find "rg")
  :defer t)

(use-package ace-jump-mode
  :defer t

  :general
  (:keymaps 'normal
   "g a" 'ace-jump-char-mode))

(provide 'conf/utilities)
