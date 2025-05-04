(require 'straight)
(require 'use-package)
(require 'general)
(require 'conf/common)

(use-package macrostep
  :defer t

  :general-config
  (my-map
    "e e m" 'macrostep-expand)

  :init
  (my-create-evil-toggle-for-mode macrostep-mode))

(use-package straight
  :straight nil

  :general-config
  (my-map
    "e u" 'straight-pull-all)
  )

(use-package beginend
  :config
  (beginend-global-mode))

(use-package sudo-edit
  :general-config
  (my-map
    "o s" 'sudo-edit)

  :init
  (setq sudo-edit-user "root")

  (with-eval-after-load 'evil-ex
    (evil-ex-define-cmd "w!!" 'sudo-edit)))

(use-package which-key
  :demand t

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

(use-package dtrt-indent
  :defer t

  :init
  (setq dtrt-indent-verbosity 0))

(use-package projectile
  :demand t

  :general-config
  (my-map
    "o p" 'my-buffer-file-path

    "p" (cons "projectile" projectile-command-map))

  :init
  ;; Consider files ending in _test to be tests.
  (defun my--projectile-test-suffix-function (project-type)
    (or (projectile-test-suffix project-type) "_test"))

  (defun my-buffer-file-path ()
    (interactive)

    (kill-new
     (if buffer-file-name
         (f-relative buffer-file-name (projectile-project-root))
       (buffer-name))))

  (setq projectile-sort-order 'recently-active
        projectile-completion-system 'helm
        projectile-cache-file (my-cache-dir "projectile.cache")
        projectile-known-projects-file (my-cache-dir "projectile-bookmarks.eld")
        projectile-test-suffix-function #'my--projectile-test-suffix-function
        projectile-indexing-method 'alien
        projectile-switch-project-action 'magit-status
        )

  :config
  (projectile-mode)

  (add-to-list 'projectile-other-file-alist '("cc" "h" "hpp" "hh"))
  (add-to-list 'projectile-other-file-alist '("h" "c" "cpp" "cc")))

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

(use-package link-hint
  :general-config
  (my-map
    "o l" 'link-hint-open-link)

  :init
  (setq link-hint-avy-style 'post))

(use-package buffer-move
  :general-config
  (my-map :infix "w m"
    "" (cons "move" (make-sparse-keymap))
    "k" 'buf-move-up
    "j" 'buf-move-down
    "h" 'buf-move-left
    "l" 'buf-move-right)

  (my-map :infix "b m"
    "" (cons "move" (make-sparse-keymap))
    "k" 'buf-move-up
    "j" 'buf-move-down
    "h" 'buf-move-left
    "l" 'buf-move-right)
  )

(use-package hl-todo
  :defines hl-todo-keyword-faces

  :init
  (setq hl-todo-activate-in-modes '(prog-mode text-mode))

  (my-with-atom-one-dark-colors
   (setq hl-todo-keyword-faces
         `(("TODO"  . ,atom-one-dark-blue)
           ("NOTE"  . ,atom-one-dark-orange-2)
           ("FIXME" . ,atom-one-dark-red-1))))

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

(use-package undo-fu
  :demand t
  :if (version< emacs-version "28"))

(use-package undo-tree
  :if (version< emacs-version "27")
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

(use-package rainbow-mode
  :general-config
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
  :general-config
  (my-map "t c" 'fci-mode)

  :init
  (setq fci-rule-use-dashes t
        fci-dash-pattern 0.50))

(use-package ace-window
  :general-config
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
  ;; (define-advice ace-delete-window
  ;;     (:after (&rest args) balance-windows)
  ;;   "Balance the windows after deleting a window."
  ;;   (balance-windows))
  )

(use-package zoom-window
  :general-config
  (my-map :infix "w"
    "f" 'zoom-window-zoom)

  :init
  (setq zoom-window-mode-line-color nil))

(use-package shackle
  :config
  (setq shackle-rules
        '((help-mode :select t)
          (compilation-mode :select t)
          ("*Diff*" :select t)
          ("*Package Commit List*" :select t)))

  (shackle-mode))

(use-package highlight-numbers
  :defer t

  :hook
  (prog-mode . highlight-numbers-mode))

(use-package avy
  :defer t

  :init
  (setq avy-style 'pre))

(use-package helpful
  :defer t

  :general-config
  (my-map :infix "e H"
    "" (cons "helpful" (make-sparse-keymap))
    "f" 'helpful-callable
    "v" 'helpful-variable
    "k" 'helpful-key
    "H" 'helpful-at-point
    "F" 'helpful-function
    "c" 'helpful-command)

  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state #'helpful-mode 'emacs)))

(use-package rg
  :if (executable-find "rg")
  :defer t

  :init
  (setq rg-hide-command nil))

(use-package deadgrep
  :if (executable-find "rg")
  :defer t)

(use-package ace-jump-mode
  :general-config
  (:keymaps 'normal
   "g a" 'ace-jump-char-mode))

(use-package drag-stuff
  :hook ((prog-mode . drag-stuff-mode))

  :general-config
  (:keymaps 'normal
   "<M-up>" 'drag-stuff-up
   "<M-down>" 'drag-stuff-down))

(use-package default-text-scale
  :config
  (default-text-scale-mode))

(provide 'conf/utilities)
