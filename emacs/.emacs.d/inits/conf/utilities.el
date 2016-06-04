(require 'use-package)
(require 'general)

(use-package exec-path-from-shell
  :if (and (daemonp) (not (equal system-type 'windows-nt)))

  :config
  (exec-path-from-shell-copy-envs
   '("VM" "PATH" "GTAGSCONF" "GTAGSLABEL" "SSH_AUTH_SOCK")))

(use-package unicode-fonts
  :disabled t

  :config
  (unicode-fonts-setup))

(use-package which-key
  :diminish which-key-mode

  :init
  (setq which-key-idle-delay 1.0)
  (setq which-key-use-C-h-commands nil)

  :config
  (which-key-mode))

(use-package help-fns+
  :commands describe-keymap)

;; TODO
;; use (member "Symbola" (font-family-list))
;; to fall back on unicode icons
(use-package fontawesome
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
  (setq dtrt-indent-verbosity 0)

  (add-hook 'prog-mode-hook #'dtrt-indent-mode))

(use-package paradox
  :defer t)

(use-package ag
  :if (executable-find "ag")
  :defer t

  :init
  (defun my-ag-root-function (file-or-dir-name)
    (let ((default-directory file-or-dir-name))
      (projectile-project-root)))

  (setq ag-highlight-search t)
  (setq ag-project-root-function #'my-ag-root-function))

;; TODO
;; can configure test dirs by configuring projectile-test-prefix etc
;; see default implementation
(use-package projectile
  :defer t

  :general
  ("C-c e" 'my-edit-inits
   "C-M-/" 'helm-projectile-ag)

  :init
  (defun my-edit-inits ()
    (interactive)
    (projectile-switch-project-by-name "~/.dots"))

  (setq projectile-completion-system 'helm)
  (setq projectile-cache-file (my-cache-dir "projectile.cache"))
  (setq projectile-known-projects-file (my-cache-dir "projectile-bookmarks.eld"))

  ;; consider files ending in _test to be tests
  (defun my-projectile-test-suffix-function (project-type)
    (or (projectile-test-suffix project-type) "_test"))

  (setq projectile-test-suffix-function #'my-projectile-test-suffix-function)

  (add-hook 'after-init-hook #'projectile-global-mode)

  :config
  (add-to-list 'projectile-other-file-alist '("cc" "h" "hpp" "hh"))
  (add-to-list 'projectile-other-file-alist '("h" "c" "cpp" "cc")))

(use-package perspective
  :disabled t)

(use-package anzu
  :diminish anzu-mode

  :init
  (defun my-anzu-update (here total)
    (when anzu--state
      (let ((status
             (cond
              ((eq anzu--state 'search)
               (format " %s of %d%s "
                       (anzu--format-here-position here total)
                       total (if anzu--overflow-p "+" "")))
              ((eq anzu--state 'replace-query) (format " %d replace " total))
              ((eq anzu--state 'replace) (format " %d of %d " here total)))))
        (propertize status 'face 'anzu-mode-line))))

  (defun my-anzu-hook ()
    (make-local-variable 'anzu--state))

  (setq anzu-mode-line-update-function #'my-anzu-update)
  (setq anzu-cons-mode-line-p nil)

  :config
  (add-hook 'anzu-mode-hook #'my-anzu-hook)
  (global-anzu-mode +1))

(use-package expand-region
  :defer t

  :general
  ("C-=" 'er/expand-region)
  (bind* "v" 'er/expand-region))

(use-package buffer-move
  :defer t)

(use-package frame-cmds
  :defer t)

(use-package olivetti
  :defer t)

(use-package link-hint
  :general
  ("C-c l o" 'link-hint-open-link
   "C-c l c" 'link-hint-copy-link)

  :init
  (setq link-hint-avy-style 'post))

(use-package hl-todo
  :defer t

  :init
  (add-hook 'prog-mode-hook #'hl-todo-mode))

(use-package gist
  :general
  ("C-c g g s" 'gist-region-or-buffer-private ;; s for secret
   "C-c g g p" 'gist-region-or-buffer))       ;; p for public

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
  ;; (setq undo-tree-history-directory-alist
  ;;       `((".*" . ,(my-cache-dir "undos/"))))
  ;; (setq undo-tree-auto-save-history t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff nil)

  :config
  (defadvice undo-tree-make-history-save-file-name
      (after undo-tree activate)
    (setq ad-return-value (concat ad-return-value ".gz")))

  (global-undo-tree-mode))

(use-package multi-term
  :defer t

  :init
  (setq multi-term-buffer-name "term")
  (setq multi-term-program "/usr/bin/zsh"))

(use-package visual-regexp
  :defer t)

(use-package swiper
  :general
  ("C-s" 'swiper
   "M-/" 'swiper
   "M-?" 'swiper-all
   [f6] 'ivy-resume)

  :init
  (setq swiper-action-recenter t)
  (setq ivy-use-virtual-buffers t))

(use-package counsel
  :defer t)

(use-package pt
  :if (executable-find "pt")
  :defer t)

(use-package rainbow-mode
  :diminish rainbow-mode

  :general ("C-c r c" 'rainbow-mode)

  :init
  ;; disable highlighting color names
  (setq rainbow-x-colors nil)

  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package rainbow-blocks
  :general ("C-c r b" 'rainbow-blocks-mode))

(use-package rainbow-delimiters
  :general ("C-c r d" 'rainbow-delimiters-mode)

  :init
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

(use-package color-identifiers-mode
  :general ("C-c r i" 'color-identifiers-mode))

(use-package relative-line-numbers
  :general
  (bind* "n" 'relative-line-numbers-mode)

  :init
  (defun abs-rel-numbers (offset)
    (if (= offset 0)
        (format "%3d " (line-number-at-pos))
      (format "%3d " (abs offset))))

  (setq relative-line-numbers-format #'abs-rel-numbers)
  (setq relative-line-numbers-motion-function #'forward-visible-line)

  (add-hook 'prog-mode-hook #'relative-line-numbers-mode))

(use-package wgrep
  :defer t)

(use-package wgrep-ag
  :defer t)

(use-package fill-column-indicator
  :general
  (bind* "c" 'fci-mode)

  :init
  (setq fci-rule-use-dashes t)
  (setq fci-dash-pattern 0.50)

  (defun my-git-commit-fill-column ()
    (fci-mode 1))

  (add-hook 'git-commit-setup-hook #'my-git-commit-fill-column))

(use-package bug-reference-github
  :defer t

  :init
  (add-hook 'find-file-hook #'bug-reference-github-set-url-format))

(use-package ace-window
  :defer t

  :general ("C-x o" 'ace-window)

  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package shackle
  :init
  (setq shackle-rules
        '((help-mode :select t)
          (compilation-mode :noselect t)
          ("*Diff*" :select t :frame t)
          ("*Package Commit List*" :select t)))

  :config
  (shackle-mode))

(use-package reveal-in-osx-finder
  :if (eq system-type 'darwin)
  :defer t)

(use-package highlight-numbers
  :defer t

  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package restclient
  :defer t)

(use-package emojify
  :defer t

  :init
  (setq emojify-program-contexts 'comments)
  (setq emojify-point-entered-behaviour 'uncover)
  (setq emojify-emojis-dir (my-cache-dir "emojis"))

  (add-hook 'after-init-hook #'global-emojify-mode)

  :config
  (my-setq-append
   emojify-inhibit-major-modes
   flycheck-error-list-mode
   magit-status-mode
   magit-revision-mode))

(use-package emoji-cheat-sheet-plus
  :general
  ("C-x 8 e" 'emoji-cheat-sheet-plus-insert))

(use-package list-environment
  :defer t)

(use-package narrow-indirect
  :defer t)

(use-package emmet-mode
  :defer t

  :init
  (add-hook 'sgml-mode-hook #'emmet-mode)
  (add-hook 'css-mode-hook  #'emmet-mode))

(use-package esup
  :defer t)

(provide 'conf/utilities)
