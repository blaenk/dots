(require 'use-package)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-copy-envs
   '("VM" "PATH" "GTAGSCONF" "GTAGSLABEL" "SSH_AUTH_SOCK")))

(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

;; TODO
;; use (member "Symbola" (font-family-list))
;; to fall back on unicode icons
(use-package fontawesome
  :init
  (defun blaenk/set-char-widths (alist)
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

  (blaenk/set-char-widths
   `((2 . (,(string-to-char (fontawesome "cloud"))
           ,(string-to-char (fontawesome "refresh")))))))

(use-package dtrt-indent)

(use-package paradox)

(use-package ag
  :init
  (setq ag-reuse-buffers t)
  (setq ag-project-root-function
        (lambda (file-or-dir-name)
          (let ((default-directory file-or-dir-name))
            (projectile-project-root))))
  (setq ag-highlight-search t))

(use-package anzu
  :diminish anzu-mode
  :init
  (defun blaenk/anzu-update (here total)
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

  (setq anzu-mode-line-update-function 'blaenk/anzu-update)
  (setq anzu-cons-mode-line-p nil)

  :config
  (add-hook 'anzu-mode-hook (lambda () (make-local-variable 'anzu--state)))
  (global-anzu-mode +1))

(use-package browse-at-remote
  :bind
  ("C-c g o" . browse-at-remote/kill))

;; TODO necessary? required with use-package
(use-package diminish)

(use-package expand-region
  :bind
  ("C-=" . er/expand-region)

  :config
  ;; (evil-define-key 'visual global-map (kbd "v") 'er/expand-region)
  )

(use-package buffer-move)

(use-package frame-cmds)

(use-package hydra)

(use-package olivetti)

(use-package ace-link
  :config
  (ace-link-setup-default))

(use-package hl-todo
  :config
  (add-hook 'prog-mode-hook 'hl-todo-mode))

(use-package pcache
  :config
  ;; TODO
  ;; pending https://github.com/sigma/pcache/issues/6
  ;; should change this, might hide other things that may be stored in var/
  ;; (delete-directory (concat user-emacs-directory "var") 'recursive)
  (setq pcache-directory (blaenk/cache-dir "var/pcache")))

(use-package gist
  :bind
  (("C-c g g s" . gist-region-or-buffer-private)
   ("C-c g g p" . gist-region-or-buffer)))

(use-package highlight-escape-sequences
  :config
  (hes-mode))

(use-package highlight-quoted
  :init
  (setq highlight-quoted-highlight-symbols nil)

  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode))

(use-package mmm-mode
  :disabled t
  :demand t

  :bind
  ("C-c m" . mmm-parse-buffer)

  :init
  (setq mmm-global-mode 'maybe)
  (setq mmm-parse-when-idle t)

  :config
  (mmm-add-classes
   '((gfm-toml-metadata
      :submode toml-mode
      :front "\\`---$"
      :back "\n---$")))

  (mmm-add-mode-ext-class 'gfm-mode nil 'gfm-toml-metadata)

  (defun blaenk/mmm-markdown-auto-class (lang &optional submode)
    "Define a mmm-mode class for LANG in `markdown-mode' using SUBMODE.
If SUBMODE is not provided, use `LANG-mode' by default."
    (let ((class (intern (concat "gfm-" lang)))
          (submode (or submode (intern (concat lang "-mode"))))
          (front (concat "^``` ?" lang "[\n\r]+"))
          (back "^```"))
      (mmm-add-classes (list (list class :submode submode :front front :back back)))
      (mmm-add-mode-ext-class 'gfm-mode nil class)))

  (mapc 'blaenk/mmm-markdown-auto-class
        '("c"
          "cpp"
          "haskell"
          "html"
          "java"
          "python"
          "ruby"
          "rust"
          ))

  ;; NOTE for when language and mode-name differ
  (blaenk/mmm-markdown-auto-class "shell" 'sh-mode)
  (blaenk/mmm-markdown-auto-class "bash" 'sh-mode)
  (blaenk/mmm-markdown-auto-class "elisp" 'emacs-lisp-mode))

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
  ;;       `((".*" . ,(blaenk/cache-dir "undos/"))))
  ;; (setq undo-tree-auto-save-history t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)

  :config
  (global-undo-tree-mode)

  (defadvice undo-tree-make-history-save-file-name
      (after undo-tree activate)
    (setq ad-return-value (concat ad-return-value ".gz"))))

(use-package multi-term
  :init
  (setq multi-term-buffer-name "term")
  (setq multi-term-program "/usr/bin/zsh"))

(use-package visual-regexp)

(use-package swiper
  :init
  (setq ivy-use-virtual-buffers t)

  :bind
  (("C-s" . swiper)
   ([f6] . ivy-resume)))

(use-package pt)

(use-package rainbow-mode
  :demand t
  :diminish rainbow-mode
  :bind
  ("C-c r c" . rainbow-mode)

  :config
  ;; disable highlighting color names
  (setq rainbow-x-colors nil))

(use-package rainbow-blocks
  :bind
  ("C-c r b" . rainbow-blocks-mode))

(use-package rainbow-delimiters
  :demand t
  :bind
  ("C-c r d" . rainbow-delimiters-mode)

  :config
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(use-package color-identifiers-mode
  :bind
  ("C-c r i" . color-identifiers-mode))

(use-package relative-line-numbers
  :init
  (defun abs-rel-numbers (offset)
    (if (= offset 0)
        (format "%3d " (line-number-at-pos))
      (format "%3d " (abs offset))))

  (setq relative-line-numbers-format #'abs-rel-numbers)

  :config
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key "n" 'relative-line-numbers-mode))

  (setq relative-line-numbers-motion-function 'forward-visible-line)
  (add-hook 'prog-mode-hook 'relative-line-numbers-mode))

(use-package wgrep)

(use-package wgrep-ag)

(use-package fill-column-indicator
  :config
  (with-eval-after-load 'magit
    ;; TODO does this actually work?
    (add-hook 'git-commit-setup-hook 'fci-mode)))

(use-package bug-reference-github
  :config
  (add-hook 'find-file-hook 'bug-reference-github-set-url-format))

(use-package ace-window
  :bind
  ("C-x o" . ace-window)
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package shackle
  :init
  (setq shackle-rules
        '((help-mode :select t)
          (compilation-mode :noselect t)))

  :config
  (shackle-mode))

(use-package reveal-in-osx-finder
  :if (eq system-type 'darwin))

(use-package highlight-numbers
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package restclient)

(use-package emojify
  :init
  (setq emojify-prog-contexts 'comments)
  (setq emojify-point-entered-behaviour 'uncover)

  :config
  (add-to-list 'emojify-inhibit-major-modes 'magit-status-mode)
  (add-to-list 'emojify-inhibit-major-modes 'magit-revision-mode)
  (add-hook 'after-init-hook 'global-emojify-mode))

(use-package emoji-cheat-sheet-plus
  :bind
  ("C-x 8 e" . emoji-cheat-sheet-plus-insert))

(use-package list-environment)
(use-package narrow-indirect)

(use-package emmet-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode))

(use-package ggtags
  :config
  (add-hook 'prog-mode-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'rust-mode)
                (ggtags-mode 1)))))

(use-package rtags)
