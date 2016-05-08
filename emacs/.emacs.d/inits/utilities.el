(require 'use-package)

(use-package exec-path-from-shell
  :if (not (equal system-type 'windows-nt))
  :config
  (exec-path-from-shell-copy-envs
   '("VM" "PATH" "GTAGSCONF" "GTAGSLABEL" "SSH_AUTH_SOCK")))

(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

(use-package bind-map
  :after evil
  :config
  (bind-map blaenk/leader-map
            :keys ("M-m")
            :evil-keys ("SPC")
            :evil-states (normal motion visual)))

(use-package help-fns+
  :commands describe-keymap)

;; TODO
;; use (member "Symbola" (font-family-list))
;; to fall back on unicode icons
(use-package fontawesome
  :config
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

(use-package dtrt-indent
  :defer t)

(use-package paradox
  :defer t)

(use-package ag
  :if (executable-find "ag")
  :defer t
  :init
  (defun blaenk/ag-root-function (file-or-dir-name)
    (let ((default-directory file-or-dir-name))
      (projectile-project-root)))

  (setq ag-highlight-search t)
  (setq ag-project-root-function 'blaenk/ag-root-function))

;; TODO
;; can configure test dirs by configuring projectile-test-prefix etc
;; see default implementation
(use-package projectile
  :init
  (defun blaenk/edit-inits ()
    (interactive)
    (projectile-switch-project-by-name "~/.dots"))

  (setq projectile-completion-system 'helm)
  (setq projectile-cache-file (blaenk/cache-dir "projectile.cache"))
  (setq projectile-known-projects-file (blaenk/cache-dir "projectile-bookmarks.eld"))

  ;; consider files ending in _test to be tests
  (defun blaenk/projectile-test-suffix-function (project-type)
    (or (projectile-test-suffix project-type) "_test"))

  (setq projectile-test-suffix-function 'blaenk/projectile-test-suffix-function)

  :config
  (bind-key "C-c e" 'blaenk/edit-inits)
  (bind-key "C-M-/" 'helm-projectile-ag)

  (add-to-list 'projectile-other-file-alist '("cc" "h" "hpp" "hh"))
  (add-to-list 'projectile-other-file-alist '("h" "c" "cpp" "cc"))

  (projectile-global-mode))

(use-package perspective
  :disabled t
  :config
  ;; (persp-mode)
  )

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

  (defun blaenk/anzu-hook ()
    (make-local-variable 'anzu--state))

  (setq anzu-mode-line-update-function 'blaenk/anzu-update)
  (setq anzu-cons-mode-line-p nil)

  :config
  (add-hook 'anzu-mode-hook 'blaenk/anzu-hook)
  (global-anzu-mode +1))

(use-package browse-at-remote
  :defer t
  :bind
  ("C-c g o" . browse-at-remote/kill))

;; TODO remove
(use-package diminish)

(use-package expand-region
  :defer t
  :bind
  ("C-=" . er/expand-region)

  :init
  ;; (evil-define-key 'visual global-map (kbd "v") 'er/expand-region)
  (with-eval-after-load 'bind-map
    (bind-key "v" 'er/expand-region blaenk/leader-map))
  )

(use-package buffer-move)

(use-package frame-cmds)

(use-package hydra
  :config
  (with-eval-after-load 'evil
    (require 'evil-vars)
    (require 'evil-commands)

    (defhydra hydra-move-to-window (evil-window-map "g")
      "move to window"
      ("q" nil)

      ("j" evil-window-down)
      ("k" evil-window-up)
      ("h" evil-window-left)
      ("l" evil-window-right))

    (with-eval-after-load 'buffer-move
      (require 'evil-vars)
      (defhydra hydra-move-buffer (evil-window-map "m")
        "move buffer"
        ("q" nil)

        ("j" buf-move-down)
        ("k" buf-move-up)
        ("h" buf-move-left)
        ("l" buf-move-right)))

    (with-eval-after-load 'frame-cmds
      (defhydra hydra-resize-frame (evil-window-map "f")
        "resize frame"
        ("q" nil)

        ("j" enlarge-frame)
        ("k" shrink-frame)
        ("h" shrink-frame-horizontally)
        ("l" enlarge-frame-horizontally)))

    (defhydra hydra-resize-window (evil-window-map "r")
      "resize window"
      ("q" nil)

      ("=" balance-windows)
      ("m" evil-window-set-height)

      ("f" hydra-resize-frame/body "resize frame" :exit t)

      ("j" shrink-window)
      ("k" enlarge-window)
      ("h" shrink-window-horizontally)
      ("l" enlarge-window-horizontally))))

(use-package olivetti
  :defer t)

(use-package ace-link
  :defer t
  :config
  (ace-link-setup-default))

(use-package link-hint
  :defer t
  :bind
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link)

  :init
  (setq link-hint-avy-style 'post))

(use-package hl-todo
  :defer t
  :init
  (add-hook 'prog-mode-hook 'hl-todo-mode))

(use-package pcache
  :defer t
  :config
  ;; TODO
  ;; pending https://github.com/sigma/pcache/issues/6
  ;; should change this, might hide other things that may be stored in var/
  ;; (delete-directory (concat user-emacs-directory "var") 'recursive)
  (setq pcache-directory (blaenk/cache-dir "var/pcache")))

(use-package gist
  :defer t
  :bind
  (("C-c g g s" . gist-region-or-buffer-private) ;; s for secret
   ("C-c g g p" . gist-region-or-buffer)))       ;; p for public

(use-package highlight-escape-sequences
  :defer t
  :init
  (add-hook 'prog-mode-hook 'hes-mode))

(use-package highlight-quoted
  :defer t
  :init
  (setq highlight-quoted-highlight-symbols nil)
  (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode))

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
  :defer t
  :init
  (setq multi-term-buffer-name "term")
  (setq multi-term-program "/usr/bin/zsh"))

(use-package visual-regexp
  :defer t)

(use-package swiper
  :defer t

  :bind
  (("C-s" . swiper)
   ("M-/" . swiper)
   ("M-?" . swiper-all)
   ([f6] . ivy-resume))

  :init
  (setq ivy-use-virtual-buffers t))

(use-package counsel
  :defer t)

(use-package pt
  :if (executable-find "pt")
  :defer t)

(use-package rainbow-mode
  :defer t
  :diminish rainbow-mode
  :bind
  ("C-c r c" . rainbow-mode)

  :init
  ;; disable highlighting color names
  (setq rainbow-x-colors nil))

(use-package rainbow-blocks
  :defer t
  :bind
  ("C-c r b" . rainbow-blocks-mode))

(use-package rainbow-delimiters
  :defer t
  :bind
  ("C-c r d" . rainbow-delimiters-mode)

  :init
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(use-package color-identifiers-mode
  :defer t
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
  (with-eval-after-load 'bind-map
    (bind-key "n" 'relative-line-numbers-mode blaenk/leader-map))

  (setq relative-line-numbers-motion-function 'forward-visible-line)
  (add-hook 'prog-mode-hook 'relative-line-numbers-mode))

(use-package wgrep
  :defer t)

(use-package wgrep-ag
  :defer t)

(use-package fill-column-indicator
  :defer t
  :init
  (setq fci-rule-use-dashes t)
  (setq fci-dash-pattern 0.50)

  (with-eval-after-load 'bind-map
    (bind-key "c" 'fci-mode blaenk/leader-map))

  (defun blaenk/git-commit-fill-column ()
    (fci-mode 1))

  (with-eval-after-load 'magit
    (add-hook 'git-commit-setup-hook 'blaenk/git-commit-fill-column)))

(use-package bug-reference-github
  :defer t
  :init
  (add-hook 'find-file-hook 'bug-reference-github-set-url-format))

(use-package ace-window
  :defer t
  :bind
  ("C-x o" . ace-window)
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package shackle
  :init
  (setq shackle-rules
        '((help-mode :select t)
          (compilation-mode :noselect t)
          ("*Diff*" :select t :frame t)
          ))

  :config
  (shackle-mode))

(use-package reveal-in-osx-finder
  :if (eq system-type 'darwin)
  :defer t)

(use-package highlight-numbers
  :defer t
  :init
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package restclient
  :defer t)

(use-package emojify
  :defer t
  :init
  (setq emojify-program-contexts 'comments)
  (setq emojify-point-entered-behaviour 'uncover)
  (setq emojify-emojis-dir (blaenk/cache-dir "emojis"))

  (add-hook 'after-init-hook 'global-emojify-mode)

  :config
  (blaenk/setq-append
   emojify-inhibit-major-modes
   magit-status-mode
   magit-revision-mode))

(use-package emoji-cheat-sheet-plus
  :defer t
  :bind
  ("C-x 8 e" . emoji-cheat-sheet-plus-insert))

(use-package list-environment
  :defer t)

(use-package narrow-indirect
  :defer t)

(use-package emmet-mode
  :defer t
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode))

(use-package ggtags
  :defer t
  :init
  (defun blaenk/ggtags-hook ()
    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'rust-mode)
      (ggtags-mode 1)))

  (add-hook 'prog-mode-hook 'blaenk/ggtags-hook))

(use-package rtags
  :init
  (setq rtags-completions-enabled t)
  (setq rtags-autostart-diagnostics t)

  :config
  ;; (rtags-diagnostics)
  )

(use-package esup
  :commands esup)

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
      (mmm-add-classes (list (list class
                                   :submode submode
                                   :front front
                                   :back back)))
      (mmm-add-mode-ext-class 'gfm-mode nil class)))

  ;; (mapc 'blaenk/mmm-markdown-auto-class
  ;;       '("c"
  ;;         "haskell"
  ;;         "html"
  ;;         "java"
  ;;         "python"
  ;;         "ruby"
  ;;         "rust"
  ;;         ))

  ;; ;; NOTE for when language and mode-name differ
  ;; (blaenk/mmm-markdown-auto-class "cpp" 'c++-mode)
  ;; (blaenk/mmm-markdown-auto-class "shell" 'sh-mode)
  ;; (blaenk/mmm-markdown-auto-class "bash" 'sh-mode)
  ;; (blaenk/mmm-markdown-auto-class "elisp" 'emacs-lisp-mode)
  )
