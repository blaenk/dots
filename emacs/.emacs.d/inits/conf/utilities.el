(require 'use-package)
(require 'general)
(require 'conf/common)

(use-package discover-my-major
  :general
  ([remap describe-mode] 'discover-my-major
   "C-h M-m" 'discover-my-mode))

(use-package imenu-anywhere
  :general
  ("M-I" 'imenu-anywhere))

(use-package imenu-list
  :defer t)

(use-package exec-path-from-shell
  :if (and (daemonp) (not (equal system-type 'windows-nt)))

  :config
  (exec-path-from-shell-copy-envs
   '("VM" "PATH" "GTAGSCONF" "GTAGSLABEL" "SSH_AUTH_SOCK")))

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
  (setq dtrt-indent-verbosity 0))

(use-package paradox
  :general
  (my-map
    "e u" 'paradox-list-packages))

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

  :config
  (projectile-global-mode)

  (add-to-list 'projectile-other-file-alist '("cc" "h" "hpp" "hh"))
  (add-to-list 'projectile-other-file-alist '("h" "c" "cpp" "cc")))

(use-package term-projectile
  :disabled t)

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

(use-package expand-region
  :general
  ("C-=" 'er/expand-region))

(use-package buffer-move
  :general
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

(use-package olivetti
  :defer t)

(use-package typo
  :defer t

  :init
  (setq-default typo-language "English"))

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

(use-package counsel
  :defer t)

(use-package pt
  :if (executable-find "pt")
  :defer t)

(use-package rainbow-mode
  :diminish rainbow-mode

  :general
  (my-map
    "t r" '(:ignore t :which-key "rainbow")
    "t r c" 'rainbow-mode)

  :init
  ;; disable highlighting color names
  (setq rainbow-x-colors nil)

  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package rainbow-blocks
  :general
  (my-map "t r b" 'rainbow-blocks-mode))

(use-package rainbow-delimiters
  :general
  (my-map "t r d" 'rainbow-delimiters-mode)

  :init
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

(use-package color-identifiers-mode
  :general
  (my-map "t r i" 'color-identifiers-mode))

(use-package relative-line-numbers
  :general
  (my-map "t n" 'relative-line-numbers-mode)

  :init
  (defun abs-rel-numbers (offset)
    (if (= offset 0)
        (format "%3d " (line-number-at-pos))
      (format "%3d " (abs offset))))

  (setq relative-line-numbers-format #'abs-rel-numbers
        relative-line-numbers-motion-function #'forward-visible-line)

  (add-hook 'prog-mode-hook #'relative-line-numbers-mode))

(use-package wgrep
  :defer t)

;; TODO
;; this only works when ag-group-matches is nil
;; preferably keep it on unless necessary, so perhaps
;; if wgrep-ag is invoked, rerun ag with it off?
(use-package wgrep-ag
  :defer t)

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
    "w o" 'ace-window
    "o w" 'ace-window)

  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package shackle
  :init
  (setq shackle-rules
        '((help-mode :select t)
          (compilation-mode :noselect t)
          ("*Diff*" :select t :frame t)
          ("*Flycheck errors*" :select t)
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
  :defer t
  :mode ("\\.rest\\'" . restclient-mode))

(use-package know-your-http-well
  :defer t)

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

(use-package emoji-cheat-sheet-plus
  :general
  ("C-x 8 e" 'emoji-cheat-sheet-plus-insert))

(use-package list-environment
  :defer t)

(use-package narrow-indirect
  :defer t)

(use-package emmet-mode
  :defer t

  :general
  (:keymaps 'emmet-mode-keymap
   "C-S-j" 'my-emmet-preview)

  :init
  (defun my-emmet-jsx ()
    (setq-local emmet-expand-jsx-className? t))

  (add-hook 'js2-jsx-mode-hook #'my-emmet-jsx)

  (add-hook 'js2-mode-hook #'emmet-mode)
  (add-hook 'js2-jsx-mode-hook #'emmet-mode)
  (add-hook 'sgml-mode-hook #'emmet-mode)
  (add-hook 'css-mode-hook  #'emmet-mode)
  (add-hook 'web-mode-hook  #'emmet-mode)

  :config
  (defun my-emmet-preview ()
    (interactive)
    (emmet-expand-line '(4))))

(use-package yasnippet
  :defer t

  :general
  (:keymaps 'yas-minor-mode-map
   "<tab>" nil
   "TAB" nil
   "M-n" 'yas-expand
   "M-N" 'company-yasnippet)

  :init
  (setq yas-indent-line 'auto
        yas-also-auto-indent-first-line t)

  (defun my-evil-insert-before-yasnippet-expand ()
    (evil-insert-state))

  (add-hook 'yas-before-expand-snippet-hook
            #'my-evil-insert-before-yasnippet-expand)

  (add-hook 'after-init-hook 'yas-global-mode))

(use-package mocha-snippets
  :defer t)

(use-package react-snippets
  :defer t)

(provide 'conf/utilities)
