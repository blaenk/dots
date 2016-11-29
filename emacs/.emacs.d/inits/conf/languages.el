(require 'use-package)
(require 'general)
(require 'conf/common)

(use-package anaconda-mode
  :defer t

  :init
  (setq anaconda-mode-installation-directory (my-cache-dir "anaconda-mode"))

  (add-hook 'python-mode-hook #'anaconda-mode)
  (add-hook 'python-mode-hook #'anaconda-eldoc-mode))

(use-package elpy
  :defer t)

(use-package lua-mode
  :defer t)

(use-package lispy
  :defer t)

(use-package dockerfile-mode
  :defer t)

(use-package cider
  :defer t

  :init
  (add-hook 'cider-mode-hook #'eldoc-mode))

;; TODO
;; requires extra setup
;; choose between ghc and haskell-mode
(use-package ghc
  :defer t)

(use-package haskell-mode
  :defer t

  :init
  (add-hook 'haskell-mode-hook #'haskell-indentation-mode))

(use-package intero
  :defer t

  :init
  (add-hook 'haskell-mode-hook #'intero-mode))

(use-package clojure-mode
  :defer t)

(use-package systemd
  :defer t)

(use-package gitconfig-mode
  :defer t)

(use-package gitignore-mode
  :defer t)

(use-package gitattributes-mode
  :defer t)

(use-package ox-gfm
  :defer t)

(use-package markdown-mode
  :defer t

  :mode
  (("\\.md\\'" . gfm-mode)
   ("\\.markdown\\'" . gfm-mode))

  :general
  (:keymaps '(markdown-mode-map gfm-mode-map)
   :states 'normal
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line)

  :init
  (my-map
    "t t" 'orgtbl-mode)

  (setq markdown-enable-math t
        markdown-asymmetric-header t
        markdown-gfm-use-electric-backquote nil
        markdown-italic-underscore t
        markdown-use-pandoc-style-yaml-metadata t)

  (defun my-gfm-hook ()
    (interactive)

    (setq-local word-wrap t)
    (setq-local outline-regexp "^\\(?:\\(#+\\)[ \t]+\\(.*?\\)[ \t]*\\(#*\\)\\)$")

    (with-eval-after-load 'evil-surround
      (push '(?s . ("**" . "**")) evil-surround-pairs-alist)
      (push '(?i . ("_" . "_")) evil-surround-pairs-alist)
      (push '(?p . ("<span class=\"path\">" . "</span>"))
            evil-surround-pairs-alist)))

  (add-hook 'gfm-mode-hook #'my-gfm-hook)
  (add-hook 'gfm-mode-hook #'outline-minor-mode)
  (add-hook 'gfm-mode-hook #'flyspell-mode)
  (add-hook 'gfm-mode-hook #'visual-line-mode)
  (add-hook 'gfm-mode-hook #'whitespace-mode)
  (add-hook 'gfm-mode-hook #'typo-mode)

  :config
  (add-to-list 'markdown-gfm-additional-languages "cpp")
  (add-to-list 'markdown-gfm-additional-languages "elisp"))

(use-package yaml-mode
  :defer t

  :init
  (add-hook 'yaml-mode-hook #'turn-off-flyspell t)
  (add-hook 'yaml-mode-hook #'flyspell-prog-mode t))

(use-package enh-ruby-mode
  :defer t
  :interpreter "ruby"
  :mode (("\\.rb\\'" . enh-ruby-mode)
         ("Rakefile" . enh-ruby-mode)
         ("Gemfile" . enh-ruby-mode)
         ("\\.rake\\'" . enh-ruby-mode)))

(use-package robe
  :defer t

  :init
  (add-hook 'ruby-mode-hook #'robe-mode)
  (add-hook 'enh-ruby-mode-hook #'robe-mode))

(use-package inf-ruby
  :defer t

  :init
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode)
  (add-hook 'enh-ruby-mode-hook #'inf-ruby-minor-mode)

  :config
  (inf-ruby-switch-setup))

(use-package projectile-rails
  :defer t

  :general
  (my-map :keymaps 'ruby-mode-map
    "m r" '(:keymap projectile-rails-command-map :which-key "rails"))

  :init
  (add-hook 'ruby-mode-hook #'projectile-rails-on)

  :config
  (remove-hook 'enh-ruby-mode-hook 'erm-define-faces))

(use-package erlang
  :defer t)

(use-package scala-mode
  :defer t)

(use-package go-mode
  :defer t

  :init
  (setq gofmt-command "goimports")

  (add-hook 'go-mode-hook #'subword-mode)
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package go-eldoc
  :defer t

  :init
  (add-hook 'go-mode-hook #'go-eldoc-setup))

(use-package less-css-mode
  :defer t)

(use-package scss-mode
  :mode "\\.sass\\'"

  :init
  (defun my-scss-hook ()
    (setq-local comment-end "")
    (setq-local comment-start "//"))

  (add-hook 'scss-mode-hook #'my-scss-hook))

(use-package css-eldoc
  :defer t
  :commands turn-on-css-eldoc

  :init
  (add-hook 'css-mode-hook #'turn-on-css-eldoc)
  (add-hook 'less-css-mode-hook #'turn-on-css-eldoc)
  (add-hook 'scss-mode-hook #'turn-on-css-eldoc))

(use-package elixir-mode
  :defer t)

(use-package alchemist
  :defer t)

(use-package irony
  :general
  (:keymaps 'irony-mode-map
   [remap completion-at-point] 'irony-completion-at-point-async
   [remap complete-symbol] 'irony-completion-at-point-async)

  :init
  (setq irony-user-dir (my-cache-dir "irony"))

  (add-hook 'c++-mode-hook #'irony-mode)
  (add-hook 'c-mode-hook #'irony-mode)
  (add-hook 'objc-mode-hook #'irony-mode)
  (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options))

(use-package irony-eldoc
  :defer t

  :init
  (add-hook 'irony-mode-hook #'irony-eldoc))

(use-package realgud
  :defer t)

(use-package swift-mode
  :if (eq system-type 'darwin)

  :config
  (with-eval-after-load 'flycheck
    (add-to-list 'flycheck-checkers 'swift)))

(use-package vimrc-mode
  :defer t)

(use-package js2-mode
  :interpreter "node"

  :init
  (setq-default js2-basic-offset 2
                js2-mode-show-strict-warnings nil
                js2-skip-preprocessor-directives t
                js2-strict-trailing-comma-warning nil
                js2-include-node-externs t
                js2-global-externs '(
                                     "after"
                                     "afterEach"
                                     "assert"
                                     "before"
                                     "beforeEach"
                                     "context"
                                     "describe"
                                     "expect"
                                     "it"
                                     "sinon"
                                     "specify"
                                     ))

  (add-hook 'js2-mode-hook #'subword-mode)
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

  (defun my--route-expression (method)
    `(,(upcase method)
      ,(concat
        "^\\s-*\\sw+\."
        method
        "\\s-*([\"']\\(.+\\)[\"']\\s-*,.*")
      1))

  (defvar my-route-imenu-expressions
    (-map #'my--route-expression
          '("all"
            "delete"
            "get"
            "head"
            "options"
            "post"
            "put"
            "patch"
            "use"
            "trace"
            )))

  (defun my--mocha-expression (name)
    `(,(upcase-initials name)
      ,(concat
        "^\\s-*"
        name
        "\\(\.skip\\|\.only\\)?\\s-*([\"']\\(.+\\)[\"']\\s-*,.*")
      2))

  (defvar my-mocha-imenu-expressions
    (-map #'my--mocha-expression
          '("after"
            "afterEach"
            "it"
            "context"
            "beforeEach"
            "before"
            "describe")))

  (defun my-js2-imenu-index-function ()
    ;; a submenu can be created as:
    ;; (list (cons "Menu Title" (imenu--generic-function expressions)))
    (let ((js2-imenu-index (js2-mode-create-imenu-index))
          (mocha-index (imenu--generic-function my-mocha-imenu-expressions))
          (route-index (imenu--generic-function my-route-imenu-expressions)))
      (-concat js2-imenu-index mocha-index route-index)))

  (defun my-js2-imenu-extras-hook ()
    (setq-local imenu-create-index-function #'my-js2-imenu-index-function))

  (add-hook 'js2-imenu-extras-mode-hook #'my-js2-imenu-extras-hook)

  :config
  (defun my-js2-comment-line-break (&optional soft)
    (if (nth 4 (syntax-ppss))
        (js2-line-break soft)
      (comment-indent-new-line soft)))

  (defun my-js2-hook ()
    (setq-local comment-line-break-function #'my-js2-comment-line-break))

  (add-hook 'js2-mode-hook #'my-js2-hook))

(use-package rjsx-mode
  :defer t
  :mode ("\\.jsx?\\'" . rjsx-mode))

(use-package json-mode
  :defer t

  :mode
  (("\\.eslintrc\\'" . json-mode)
   ("\\.babelrc\\'" . json-mode))

  :init
  (setq json-reformat:indent-width 2)

  (add-hook 'json-mode-hook #'subword-mode))

(use-package jade
  :defer t)

(use-package js-doc
  :general
  (:keymaps 'js2-mode-map
   "@" 'js-doc-insert-tag)

  (my-map :keymaps 'js2-mode-map
    "m d" 'js-doc-insert-function-doc-snippet))

(use-package tern
  :defer t

  :init
  (add-hook 'js2-mode-hook #'tern-mode))

(use-package ggtags
  :disabled t
  :defer t

  :init
  (defun my-ggtags-hook ()
    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'rust-mode)
      (ggtags-mode 1)))

  (add-hook 'prog-mode-hook #'my-ggtags-hook))

(use-package rtags
  :defer t

  :init
  (setq rtags-completions-enabled t
        rtags-autostart-diagnostics t
        rtags-use-helm t)

  :config
  (with-eval-after-load 'evil
    (add-hook 'rtags-jump-hook 'evil--jumps-push)))

(use-package modern-cpp-font-lock
  :defer t

  :init
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

(use-package cmake-mode
  :defer t)

(use-package cmake-font-lock
  :defer t

  :init
  (add-hook 'cmake-mode-hook #'cmake-font-lock-activate))

(use-package cmake-ide
  :config
  (cmake-ide-setup))

(use-package rust-mode
  :defer t

  :init
  (defun my-rust-hook ()
    (set (make-local-variable 'compile-command) "cargo build")
    (helm-gtags-mode))

  (add-hook 'rust-mode-hook #'my-rust-hook))

(use-package racer
  :defer t

  :init
  (setq racer-rust-src-path "~/code/rust/rust/src")

  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package toml-mode
  :defer t)

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.handlebars\\'" . web-mode))

  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t))

(use-package tex-site
  :ensure auctex
  :defer t

  :init
  (setq-default TeX-master nil)

  (setq TeX-PDF-mode t
        TeX-auto-save t
        TeX-parse-self t

        TeX-view-program-selection '((output-dvi "DVI Viewer")
                                     (output-pdf "PDF Viewer")
                                     (output-html "HTML Viewer"))

        TeX-view-program-list '(("DVI Viewer" "open %o")
                                ("PDF Viewer" "open %o")
                                ("HTML Viewer" "open %o")))

  (add-hook 'LaTeX-mode-hook #'flyspell-mode)
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode))

(use-package latex-preview-pane
  :defer t)

(use-package pkgbuild-mode
  :defer t)

(use-package clang-format
  :general
  (:keymaps 'c-mode-base-map
   "C-c C-f" 'clang-format-buffer))

(use-package rustfmt
  :general
  (:keymaps 'rust-mode-map
   "C-c C-f" 'rustfmt-format-buffer))

(use-package google-c-style
  :defer t

  :init
  (add-hook 'c-mode-common-hook #'google-set-c-style))

(use-package cargo
  :defer t

  :init
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

(provide 'conf/languages)
