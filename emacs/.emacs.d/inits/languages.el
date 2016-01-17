(require 'use-package)

(use-package anaconda-mode
  :init
  (setq anaconda-mode-installation-directory (blaenk/cache-dir "anaconda-mode"))
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'eldoc-mode))

(use-package tern
  :config
  (add-hook 'js2-mode-hook 'tern-mode))

(use-package latex-preview-pane)

(use-package lua-mode
  :mode "\\.lua$"
  :interpreter "lua")

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

;; TODO
;; configure thoroughly when used
;; https://github.com/clojure-emacs/cider
(use-package cider
  :init
  (setq cider-auto-mode nil)

  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (add-hook 'cider-mode-hook 'company-mode))

;; TODO
;; requires extra setup
;; choose between ghc and haskell-mode
(use-package ghc)

(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode))

(use-package clojure-mode)

(use-package json-mode
  :init
  (setq json-reformat:indent-width 2))

(use-package systemd)

(use-package gitconfig-mode)
(use-package gitignore-mode)
(use-package gitattributes-mode)

(use-package markdown-mode
  :mode
  (("\\.markdown\\'" . gfm-mode)
   ("\\.md\\'" . gfm-mode))

  :init
  (setq markdown-enable-math t)
  (setq markdown-asymmetric-header t)

  :config
  (add-hook 'gfm-mode-hook (lambda ()
                             (interactive)
                             (set-face-attribute
                              'markdown-comment-face nil
                              :strike-through nil)
                             (evil-leader/set-key
                               "k" 'beginning-of-defun
                               "j" 'end-of-defun)))
  (add-hook 'gfm-mode-hook 'whitespace-mode)
  (add-hook 'gfm-mode-hook 'flyspell-mode))

(use-package yaml-mode)

(use-package inf-ruby
  :config
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
  (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)
  (inf-ruby-switch-setup))

(use-package enh-ruby-mode)

(use-package erlang
  :defer t)

(use-package scala-mode2)

(use-package go-mode)

(use-package less-css-mode)

(use-package robe
  :config
  (with-eval-after-load 'company
    (push 'company-robe company-backends))

  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'enh-ruby-mode-hook 'robe-mode))

(use-package scss-mode
  :mode "\\.scss\\'")

(use-package elixir-mode)

(use-package alchemist)

(use-package irony
  :init
  (setq irony-user-dir (blaenk/cache-dir "irony"))

  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)

  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun blaenk/irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))

  (add-hook 'irony-mode-hook 'blaenk/irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package irony-eldoc)

(use-package swift-mode
  :if (eq system-type 'darwin)

  :config
  (with-eval-after-load 'flycheck
    (add-to-list 'flycheck-checkers 'swift)))

(use-package vimrc-mode)

(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node"

  :init
  (setq-default js2-basic-offset 2)
  (setq-default js2-global-externs
                '("require" "global" "module"
                  "describe" "it" "assert"
                  "sinon"))

  :config
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode))

(use-package cmake-mode)

(use-package cmake-font-lock
  :config
  (autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
  (add-hook 'cmake-mode-hook 'cmake-font-lock-activate))

(use-package racer
  :init
  (setq racer-rust-src-path "~/code/rust/rust/src")
  ;; (setq racer-cmd "~/code/rust/racer/target/release/racer")

  :config
  (setq company-tooltip-align-annotations t)
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package rust-mode
  :init
  (add-hook 'rust-mode-hook
            (lambda ()
              (set (make-local-variable 'compile-command) "cargo build"))))

(use-package toml-mode)

(use-package web-mode
  :mode "\\.html?\\'"
  :init
  (setq web-mode-enable-current-element-highlight t))

(use-package tex-site
  :ensure auctex
  :init
  (setq TeX-PDF-mode t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)

  (setq TeX-view-program-selection
        '((output-dvi "DVI Viewer")
          (output-pdf "PDF Viewer")
          (output-html "HTML Viewer")))

  (setq TeX-view-program-list
        '(("DVI Viewer" "open %o")
          ("PDF Viewer" "open %o")
          ("HTML Viewer" "open %o")))

  :config
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode))

(use-package pkgbuild-mode)

(use-package clang-format
  :config
  (define-key c-mode-base-map (kbd "C-c C-f") 'clang-format-buffer))

(use-package rustfmt
  :config
  (define-key rust-mode-map (kbd "C-c C-f") 'rustfmt-format-buffer))

(use-package google-c-style)

(use-package cmake-ide
  :config
  (cmake-ide-setup))

(use-package cargo
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode))
