(require 'use-package)

(use-package anaconda-mode
  :defer t
  :init
  (setq anaconda-mode-installation-directory (blaenk/cache-dir "anaconda-mode"))
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'eldoc-mode))

(use-package tern
  :defer t
  :init
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
  :defer t
  :init
  (setq cider-auto-mode nil)
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (add-hook 'cider-mode-hook 'company-mode))

;; TODO
;; requires extra setup
;; choose between ghc and haskell-mode
(use-package ghc
  :defer t)

(use-package haskell-mode
  :defer t
  :init
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode))

(use-package clojure-mode
  :defer t)

(use-package json-mode
  :defer t
  :init
  (setq json-reformat:indent-width 2))

(use-package systemd
  :defer t)

(use-package gitconfig-mode
  :defer t)

(use-package gitignore-mode
  :defer t)

(use-package gitattributes-mode
  :defer t)

(use-package markdown-mode
  :defer t

  :mode
  (("\\.markdown\\'" . gfm-mode)
   ("\\.md\\'" . gfm-mode))

  :init
  (setq markdown-enable-math t)
  (setq markdown-asymmetric-header t)
  (setq markdown-gfm-use-electric-backquote nil)
  (setq markdown-italic-underscore t)

  (defun blaenk/gfm-hook ()
    (interactive)

    (setq-local word-wrap t)

    (with-eval-after-load 'evil-surround
      (push '(?s . ("**" . "**")) evil-surround-pairs-alist)
      (push '(?i . ("_" . "_")) evil-surround-pairs-alist)
      (push '(?p . ("<span class=\"path\">" . "</span>")) evil-surround-pairs-alist))

    (set-face-attribute
     'markdown-comment-face nil
     :strike-through nil)

    (bind-keys :map blaenk/leader-map
               ("k" . beginning-of-defun)
               ("j" . end-of-defun)))

  (add-hook 'gfm-mode-hook 'blaenk/gfm-hook)
  (add-hook 'gfm-mode-hook 'outline-minor-mode)
  (add-hook 'gfm-mode-hook 'whitespace-mode)
  (add-hook 'gfm-mode-hook 'flyspell-mode))

(use-package yaml-mode
  :defer t)

(use-package inf-ruby
  :defer t
  :init
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
  (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)

  :config
  (inf-ruby-switch-setup))

(use-package enh-ruby-mode
  :defer t)

(use-package erlang
  :defer t)

(use-package scala-mode2
  :defer t)

(use-package go-mode
  :defer t)

(use-package less-css-mode
  :defer t)

(use-package robe
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-robe))

  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'enh-ruby-mode-hook 'robe-mode))

(use-package scss-mode
  :defer t
  :mode "\\.scss\\'")

(use-package elixir-mode
  :defer t)

(use-package alchemist
  :defer t)

(use-package irony
  :init
  (setq irony-user-dir (blaenk/cache-dir "irony"))

  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)

  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun blaenk/irony-mode-hook ()
    (bind-key [remap completion-at-point] 'irony-completion-at-point-async irony-mode-map)
    (bind-key [remap complete-symbol] 'irony-completion-at-point-async irony-mode-map))

  (add-hook 'irony-mode-hook 'blaenk/irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package irony-eldoc
  :defer t
  :init
  (add-hook 'irony-mode-hook 'irony-eldoc))

(use-package swift-mode
  :if (eq system-type 'darwin)
  :defer t

  :config
  (with-eval-after-load 'flycheck
    (add-to-list 'flycheck-checkers 'swift)))

(use-package vimrc-mode
  :defer t)

(use-package js2-mode
  :mode "\\.js\\'"
  :defer t
  :interpreter "node"

  :init
  (setq-default js2-basic-offset 2)
  (setq-default js2-global-externs
                '("require" "global" "module"
                  "describe" "it" "assert"
                  "sinon"))

  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode))

(use-package cmake-mode
  :defer t)

(use-package cmake-font-lock
  :defer t
  :init
  (autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
  (add-hook 'cmake-mode-hook 'cmake-font-lock-activate))

(use-package racer
  :defer t
  :init
  (setq racer-rust-src-path "~/code/rust/rust/src")
  (setq company-tooltip-align-annotations t)
  ;; (setq racer-cmd "~/code/rust/racer/target/release/racer")

  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode))

(use-package rust-mode
  :defer t
  :init
  (defun blaenk/rust-hook ()
    (set (make-local-variable 'compile-command) "cargo build"))

  (add-hook 'rust-mode-hook 'blaenk/rust-hook))

(use-package toml-mode
  :defer t)

(use-package web-mode
  :mode "\\.html?\\'"
  :defer t
  :init
  (setq web-mode-enable-current-element-highlight t))

(use-package tex-site
  :ensure auctex
  :defer t
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

(use-package pkgbuild-mode
  :defer t)

(use-package clang-format
  :defer t
  :init
  (defun blaenk/clang-format ()
    (bind-key "C-c C-f" 'clang-format-buffer c-mode-base-map))

  (add-hook 'c++-mode-hook 'blaenk/clang-format)
  (add-hook 'c-mode-hook 'blaenk/clang-format))

(use-package rustfmt
  :defer t
  :init
  (defun blaenk/rustfmt ()
    (bind-key "C-c C-f" 'rustfmt-format-buffer rust-mode-map))

  (add-hook 'rust-mode-hook 'blaenk/rustfmt))

(use-package google-c-style
  :defer t)

(use-package cmake-ide
  :config
  (cmake-ide-setup))

(use-package cargo
  :defer t
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode))
