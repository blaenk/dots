(require 'use-package)
(require 'general)
(require 'conf/common)
(require 'f)
(require 'cl)

(use-package ahk-mode
  :if (eq system-type 'windows-nt)
  :defer t

  :init
  (setq ahk-indentation 2)

  (defun my-ahk-mode-hook()
    (run-mode-hooks 'prog-mode-hook))

  (add-hook 'ahk-mode-hook 'my-ahk-mode-hook))

(use-package dockerfile-mode :defer t)

(use-package systemd :defer t)

(use-package nginx-mode :defer t)

(use-package gitconfig-mode :defer t)

(use-package gitignore-mode :defer t)

(use-package gitattributes-mode :defer t)

(use-package ox-gfm :defer t)

(use-package haskell-mode :defer t)

(use-package markdown-mode
  :defer t

  :mode
  (("\\.md\\'" . gfm-mode)
   ("\\.markdown\\'" . gfm-mode))

  :general
  (:keymaps '(markdown-mode-map gfm-mode-map)
   :states 'motion
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line)

  (my-map :keymaps '(markdown-mode-map gfm-mode-map)
    "m c" 'my-insert-gfm-code-block
    "t t" 'orgtbl-mode)

  :init
  (defun my-insert-gfm-code-block ()
    (interactive)
    (call-interactively 'markdown-insert-gfm-code-block)
    (evil-insert-state))

  (setq markdown-enable-math t
        markdown-asymmetric-header t
        markdown-italic-underscore t
        markdown-use-pandoc-style-yaml-metadata t
        markdown-gfm-additional-languages '("cpp" "elisp" "postgresql"))

  (defun my-markdown-mode-hook ()
    (setq-local word-wrap t)
    (setq-local outline-regexp "^\\(?:\\(#+\\)[ \t]+\\(.*?\\)[ \t]*\\(#*\\)\\)$")

    (with-eval-after-load 'evil-surround
      (push '(?s . ("**" . "**")) evil-surround-pairs-alist)
      (push '(?i . ("_" . "_")) evil-surround-pairs-alist)
      (push '(?p . ("<span class=\"path\">" . "</span>")) evil-surround-pairs-alist)))

  (add-hook 'markdown-mode-hook #'my-markdown-mode-hook)
  (add-hook 'markdown-mode-hook #'olivetti-mode)
  (add-hook 'markdown-mode-hook #'outline-minor-mode)
  (add-hook 'markdown-mode-hook #'flyspell-mode)
  (add-hook 'markdown-mode-hook #'visual-line-mode)
  (add-hook 'markdown-mode-hook #'whitespace-mode))

(use-package markdown-edit-indirect
  :defer t

  :general
  (:keymaps 'markdown-mode-map
   "C-'" 'markdown-edit-indirect))

(use-package yaml-mode
  :defer t

  :init
  (add-hook 'yaml-mode-hook #'turn-off-flyspell t)
  (add-hook 'yaml-mode-hook #'flyspell-prog-mode t))

(use-package yard-mode
  :defer t

  :init
  (add-hook 'ruby-mode-hook 'yard-mode))

(use-package robe
  :defer t

  :init
  (add-hook 'ruby-mode-hook 'robe-mode))

(use-package inf-ruby
  :defer t

  :init
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

(use-package go-guru :defer t)

(use-package go-rename :defer t)

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

(use-package go-playground :defer t)

(use-package less-css-mode :defer t)

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
  :if (not (eq system-type 'windows-nt))
  :defer t)

(use-package vimrc-mode :defer t)

(use-package js2-mode
  :interpreter "node"
  :mode ("\\.js\\'" . js2-mode)

  :init
  (setq-default js2-basic-offset 2
                js2-ignored-warnings '("msg.no.side.effects")
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
  :mode "/\\(components\\|containers\\)/.+\\.js")

(use-package json-mode
  :mode
  (("\\.eslintrc\\'" . json-mode)
   ("\\.babelrc\\'" . json-mode))

  :init
  (setq json-reformat:indent-width 2)

  (add-hook 'json-mode-hook #'subword-mode))

(use-package indium :defer t)

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

(use-package cmake-mode :defer t)

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

(use-package rust-playground :defer t)

(use-package racer
  :defer t

  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package lsp-mode
  :disabled t
  :defer t

  :init
  (add-hook 'rust-mode-hook #'global-lsp-mode))

(use-package lsp-rust :disabled t :defer t)

(use-package toml-mode :defer t)

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.handlebars\\'" . web-mode))

  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t))

(use-package emmet-mode
  :defer t

  :general
  (:keymaps 'emmet-mode-keymap
   "C-S-j" 'my-emmet-preview)

  :init
  (defun my-emmet-jsx ()
    (setq-local emmet-expand-jsx-className? t))

  (add-hook 'rjsx-mode-hook #'my-emmet-jsx)
  (add-hook 'js2-jsx-mode-hook #'my-emmet-jsx)

  (add-hook 'js2-mode-hook #'emmet-mode)
  (add-hook 'sgml-mode-hook #'emmet-mode)
  (add-hook 'css-mode-hook  #'emmet-mode)
  (add-hook 'web-mode-hook  #'emmet-mode)

  :config
  (defun my-emmet-preview ()
    (interactive)
    (emmet-expand-line '(4))))

(use-package tex-site
  :ensure auctex
  :defer t

  :general
  (:keymaps 'LaTeX-mode-map
   "C-c C-c" 'my-latex-make-and-preview)

  :init
  (define-advice TeX-command-sentinel
      (:after (process msg) show-output-window-on-error)
    (if (not (eq (process-exit-status process) 0))
        (TeX-recenter-output-buffer nil)))

  ;; Offer to save the file if there are changes, then build the file with Make.
  (defun my-latex-make-and-preview ()
    (interactive)

    (if (and (buffer-modified-p)
             (y-or-n-p (concat "Save file " buffer-file-name "? ")))
        (save-buffer))

    (flet ((TeX-process-check (&rest IGNORE) nil))
      (TeX-command "Make" 'TeX-master-file 0)))

  ;; Introduce two expansion variables:
  ;;   %projectile-root is self-explanatory
  ;;   %pdf-path is the path to the backing file with a .pdf extension
  (setq TeX-expand-list
        '(("%projectile-root" projectile-project-root)
          ("%pdf-path" (lambda () (f-swap-ext buffer-file-name "pdf")))))

  ;; Introduce a Make command that builds the project using the Makefile at
  ;; the project root, then view the PDF with evince.
  (with-eval-after-load 'tex
    (add-to-list 'TeX-command-list
                 '("Make"
                   "make -C %projectile-root && evince %pdf-path"
                   TeX-run-command nil t
                   :help "Build with Make and View")))

  (setq TeX-PDF-mode t
        TeX-parse-self t
        TeX-view-program-selection '((output-pdf "PDF Viewer"))
        TeX-view-program-list '(("PDF Viewer" "open %pdf-path")))

  (add-hook 'LaTeX-mode-hook #'olivetti-mode)
  (add-hook 'LaTeX-mode-hook #'flyspell-mode)
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode))

(use-package latex-preview-pane :defer t)

(use-package pkgbuild-mode :defer t)

(use-package clang-format
  :general
  (:keymaps 'c-mode-base-map
   "C-c C-f" 'clang-format-buffer))

(use-package google-c-style
  :defer t

  :init
  (add-hook 'c-mode-common-hook #'google-set-c-style))

(use-package cargo
  :defer t

  :init
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

(provide 'conf/languages)
