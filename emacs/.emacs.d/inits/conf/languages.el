(require 'straight)
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

  (defun my--ahk-mode-hook()
    (run-mode-hooks #'prog-mode-hook))

  (add-hook 'ahk-mode-hook #'my--ahk-mode-hook))

(use-package dockerfile-mode :defer t)

(use-package systemd :defer t)

(use-package nginx-mode :defer t)

(use-package gitconfig-mode :defer t)

(use-package gitignore-mode :defer t)

(use-package gitattributes-mode :defer t)

(use-package haskell-mode :defer t)

(use-package markdown-mode
  :mode
  (("\\.md\\'" . gfm-mode)
   ("\\.markdown\\'" . gfm-mode))

  :general
  (:keymaps '(markdown-mode-map gfm-mode-map)
    "M-{" 'markdown-previous-visible-heading
    "M-}" 'markdown-next-visible-heading)

  (my-map :keymaps '(markdown-mode-map gfm-mode-map)
    "m e" 'markdown-edit-code-block

    "m t c" 'markdown-toggle-gfm-checkbox
    "m t g" 'markdown-toggle-inline-images
    "m t l" 'markdown-toggle-url-hiding
    "m t m" 'markdown-toggle-markup-hiding

    "m i b" 'markdown-insert-bold
    "m i c" 'markdown-insert-gfm-code-block
    "m i C" 'my-markdown-insert-gfm-code-block-and-edit
    "m i f" 'my-markdown-insert-named-footnote
    "m i g" 'markdown-insert-image
    "m i h" 'markdown-insert-header-dwim
    "m i i" 'markdown-insert-italic
    "m i k" 'my-markdown-insert-kbd
    "m i l" 'markdown-insert-link
    "m i m" 'markdown-insert-list-item
    "m i p" 'my-markdown-insert-path
    "m i q" 'markdown-insert-blockquote
    "m i s" 'markdown-insert-strikethrough

    "m o l" 'markdown-follow-thing-at-point

    ;; Jump between footnote marker and definition, etc.
    "m s o" 'markdown-do

    "m m" '(:ignore t :which-key "move")
    "m m k" 'markdown-move-up
    "m m j" 'markdown-move-down

    "m p" '(:ignore t :which-key "promote")
    "m p p" 'markdown-promote
    "m p d" 'markdown-demote

    "t t" 'orgtbl-mode)

  :init
  (setq markdown-enable-math t
        markdown-asymmetric-header t
        markdown-italic-underscore t
        markdown-use-pandoc-style-yaml-metadata t
        markdown-fontify-code-blocks-natively t
        markdown-footnote-location 'immediately
        markdown-reference-location 'immediately
        markdown-gfm-additional-languages '("cpp" "elisp" "postgresql"))

  (defun my-markdown-insert-kbd ()
    "Insert the kbd snippet."
    (interactive)

    (yas-expand-snippet (yas-lookup-snippet "kbd")))

  (defun my-markdown-insert-path ()
    "Insert the path snippet."
    (interactive)

    (yas-expand-snippet (yas-lookup-snippet "path")))

  (defun my-markdown-insert-gfm-code-block-and-edit ()
    (interactive)

    (add-hook 'edit-indirect-after-creation-hook
              #'evil-insert-state
              nil 'local)

    (let ((current-prefix-arg '(4)))
      (call-interactively 'markdown-insert-gfm-code-block))

    (remove-hook 'edit-indirect-after-creation-hook
                 #'evil-insert-state
                 'local)

    (evil-normal-state))

  (defun my--markdown-mode-hook ()
    (setq-local word-wrap t)

    (with-eval-after-load 'evil-surround
      (push '(?s . ("**" . "**")) evil-surround-pairs-alist)
      (push '(?i . ("_" . "_")) evil-surround-pairs-alist)
      (push '(?p . ("<span class=\"path\">" . "</span>")) evil-surround-pairs-alist)))

  (add-hook 'markdown-mode-hook #'my--markdown-mode-hook)
  (add-hook 'markdown-mode-hook #'outline-minor-mode)
  (add-hook 'markdown-mode-hook #'flyspell-mode)
  (add-hook 'markdown-mode-hook #'visual-line-mode)
  (add-hook 'markdown-mode-hook #'whitespace-mode)

  :config
  (my-advise-to-insert-after markdown-insert-gfm-code-block)

  (setq markdown-code-lang-modes
        (-concat '(("postgresql" . sql-mode)
                   ("console" . sh-mode)
                   ("html" . html-mode)
                   ("javascript" . js-mode)
                   ("zsh" . sh-mode))
                 markdown-code-lang-modes))

  (defun my-markdown-insert-named-footnote (name)
    "Insert footnote with a given name and move point to footnote definition."
    (interactive "sFootnote name: ")

    (insert (format "[^%s]" name))
    (markdown-footnote-text-find-new-location)
    (markdown-ensure-blank-line-before)

    (unless (markdown-cur-line-blank-p)
      (insert "\n"))

    (insert (format "[^%s]: " name))
    (markdown-ensure-blank-line-after)))

(use-package yaml-mode
  :defer t

  :init
  (add-hook 'yaml-mode-hook #'turn-off-flyspell t)
  (add-hook 'yaml-mode-hook #'flyspell-prog-mode t))

(use-package ruby-mode
  :straight nil

  :init
  (setq ruby-insert-encoding-magic-comment nil))

(use-package go-mode
  :defer t

  :init
  (setq gofmt-command "goimports")

  (add-hook 'go-mode-hook #'subword-mode)
  (add-hook 'before-save-hook #'gofmt-before-save))

(use-package less-css-mode :defer t)

(use-package scss-mode
  :mode "\\.sass\\'"

  :init
  (defun my--scss-hook ()
    (setq-local comment-end "")
    (setq-local comment-start "//"))

  (add-hook 'scss-mode-hook #'my--scss-hook))

(use-package css-eldoc
  :defer t
  :commands turn-on-css-eldoc

  :init
  (add-hook 'css-mode-hook #'turn-on-css-eldoc)
  (add-hook 'less-css-mode-hook #'turn-on-css-eldoc)
  (add-hook 'scss-mode-hook #'turn-on-css-eldoc))

(use-package vimrc-mode :defer t)

(use-package json-mode
  :mode
  (("\\.eslintrc\\'" . json-mode)
   ("\\.babelrc\\'" . json-mode))

  :init
  (setq json-reformat:indent-width 2)

  (add-hook 'json-mode-hook #'subword-mode))

(use-package modern-cpp-font-lock
  :defer t

  :init
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

(use-package cmake-mode :defer t)

(use-package cmake-font-lock
  :defer t

  :init
  (add-hook 'cmake-mode-hook #'cmake-font-lock-activate))

(use-package rust-mode
  :defer t

  :init
  (defun my--rust-hook ()
    (setq-local compile-command "cargo build")
    (helm-gtags-mode))

  (add-hook 'rust-mode-hook #'my--rust-hook))

(use-package toml-mode :defer t)

(use-package emmet-mode
  :defer t

  :general
  (:keymaps 'emmet-mode-keymap
   "C-S-j" 'my-emmet-preview)

  :init
  (defun my--emmet-jsx ()
    (setq-local emmet-expand-jsx-className? t))

  (add-hook 'rjsx-mode-hook #'my--emmet-jsx)
  (add-hook 'js2-jsx-mode-hook #'my--emmet-jsx)

  (add-hook 'js2-mode-hook #'emmet-mode)
  (add-hook 'sgml-mode-hook #'emmet-mode)
  (add-hook 'css-mode-hook  #'emmet-mode)
  (add-hook 'web-mode-hook  #'emmet-mode)

  :config
  (defun my-emmet-preview ()
    "Preview the emmet expression."
    (interactive)

    (emmet-expand-line '(4))))

(use-package tex-site
  :straight auctex

  :general
  (:keymaps 'LaTeX-mode-map
   "C-c C-c" 'my-latex-make-and-preview)

  (my-map :keymaps 'LaTeX-mode-map
    "m c" 'my-latex-make-and-preview)

  :init
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

  (define-advice TeX-command-sentinel
      (:after (process msg) show-output-window-on-error)
    "Show the output window when there is an error."
    (if (not (eq (process-exit-status process) 0))
        (TeX-recenter-output-buffer nil)))

  (defun my-latex-make-and-preview ()
    "Offer to save the file if there are changes, then build the file with Make."
    (interactive)

    (if (and (buffer-modified-p)
             (y-or-n-p (concat "Save file " buffer-file-name "? ")))
        (save-buffer))

    (defun TeX-process-check (&rest IGNORE)
      (TeX-command "Make" 'TeX-master-file 0)))

  (add-hook 'LaTeX-mode-hook #'olivetti-mode)
  (add-hook 'LaTeX-mode-hook #'flyspell-mode)
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode))

(use-package latex-preview-pane :defer t)

(use-package pkgbuild-mode :defer t)

(use-package clang-format
  :general
  (:keymaps 'c-mode-base-map
   "C-c C-f" 'clang-format-buffer)

  (my-map :keymaps 'c-mode-base-map
    "m f" 'clang-format-buffer))

(use-package google-c-style
  :defer t

  :init
  (add-hook 'c-mode-common-hook #'google-set-c-style))

(use-package cargo
  :defer t

  :init
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

(use-package sqlup-mode :defer t)

(use-package sql-indent
  :defer t
  :straight (:host github :repo "alex-hhh/emacs-sql-indent"))

(use-package graphql-mode :defer t)

(use-package clojure-mode :defer t)

(use-package scala-mode :defer t)

(use-package thrift :defer t)

(use-package protobuf-mode :defer t)

(provide 'conf/languages)
