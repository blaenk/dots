(require 'straight)
(require 'use-package)
(require 'general)

(use-package flycheck
  :general
  (my-map
    "c r" 'flycheck-buffer
    "c l" 'flycheck-list-errors)

  :init
  (setq flycheck-rubocop-lint-only t)

  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)
                flycheck-emacs-lisp-load-path 'inherit)

  (defun my--flycheck-error-list-hook ()
    (select-window (get-buffer-window flycheck-error-list-buffer)))

  (add-hook 'flycheck-error-list-after-refresh-hook #'my--flycheck-error-list-hook)

  (defun my--use-eslint-from-node-modules ()
    (setq-local flycheck-javascript-eslint-executable
                (my--use-node-modules-binary "eslint")))

  (add-hook 'flycheck-mode-hook #'my--use-eslint-from-node-modules)

  (add-hook 'after-init-hook #'global-flycheck-mode)

  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state #'flycheck-error-list-mode 'emacs)))

(use-package flycheck-irony
  :after irony

  :init
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package flycheck-clang-analyzer
  :after irony

  :init
  (add-hook 'flycheck-mode-hook #'flycheck-clang-analyzer-setup))

(use-package flycheck-rust
  :after rust-mode
  :commands flycheck-rust-setup

  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package flycheck-golangci-lint
  :init
  (setq flycheck-golangci-lint-tests t
        flycheck-golangci-lint-fast t)

  :hook (go-mode . flycheck-golangci-lint-setup))

(use-package lsp-flycheck
  :straight nil
  :after lsp-mode)

(provide 'conf/flycheck)
