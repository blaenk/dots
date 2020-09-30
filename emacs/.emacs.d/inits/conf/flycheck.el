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

  (add-hook 'after-init-hook #'global-flycheck-mode)

  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state #'flycheck-error-list-mode 'emacs)))

(provide 'conf/flycheck)
