(require 'use-package)
(require 'general)

(use-package flycheck
  :demand t

  :general
  (my-map
    "c l" 'flycheck-list-errors
    "c c" 'helm-flycheck)

  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)
                flycheck-emacs-lisp-load-path 'inherit)

  :config
  (global-flycheck-mode))

(with-eval-after-load 'irony
  (use-package flycheck-irony
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

(with-eval-after-load 'rust
  (use-package flycheck-rust
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package flycheck-google-cpplint
  :disabled t

  :init
  (setq flycheck-c/c++-googlelint-executable "cpplint"
        flycheck-googlelint-filter "-legal")

  :config
  (flycheck-add-next-checker
   'irony
   '(warning . c/c++-googlelint)))

(provide 'conf/flycheck)
