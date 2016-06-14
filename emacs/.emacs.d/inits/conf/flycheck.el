(require 'use-package)
(require 'general)

(use-package flycheck
  :general
  (my-map
    "c" '(:ignore t :which-key "check")
    "c l" 'flycheck-list-errors
    "c c" 'helm-flycheck)

  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)
                flycheck-emacs-lisp-load-path 'inherit)

  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package flycheck-irony
  :after irony

  :init
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package flycheck-rust
  :after rust

  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package flycheck-google-cpplint
  :after irony

  :init
  (setq flycheck-c/c++-googlelint-executable "cpplint"
        flycheck-googlelint-filter "-legal")

  :config
  (flycheck-add-next-checker
   'irony
   '(warning . c/c++-googlelint)))

(provide 'conf/flycheck)
