(require 'use-package)

(use-package flycheck
  :defer t
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq-default flycheck-emacs-lisp-load-path 'inherit)

  (add-hook 'after-init-hook 'global-flycheck-mode)

  :config
  (with-eval-after-load 'irony
    (use-package flycheck-irony
      :init
      (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

  (with-eval-after-load 'rust
    (use-package flycheck-rust
      :init
      (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)))

  (use-package flycheck-google-cpplint
    :disabled t
    :init
    (setq flycheck-c/c++-googlelint-executable "cpplint")
    (setq flycheck-googlelint-filter "-legal")

    :config
    (flycheck-add-next-checker
     'irony
     '(warning . c/c++-googlelint))))

(provide 'conf/flycheck)
