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

  (with-eval-after-load 'rtags
    (use-package flycheck-rtags
      :ensure nil
      :config
      (defun blaenk/flycheck-rtags-setup  ()
        ;; (flycheck-select-checker 'rtags)
        ;; (setq-local flycheck-highlighting-mode nil)
        ;; (setq-local flycheck-check-syntax-automatically nil)
        )

      (add-hook 'c-mode-common-hook 'blaenk/flycheck-rtags-setup)))

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
