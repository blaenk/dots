(require 'use-package)

(use-package flycheck
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq-default flycheck-emacs-lisp-load-path 'inherit)

  (add-hook 'after-init-hook 'global-flycheck-mode)

  :config
  (use-package flycheck-irony
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

  (use-package flycheck-rtags
    :ensure nil
    :config
    (defun blaenk/flycheck-rtags-setup  ()
      ;; (flycheck-select-checker 'rtags)
      ;; (setq-local flycheck-highlighting-mode nil)
      ;; (setq-local flycheck-check-syntax-automatically nil)
      )

    (add-hook 'c-mode-common-hook 'blaenk/flycheck-rtags-setup))

  (use-package flycheck-google-cpplint
    :init
    (setq flycheck-c/c++-googlelint-executable "cpplint")
    (setq flycheck-googlelint-filter "-legal")

    :config
    (flycheck-add-next-checker
     'irony
     '(warning . c/c++-googlelint))))
