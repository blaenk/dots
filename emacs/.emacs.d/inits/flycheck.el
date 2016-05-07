(require 'use-package)

(use-package flycheck
  :defer t
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq-default flycheck-emacs-lisp-load-path 'inherit)

  (add-hook 'after-init-hook 'global-flycheck-mode)

  (use-package flycheck-irony
    :defer t
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

    :config
    (flycheck-add-next-checker
     'c/c++-clang
     '(warning . c/c++-googlelint))))
