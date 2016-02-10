(require 'use-package)

(use-package flycheck
  :defer t
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq-default flycheck-emacs-lisp-load-path 'inherit)

  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package flycheck-irony
  :defer t
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
