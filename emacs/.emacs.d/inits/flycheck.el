(require 'use-package)

(use-package flycheck
  :defer t
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package flycheck-irony
  :defer t
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
