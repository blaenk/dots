(require 'use-package)

(use-package company
  :defer t
  :init
  (setq company-idle-delay nil
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-require-match 'never
        company-global-modes '(not git-commit-mode)
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t)

  (add-hook 'prog-mode-hook 'company-mode)

  :config
  ;; get back the use of kill word even if company is active
  (define-key company-active-map (kbd "C-w") nil))

(use-package company-anaconda
  :after company
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package company-tern
  :after company
  :config
  (add-to-list 'company-backends 'company-tern))

(use-package company-cabal
  :after company
  :config
  (add-to-list 'company-backends 'company-cabal))

(use-package company-statistics
  :after company
  :init
  (setq company-statistics-file (blaenk/cache-dir "company-statistics-cache.el"))
  :config
  (add-hook 'after-init-hook 'company-statistics-mode)
  (company-statistics-mode))

(use-package company-quickhelp
  :after company
  :init
  (setq company-quickhelp-delay nil)

  :config
  (company-quickhelp-mode 1))

(use-package company-web
  :after company)

(use-package company-irony
  :after company
  :config
  (add-to-list 'company-backends 'company-irony)

  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))

(use-package company-go
  :after company
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) '(company-go))
              (company-mode))))

(use-package company-restclient
  :after company
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package company-emoji
  :after company
  :config
  (add-to-list 'company-backends 'company-emoji))
