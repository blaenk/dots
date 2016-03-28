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
  (unbind-key "C-w" company-active-map)

  (use-package company-anaconda
    :config
    (add-to-list 'company-backends 'company-anaconda))

  (use-package company-tern
    :config
    (add-to-list 'company-backends 'company-tern))

  (use-package company-cabal
    :config
    (add-to-list 'company-backends 'company-cabal))

  (use-package company-statistics
    :init
    (setq company-statistics-file (blaenk/cache-dir "company-statistics-cache.el"))
    :config
    (add-hook 'after-init-hook 'company-statistics-mode)
    (company-statistics-mode))

  (use-package company-quickhelp
    :init
    (setq company-quickhelp-delay nil)

    :config
    (company-quickhelp-mode 1))

  (use-package company-web)

  (use-package company-rtags
    :ensure nil
    :config
    (add-to-list 'company-backends 'company-rtags))

  (use-package company-irony
    :config
    ;; (add-to-list 'company-backends 'company-irony)
    )

  (use-package company-irony-c-headers
    :config
    (add-to-list 'company-backends '(company-irony-c-headers company-irony)))

  (use-package company-go
    :config
    (defun blaenk/company-go ()
      (set (make-local-variable 'company-backends) '(company-go))
      (company-mode))

    (add-hook 'go-mode-hook 'blaenk/company-go))

  (use-package company-restclient
    :config
    (add-to-list 'company-backends 'company-restclient))

  (use-package company-emoji
    :disabled t
    :config
    (add-to-list 'company-backends 'company-emoji)))
