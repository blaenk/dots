(require 'use-package)
(require 'general)

(use-package company
  :general
  ;; get back the use of kill word even if company is active
  (:keymaps 'company-active-map
    "C-w" nil

    "C-o" 'company-show-location
    "C-/" 'company-filter-candidates)

  :init
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-limit 20)
  (setq company-tooltip-align-annotations t)
  (setq company-require-match 'never)
  ;; (setq company-global-modes '(not git-commit-mode))
  ;; (setq company-dabbrev-downcase nil)
  ;; (setq company-dabbrev-ignore-case t)

  :config
  (global-company-mode)

  (use-package company-statistics
    :init
    (setq company-statistics-file
          (my-cache-dir "company-statistics-cache.el"))

    :config
    (company-statistics-mode))

  (use-package company-quickhelp
    :general
    (:keymaps 'company-active-map
      "M-h" 'company-quickhelp-manual-begin)

    :init
    (setq company-quickhelp-delay nil)

    :config
    (company-quickhelp-mode 1))

  (use-package company-irony
    :config
    (use-package company-irony-c-headers
      :config
      (defun my-company-irony ()
        (set (make-local-variable 'company-backends)
             (add-to-list 'company-backends
                          '(company-irony-c-headers company-irony))))

      (add-hook 'irony-mode-hook #'my-company-irony)))

  (use-package company-math
    :config
    (add-to-list 'company-math-allow-latex-symbols-in-faces 'markdown-math-face)
    (add-to-list 'company-backends 'company-math-symbols-unicode t)
    (add-to-list 'company-backends 'company-math-symbols-latex t))

  (use-package company-tern
    :config
    (defun my-company-tern ()
      (set (make-local-variable 'company-backends)
           (add-to-list 'company-backends 'company-tern)))

    (add-hook 'tern-mode-hook #'my-company-tern))

  (use-package robe
    :defer t
    :config
    (defun my-company-robe ()
      (set (make-local-variable 'company-backends)
           (add-to-list 'company-backends 'company-robe)))

    (add-hook 'robe-mode-hook #'my-company-robe))

  (use-package company-web
    :config
    (require 'company-web-html)
    (defun my-company-web-html ()
      (set (make-local-variable 'company-backends)
           (add-to-list 'company-backends 'company-web-html)))

    (add-hook 'web-mode-hook #'my-company-web-html))

  (use-package company-lua
    :config
    (defun my-company-lua ()
      (set (make-local-variable 'company-backends)
           (add-to-list 'company-backends 'company-lua)))

    (add-hook 'lua-mode-hook #'my-company-lua))

  (use-package company-auctex
    :config
    (defun my-company-auctex ()
      (make-local-variable 'company-backends)
      (company-auctex-init))

    (add-hook 'LaTeX-mode-hook #'my-company-auctex))

  (use-package company-go
    :defer t

    :init
    (defun my-company-go ()
      (set (make-local-variable 'company-backends)
           (add-to-list 'company-backends 'company-go)))

    (add-hook 'go-mode-hook #'my-company-go))

  (use-package company-restclient
    :config
    (add-to-list 'company-backends 'company-restclient t))

  (use-package company-anaconda
    :config
    (defun my-company-anaconda ()
      (set (make-local-variable 'company-backends)
           (add-to-list 'company-backends 'company-anaconda)))

    (add-hook 'anaconda-mode-hook #'my-company-anaconda))

  (use-package company-cabal
    :config
    (defun my-company-cabal ()
      (set (make-local-variable 'company-backends)
           (add-to-list 'company-backends 'company-cabal)))

    (add-hook 'haskell-cabal-mode-hook #'my-company-cabal))

  (use-package company-emoji
    :config
    (add-to-list 'company-backends 'company-emoji t)))

(provide 'conf/company)
