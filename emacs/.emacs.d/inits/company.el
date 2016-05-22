(require 'use-package)

(use-package company
  :defer t
  :init
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-limit 20)
  (setq company-tooltip-align-annotations t)
  ;; (setq company-require-match 'never)
  ;; (setq company-global-modes '(not git-commit-mode))
  ;; (setq company-dabbrev-downcase nil)
  ;; (setq company-dabbrev-ignore-case t)

  ;; (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'after-init-hook 'global-company-mode)

  :config
  ;; get back the use of kill word even if company is active
  (unbind-key "C-w" company-active-map)
  (bind-key "C-o" 'company-show-location company-active-map)
  (bind-key "C-/" 'company-filter-candidates company-active-map)

  (use-package company-statistics
    :init
    (setq company-statistics-file
          (blaenk/cache-dir "company-statistics-cache.el"))

    :config
    (add-hook 'after-init-hook 'company-statistics-mode)
    (company-statistics-mode))

  (use-package company-quickhelp
    :init
    (setq company-quickhelp-delay nil)

    :config
    (company-quickhelp-mode 1)
    (bind-key "M-h" 'company-quickhelp-manual-begin company-active-map))

  (use-package company-irony
    :config
    (use-package company-irony-c-headers
      :config
      (use-package company-rtags
        :ensure nil
        :config
        (add-to-list 'company-backends
                     '(company-irony-c-headers company-irony company-rtags) t))))

  (use-package company-math
    :init
    ;; (setq company-math-allow-unicode-symbols-in-faces nil)

    :config
    (add-to-list 'company-math-allow-latex-symbols-in-faces 'markdown-math-face)
    (add-to-list 'company-backends 'company-math-symbols-unicode t)
    (add-to-list 'company-backends 'company-math-symbols-latex t))

  (use-package company-tern
    :config
    (add-to-list 'company-backends 'company-tern t))

  (use-package company-web
    :config
    (require 'company-web-html)
    (add-to-list 'company-backends 'company-web-html))

  (use-package company-lua
    :config
    (add-to-list 'company-backends 'company-lua t))

  (use-package company-auctex
    :config
    (company-auctex-init))

  (use-package robe
    :ensure nil
    :config
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-robe t)))

  (use-package company-go
    :config
    (defun blaenk/company-go ()
      (set (make-local-variable 'company-backends)
           ;; add it to the beginning
           (add-to-list 'company-backends 'company-go))
      (company-mode))

    (add-hook 'go-mode-hook 'blaenk/company-go))

  (use-package company-restclient
    :config
    (add-to-list 'company-backends 'company-restclient t))

  (use-package company-anaconda
    :config
    (add-to-list 'company-backends 'company-anaconda t))

  (use-package company-cabal
    :config
    (add-to-list 'company-backends 'company-cabal t))

  (use-package company-emoji
    :config
    (add-to-list 'company-backends 'company-emoji t)))
