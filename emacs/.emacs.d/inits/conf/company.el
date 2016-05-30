(require 'use-package)
(require 'general)

(use-package company
  :defer t

  :general
  ;; get back the use of kill word even if company is active
  (:keymaps 'company-active-map
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

  ;; (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'after-init-hook 'global-company-mode)

  (bind :keymaps 'company-active-map
    "C-w" nil)

  :config
  (use-package company-statistics
    :init
    (setq company-statistics-file
          (blaenk/cache-dir "company-statistics-cache.el"))

    (add-hook 'after-init-hook 'company-statistics-mode))

  (use-package company-quickhelp
    :init
    (setq company-quickhelp-delay nil)

    :general
    (:keymaps 'company-active-map
      "M-h" 'company-quickhelp-manual-begin)

    :config
    (company-quickhelp-mode 1))

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

  (use-package robe
    :defer t
    :no-require t
    :config
    (add-to-list 'company-backends 'company-robe t))

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

(provide 'conf/company)
