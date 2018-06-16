(require 'straight)
(require 'use-package)
(require 'general)

(use-package company
  :general
  (:keymaps 'company-active-map
   "TAB" 'company-complete-selection
   "<tab>" 'company-complete-selection

   "RET" nil
   "<return>" nil

   "C-SPC" 'company-complete-selection

   "C-w" nil

   "C-o" 'company-show-location
   "C-/" 'company-filter-candidates)

  (:keymaps 'yas-minor-mode-map
   "M-N" 'my-company-yasnippet)

  :init
  (setq company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-tooltip-limit 20
        company-tooltip-align-annotations t
        company-require-match 'never
        company-dabbrev-downcase nil
        company-dabbrev-code-other-buffers 'code
        company-dabbrev-code-everywhere t)

  (defun my-company-yasnippet ()
    "Expand partial snippet or choose a snippet with company.

If a region is active, it'll be used to \"wrap\" the selection."
    (interactive)

    ;; If there region is active or there's nothing to expand, use completing
    ;; read to select the snippet. Otherwise expand.
    (if (or (region-active-p)
            (not (yas--maybe-expand-key-filter 'yas-expand)))
        (call-interactively #'company-yasnippet)
      (yas-expand)))

  (add-hook 'after-init-hook #'global-company-mode))

(use-package company-try-hard
  :general
  (:keymaps 'company-mode-map
   "C-S-n" 'company-try-hard)

  (:keymaps 'company-active-map
   "C-S-n" 'company-try-hard))

(use-package company-statistics
  :after company

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

(use-package company-irony :defer t)

(use-package company-irony-c-headers
  :defer t

  :init
  (defun my--company-irony ()
    (setq-local company-backends
                (add-to-list 'company-backends '(company-irony-c-headers company-irony))))

  (add-hook 'irony-mode-hook #'my--company-irony))

(use-package company-math
  :defer t

  :init
  (defun my--company-math ()
    (add-to-list 'company-backends 'company-math-symbols-unicode t)
    (add-to-list 'company-backends 'company-math-symbols-latex t))

  (add-hook 'global-company-mode-hook #'my--company-math)

  :config
  (add-to-list 'company-math-allow-latex-symbols-in-faces #'markdown-math-face))

(use-package robe
  :straight nil
  :after robe

  :init
  (defun my--company-robe ()
    (setq-local company-backends
                (add-to-list 'company-backends 'company-robe)))

  (add-hook 'robe-mode-hook #'my--company-robe))

(use-package company-tern
  :defer t

  :init
  (defun my--company-tern ()
    (setq-local company-backends
                (add-to-list 'company-backends 'company-tern)))

  (add-hook 'tern-mode-hook #'my--company-tern))

(use-package company-web-html
  :straight company-web
  :defer t

  :config
  (defun my--company-web-html ()
    (setq-local company-backends
                (add-to-list 'company-backends 'company-web-html)))

  (add-hook 'web-mode-hook #'my--company-web-html))

(use-package company-auctex
  :defer t

  :init
  (defun my--company-auctex ()
    (make-local-variable 'company-backends)
    (company-auctex-init))

  (add-hook 'LaTeX-mode-hook #'my--company-auctex))

(use-package company-go
  :defer t

  :init
  (defun my--company-go ()
    (setq-local company-backends
                (add-to-list 'company-backends 'company-go)))

  (add-hook 'go-mode-hook #'my--company-go))

(use-package company-restclient
  :defer t

  :init
  (defun my--company-restclient ()
    (setq-local company-backends
                (add-to-list 'company-backends 'company-restclient)))

  (add-hook 'restclient-mode-hook #'my--company-restclient))

(use-package company-emoji
  :defer t

  :init
  (defun my--company-emoji ()
    (add-to-list 'company-backends 'company-emoji t))

  (add-hook 'global-company-mode-hook #'my--company-emoji))

(provide 'conf/company)
