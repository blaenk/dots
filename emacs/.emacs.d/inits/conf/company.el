(require 'straight)
(require 'use-package)
(require 'general)

(use-package company
  :general-config
  (:keymaps 'company-active-map
   "TAB" 'company-complete-selection
   "<tab>" 'company-complete-selection

   "RET" nil
   "<return>" nil

   "C-SPC" 'company-complete-selection

   "C-w" nil

   "C-o" 'company-show-location
   "C-/" 'company-filter-candidates)

  :init
  (setq company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-tooltip-limit 20
        company-tooltip-align-annotations t
        company-require-match 'never
        company-dabbrev-downcase nil
        company-dabbrev-code-other-buffers 'code
        company-dabbrev-code-everywhere t)

  (add-hook 'after-init-hook #'global-company-mode))

(provide 'conf/company)
