(require 'use-package)
(require 'general)

(use-package flycheck
  :general
  (my-map
    "c r" 'flycheck-buffer
    "c l" 'flycheck-list-errors)

  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)
                flycheck-emacs-lisp-load-path 'inherit)

  (defun my-use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))

  (add-hook 'flycheck-mode-hook #'my-use-eslint-from-node-modules)

  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package flycheck-irony
  :after irony

  :init
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package flycheck-rust
  :after rust-mode
  :commands flycheck-rust-setup

  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package lsp-flycheck
  :ensure nil
  :after lsp-mode)

;; Clojure
;; (use-package flycheck-joker)

(provide 'conf/flycheck)
