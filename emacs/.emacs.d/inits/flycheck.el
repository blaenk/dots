(require 'use-package)

(use-package flycheck
  :preface
  ;; (defun blaenk/flycheck-cargo-rust-predicate () (flycheck-buffer-saved-p))

  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)

;;   (flycheck-define-checker blaenk/cargo-rust
;;     "A Rust syntax checker using cargo rustc.
;; This syntax checker needs Rust 1.1 or newer.
;; See URL `http://www.rust-lang.org'."
;;     :command ("cargo" "rustc" "--" "-Z" "no-trans")
;;     :error-patterns
;;     ((error line-start (file-name) ":" line ":" column ": "
;;             (one-or-more digit) ":" (one-or-more digit) " error: "
;;             (or
;;              ;; Multiline errors
;;              (and (message (minimal-match (one-or-more anything)))
;;                   " [" (id "E" (one-or-more digit)) "]")
;;              (message))
;;             line-end)
;;      (warning line-start (file-name) ":" line ":" column ": "
;;               (one-or-more digit) ":" (one-or-more digit) " warning: "
;;               (message) line-end)
;;      (info line-start (file-name) ":" line ":" column ": "
;;            (one-or-more digit) ":" (one-or-more digit) " " (or "note" "help") ": "
;;            (message) line-end))
;;     :modes rust-mode
;;     :predicate blaenk/flycheck-cargo-rust-predicate)

  ;; (add-to-list 'flycheck-checkers 'blaenk/cargo-rust)
  )

(use-package flycheck-rust
  :disabled t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package flycheck-irony
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
