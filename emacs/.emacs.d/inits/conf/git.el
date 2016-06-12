(require 'use-package)
(require 'general)

(use-package browse-at-remote
  :defer t

  :general
  (my-map "o g" 'browse-at-remote/kill)

  :init
  (setq browse-at-remote/prefer-symbolic nil))

(use-package git-messenger
  :defer t)

(use-package git-timemachine
  :defer t)

(use-package github-clone
  :defer t)

(use-package magit
  :diminish
  (magit-wip-after-save-local-mode
   magit-wip-before-change-mode)

  :general
  (my-map
    "g" '(:ignore t :which-key "git")
    "g s" 'magit-status
    "g p" 'magit-dispatch-popup
    "g f" 'magit-file-popup)

  :init
  (setq magit-save-repository-buffers 'dontask
        magit-refs-show-commit-count 'all
        magit-log-auto-more t
        magit-display-buffer-function
          #'magit-display-buffer-fullframe-status-v1)

  :config
  (defun my-magit-hunk-recenter-top ()
    (when (and (memq this-command '(magit-stage magit-unstage))
               (magit-section-when '((hunk file)) t)
               ;; TODO also check if previously at beg of window.
               ;; using information saved before the refresh
               )
      (recenter (min (max 0 scroll-margin)
                     (truncate (/ (window-body-height) 4.0))))
      ))

  (add-hook 'magit-refresh-buffer-hook 'my-magit-hunk-recenter-top)

  (defun my-open-pr ()
    "Visit the current branch's PR on Github."
    (interactive)
    (browse-url
     (format "https://github.com/%s/pull/new/%s"
             (replace-regexp-in-string
              "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
              (magit-get "remote"
                         (magit-get-push-remote)
                         "url"))
             (magit-get-current-branch))))

  (with-eval-after-load 'magit-ediff
    (add-hook 'magit-ediff-quit-hook #'my-ediff-quit))

  (magit-wip-after-save-mode)
  (magit-wip-after-apply-mode)
  (magit-wip-before-change-mode)

  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell)
  (add-hook 'git-commit-setup-hook #'fci-mode))

(use-package magit-gh-pulls
  :after magit

  :config
  (magit-define-popup-action 'magit-dispatch-popup
    ?# "Pull requests" 'magit-gh-pulls-popup ?!)

  (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls))

(provide 'conf/git)
