(require 'use-package)
(require 'general)

(use-package browse-at-remote
  :defer t

  :general
  (bind* "o g" 'browse-at-remote/kill))

(use-package git-messenger
  :defer t)

(use-package git-timemachine
  :defer t)

(use-package git-link
  :defer t)

(use-package github-clone
  :defer t)

(use-package magit
  :diminish
  (magit-wip-after-save-local-mode
   magit-wip-before-change-mode)

  :general
  (bind*
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
  (defun my-pull-request-url ()
    "Build the URL or the pull requestion on GitHub corresponding
to the current branch. Uses Magit."
    (interactive)
    (format "%s/compare/%s"
            (replace-regexp-in-string
             (rx (and
                  string-start
                  (1+ any)
                  "github.com:"
                  (group (1+ any))
                  ".git"
                  string-end))
             "https://github.com/\\1"
             (magit-get "remote" (magit-get-remote) "url"))
            (magit-get-current-branch)))

  (defun my-open-pr ()
    (interactive)
    (browse-url (my-pull-request-url)))

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
