(require 'use-package)
(require 'general)

(use-package gist
  :general
  (my-map :infix "g g"
    "" '(:ignore t :which-key "gist")
    "s" 'gist-region-or-buffer-private
    "p" 'gist-region-or-buffer
    "l" 'gist-list)

  :init
  (setq gist-ask-for-description t
        gist-list-format
        '((created "Created" 15 nil "%D %R")
          (visibility "Visibility" 8 nil
                      (lambda (public)
                        (or (and public "public")
                            "private")))
          (files "Files" 0 nil
                 (lambda (names)
                   (mapconcat 'identity names ", "))))))

(use-package git-commit-insert-issue
  :defer t

  :init
  (add-hook 'git-commit-mode-hook 'git-commit-insert-issue-mode))

(use-package bug-reference-github
  :defer t

  :init
  (add-hook 'find-file-hook #'bug-reference-github-set-url-format))

(use-package browse-at-remote
  :general
  (my-map "o g" 'browse-at-remote-kill)

  :init
  (setq browse-at-remote-prefer-symbolic nil))

(use-package git-timemachine :defer t)

(use-package github-clone :defer t)

(use-package git-commit
  :defer t

  :init
  (setq git-commit-summary-max-length 50
        git-commit-fill-column 72)

  :config
  (add-to-list 'git-commit-style-convention-checks 'overlong-summary-line))

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

  (add-hook 'magit-mode-hook 'global-magit-file-mode)

  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell)
  (add-hook 'git-commit-setup-hook #'fci-mode))

(use-package magithub
  :after magit

  :init
  (setq magithub-dir (my-cache-dir "magithub"))

  :config
  (magithub-feature-autoinject t))

(provide 'conf/git)
