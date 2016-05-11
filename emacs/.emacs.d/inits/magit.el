(require 'use-package)

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

  :bind
  (("C-c g s" . magit-status)
   ("C-c g p" . magit-dispatch-popup))

  :init
  (setq magit-save-repository-buffers 'dontask)
  (setq magit-refs-show-commit-count 'all)
  (setq magit-log-auto-more t)

  :config
  (defun blaenk/pull-request-url ()
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

  (defun blaenk/open-pr ()
    (interactive)
    (browse-url (blaenk/pull-request-url)))

  (defun blaenk/magit-display-buffer (buffer)
    (if magit-display-buffer-noselect
        (magit-display-buffer-traditional buffer)
      (progn
        (magit-save-window-configuration)
        (delete-other-windows)
        (set-window-dedicated-p nil nil)
        (set-window-buffer nil buffer)
        (get-buffer-window buffer))))

  (setq magit-display-buffer-function 'blaenk/magit-display-buffer)

  (defun blaenk/magit-bury-buffer (buffer)
    (magit-restore-window-configuration buffer))

  (setq magit-bury-buffer-function 'blaenk/magit-bury-buffer)

  (with-eval-after-load 'magit-ediff
    (add-hook 'magit-ediff-quit-hook 'blaenk/ediff-quit))

  ;; NOTE remove if perf hit
  (magit-wip-after-save-mode)
  (magit-wip-after-apply-mode)
  (magit-wip-before-change-mode)

  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
  (add-hook 'git-commit-setup-hook 'fci-mode)

  (use-package magit-gh-pulls
    :init
    (magit-define-popup-action 'magit-dispatch-popup
      ?# "Pull requests" 'magit-gh-pulls-popup ?!)

    (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)))
