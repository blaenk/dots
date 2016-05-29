(require 'use-package)

(use-package helm
  :diminish helm-mode

  :bind
  (("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("M-i" . helm-semantic-or-imenu)
   ("M-r" . helm-resume)
   ("C-x b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files)
   ("C-x C-r" . helm-recentf)
   ("C-h a" . helm-apropos)
   ("C-h i" . helm-info-emacs))

  :init
  (setq helm-adaptive-history-file
        (blaenk/cache-dir "helm-adaptive-history"))
  (setq helm-split-window-in-side-p t)
  (setq helm-display-header-line nil)
  (setq helm-imenu-execute-action-at-once-if-one nil)

  (use-package helm-config
    :ensure nil
    :init
    (setq helm-command-prefix-key "C-c h"))

  :config
  (bind :keymaps 'helm-map
    "<tab>" 'helm-execute-persistent-action
    "C-i" 'helm-execute-persistent-action
    "C-z" 'helm-select-action)

  (helm-autoresize-mode t)

  ;; open in horizontal split
  (use-package helm-files
    :ensure nil

    :config
    (defun blaenk/helm-action-horizontal-split (candidate)
      "Display buffer in horizontal split"
      ;; Select the bottom right window
      (require 'winner)
      ;; Display buffers in new windows
      (dolist (buf (helm-marked-candidates))
        (select-window (split-window-below))
        (if (get-buffer buf)
            (switch-to-buffer buf)
          (find-file buf)))
      (balance-windows))

    (defun blaenk/helm-action-vertical-split (candidate)
      "Display buffer in vertical split"
      ;; Select the bottom right window
      (require 'winner)
      ;; Display buffers in new windows
      (dolist (buf (helm-marked-candidates))
        (select-window (split-window-right))
        (if (get-buffer buf)
            (switch-to-buffer buf)
          (find-file buf)))
      (balance-windows))

    (add-to-list 'helm-find-files-actions
                 '("Display buffer in horizontal split" .
                   blaenk/helm-action-horizontal-split) t)

    (add-to-list 'helm-type-buffer-actions
                 '("Display buffer in horizontal split" .
                   blaenk/helm-action-horizontal-split) t)

    (add-to-list 'helm-find-files-actions
                 '("Display buffer in vertical split" .
                   blaenk/helm-action-vertical-split) t)

    (add-to-list 'helm-type-buffer-actions
                 '("Display buffer in vertical split" .
                   blaenk/helm-action-vertical-split) t)

    (defun blaenk/helm-horizontal-split ()
      (interactive)
      (with-helm-alive-p
        (helm-exit-and-execute-action 'blaenk/helm-action-horizontal-split)))

    (defun blaenk/helm-vertical-split ()
      (interactive)
      (with-helm-alive-p
        (helm-exit-and-execute-action 'blaenk/helm-action-vertical-split)))

    (bind :keymaps '(helm-find-files-map helm-buffer-map)
      "M-h" 'blaenk/helm-horizontal-split
      "M-v" 'blaenk/helm-vertical-split))

  (helm-mode 1)

  (use-package helm-mt
    :bind
    (("C-c t" . helm-mt))

    :config
    (bind :keymaps 'helm-mt/keymap
      "M-h" 'blaenk/helm-horizontal-split
      "M-v" 'blaenk/helm-vertical-split))

  (use-package helm-open-github
    :defer t)

  (use-package helm-unicode
    :defer t
    :config
    (bind [remap insert-char] 'helm-unicode))

  (use-package helm-ag
    :defer t)

  (use-package helm-gtags
    :diminish helm-gtags-mode
    :defer t

    :init
    (setq helm-gtags-ignore-case t)
    (setq helm-gtags-auto-update t)
    (setq helm-gtags-use-input-at-cursor t)
    (setq helm-gtags-direct-helm-completing t)
    (setq helm-gtags-prefix-key "\C-t")
    (setq helm-gtags-suggested-key-mapping t)

    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)

    :config
    (bind :keymaps 'helm-gtags-mode-map
      "M-." 'helm-gtags-dwim
      "C-M-." 'helm-gtags-select

      "M-," 'helm-gtags-pop-stack
      "C-M-," 'helm-gtags-show-stack

      "C-S-h" 'helm-gtags-previous-history
      "C-S-l" 'helm-gtags-next-history)

    (with-eval-after-load 'evil
      (evil-make-overriding-map helm-gtags-mode-map)
      (add-hook 'helm-gtags-mode-hook #'evil-normalize-keymaps)))

  (use-package helm-descbinds
    :config
    (helm-descbinds-mode))

  (use-package helm-swoop
    :defer t
    :init
    (setq helm-swoop-speed-or-color t))

  (use-package helm-projectile
    :diminish projectile-mode
    :bind
    (("C-<" . helm-projectile-switch-to-buffer)
     ("C->" . helm-projectile))

    :config
    (bind :keymaps 'helm-projectile-find-file-map
      "M-h" 'blaenk/helm-horizontal-split
      "M-v" 'blaenk/helm-vertical-split)

    (helm-projectile-on))

  (with-eval-after-load 'flycheck
    (use-package helm-flycheck
      :defer t
      :init
      (bind :keymaps 'flycheck-mode-map
        "C-c ! h" 'helm-flycheck)))

  (use-package helm-flyspell
    :defer t)

  (use-package helm-describe-modes
    :init
    (bind [remap describe-mode] 'helm-describe-modes))

  (use-package persp-projectile
    :disabled t
    :config
    (bind :keymaps 'projectile-command-map
      "p" 'projectile-persp-switch-project))

  (use-package helm-make
    :defer t)

  (use-package helm-company
    :defer t
    :init
    (with-eval-after-load 'company
      (bind :keymaps 'company-active-map
        "M-/" 'helm-company)))

  (use-package helm-css-scss
    :defer t
    :init
    ;; TODO
    ;; do local key bind
    (setq helm-css-scss-split-direction 'split-window-horizontally))

  (use-package ace-jump-helm-line
    :defer t

    :init
    (setq ace-jump-helm-line-default-action 'select)

    ;; press 'o' before the avy anchor to only move to it
    (setq ace-jump-helm-line-move-only-key ?o)

    ;; press 'p' before the avy anchor to move to it and execute
    ;; it's persistent action
    (setq ace-jump-helm-line-persistent-key ?p)

    (bind :keymaps 'helm-map
      "C-'" 'ace-jump-helm-line)))

(provide 'conf/helm)
