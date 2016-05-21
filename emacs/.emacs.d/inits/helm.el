(require 'use-package)

(use-package helm
  :diminish helm-mode
  :defer t

  :bind
  (("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("M-i" . helm-semantic-or-imenu)
   ("C-c h" . helm-command-prefix)
   ("C-x b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files)
   ("C-h a" . helm-apropos)
   ("C-h i" . helm-info-emacs)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("C-i" . helm-execute-persistent-action)
   ("C-z" . helm-select-action))

  :init
  (setq helm-adaptive-history-file
        (blaenk/cache-dir "helm-adaptive-history"))
  (setq helm-split-window-in-side-p t)
  (setq helm-display-header-line nil)

  :config
  (require 'helm-config)
  (helm-autoresize-mode t)

  ;; open in horizontal split
  (require 'helm-files)

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

  (bind-key "M-h" 'blaenk/helm-horizontal-split helm-find-files-map)
  (bind-key "M-h" 'blaenk/helm-horizontal-split helm-buffer-map)

  (bind-key "M-v" 'blaenk/helm-vertical-split helm-find-files-map)
  (bind-key "M-v" 'blaenk/helm-vertical-split helm-buffer-map)

  (helm-mode 1)

  (use-package helm-mt
    :bind
    (("C-c t" . helm-mt))

    :config
    (bind-key "M-h" 'blaenk/helm-horizontal-split helm-mt/keymap)
    (bind-key "M-v" 'blaenk/helm-vertical-split helm-mt/keymap))

  (use-package helm-open-github)

  (use-package helm-unicode
    :config
    (bind-key [remap insert-char] 'helm-unicode))

  (use-package helm-ag)

  (use-package helm-gtags
    :diminish helm-gtags-mode

    ;; TODO audit
    :init
    (setq
     helm-gtags-ignore-case t
     helm-gtags-auto-update t
     helm-gtags-use-input-at-cursor t
     helm-gtags-pulse-at-cursor t)

    :config
    (helm-gtags-mode))

  (use-package helm-descbinds
    :config
    (helm-descbinds-mode))

  (use-package helm-projectile
    :diminish projectile-mode
    :bind
    (("C-<" . helm-projectile-switch-to-buffer)
     ("C->" . helm-projectile))

    :config
    (bind-key "M-h" 'blaenk/helm-horizontal-split helm-projectile-find-file-map)

    (bind-key "M-v" 'blaenk/helm-vertical-split helm-projectile-find-file-map)

    (helm-projectile-on)

    (defmacro if-projectile (is-projectile is-not)
      `(lambda ()
        (interactive)
        (if (projectile-project-p)
            (,is-projectile)
          (,is-not))))

    ;; (define-key projectile-mode-map (kbd "M-p") 'helm-projectile)

    ;; (with-eval-after-load 'evil-leader
    ;;   (evil-leader/set-key
    ;;     "f" (if-projectile helm-projectile helm-find-files)
    ;;     "b" (if-projectile helm-projectile-switch-to-buffer helm-buffers-list)))
    )

  (with-eval-after-load 'flycheck
    (use-package helm-flycheck))

  (use-package helm-flyspell)

  (use-package persp-projectile
    :disabled t
    :config
    ;; (define-key projectile-command-map
    ;;   (kbd "p") 'projectile-persp-switch-project)
    )

  (use-package helm-make)

  (use-package ace-jump-helm-line
    :bind
    (:map helm-map
          ("C-'" . ace-jump-helm-line))

    :init
    (setq ace-jump-helm-line-default-action 'select)

    ;; press 'o' before the avy anchor to only move to it
    (setq ace-jump-helm-line-move-only-key ?o)

    ;; press 'p' before the avy anchor to move to it and execute
    ;; it's persistent action
    (setq ace-jump-helm-line-persistent-key ?p)))
