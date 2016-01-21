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
   ("C-h i" . helm-info-emacs))

  :init
  (setq helm-adaptive-history-file (blaenk/cache-dir "helm-adaptive-history"))
  (setq helm-quick-update t)
  (setq helm-split-window-in-side-p t)
  (setq helm-display-header-line nil)
  ;; (setq helm-autoresize-max-height 30)
  ;; (setq helm-autoresize-min-height 30)
  (setq helm-imenu-execute-action-at-once-if-one nil)
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)

  :config
  (require 'helm-config)
  (helm-autoresize-mode t)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)

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

  (define-key helm-find-files-map
    (kbd "M-h") 'blaenk/helm-horizontal-split)

  (define-key helm-buffer-map
    (kbd "M-h") 'blaenk/helm-horizontal-split)

  (define-key helm-find-files-map
    (kbd "M-v") 'blaenk/helm-vertical-split)

  (define-key helm-buffer-map
    (kbd "M-v") 'blaenk/helm-vertical-split)

  (helm-mode 1)

  ;; TODO test this
  (use-package helm-dictionary
    :init
    (define-key helm-command-map (kbd "d") 'helm-dictionary)

    ;; arch package `words`
    (setq helm-dictionary-database "/usr/share/dict/words")
    (setq helm-dictionary-online-dicts
          '(("Google" . "https://www.google.com/search?q=define:%s")
            ("Merriam-Webster" . "http://www.merriam-webster.com/dictionary/%s")
            ("en.wiktionary.org" . "http://en.wiktionary.org/wiki/%s"))))

  (use-package helm-mt
    :bind ("C-c t" . helm-mt)

    :config
    (define-key helm-mt/keymap
      (kbd "M-h") 'blaenk/helm-horizontal-split)

    (define-key helm-mt/keymap
      (kbd "M-v") 'blaenk/helm-vertical-split))

  (use-package helm-open-github)

  (use-package helm-unicode
    :config
    (define-key global-map [remap insert-char] 'helm-unicode))

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

    :config
    (define-key helm-projectile-find-file-map
      (kbd "M-h") 'blaenk/helm-horizontal-split)

    (define-key helm-projectile-find-file-map
      (kbd "M-v") 'blaenk/helm-vertical-split)

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

  ;; TODO
  ;; can configure test dirs by configuring projectile-test-prefix etc
  ;; see default implementation
  (use-package projectile
    :init
    (setq projectile-completion-system 'helm)
    (setq projectile-cache-file (blaenk/cache-dir "projectile.cache"))
    (setq projectile-known-projects-file (blaenk/cache-dir "projectile-bookmarks.eld"))

    :config
    (projectile-global-mode))

  (use-package perspective
    :disabled t
    :config
    ;; (persp-mode)
    )

  (use-package persp-projectile
    :disabled t
    :config
    ;; (define-key projectile-command-map
    ;;   (kbd "p") 'projectile-persp-switch-project)
    )

  (use-package helm-make))
