(require 'use-package)
(require 'general)

(use-package helm-config
  :ensure nil)

(use-package helm
  :diminish helm-mode

  :general
  (my-map
    "h" '(:keymap helm-command-map :which-key "helm")
    "o a f" 'helm-find-files
    "o a b" 'helm-buffers-list
    "o r" 'helm-recentf)

  ("M-x" 'helm-M-x
   "M-y" 'helm-show-kill-ring
   "M-i" 'helm-semantic-or-imenu
   "M-r" 'helm-resume

   "C-x b" 'helm-buffers-list
   "C-x C-f" 'helm-find-files
   "C-x C-r" 'helm-recentf

   "C-h a" 'helm-apropos
   "C-h i" 'helm-info-emacs)

  (:keymaps 'helm-map
   "<tab>" 'helm-execute-persistent-action
   "C-i" 'helm-execute-persistent-action
   "C-z" 'helm-select-action)

  :init
  (setq helm-split-window-in-side-p t
        helm-display-header-line nil)

  :config
  (helm-mode 1)

  (helm-autoresize-mode t))

(use-package helm-imenu
  :ensure nil

  :init
  (setq helm-imenu-execute-action-at-once-if-one nil))

(use-package helm-adaptive
  :ensure nil

  :init
  (setq helm-adaptive-history-file
        (my-cache-dir "helm-adaptive-history")))

(use-package helm-files
  :ensure nil

  :general
  (:keymaps '(helm-find-files-map helm-buffer-map)
   "M-h" 'my-helm-horizontal-split
   "M-v" 'my-helm-vertical-split)

  :config
  (defun my-helm-action-horizontal-split (candidate)
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

  (defun my-helm-action-vertical-split (candidate)
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
                 my-helm-action-horizontal-split) t)

  (add-to-list 'helm-type-buffer-actions
               '("Display buffer in horizontal split" .
                 my-helm-action-horizontal-split) t)

  (add-to-list 'helm-find-files-actions
               '("Display buffer in vertical split" .
                 my-helm-action-vertical-split) t)

  (add-to-list 'helm-type-buffer-actions
               '("Display buffer in vertical split" .
                 my-helm-action-vertical-split) t)

  (defun my-helm-horizontal-split ()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'my-helm-action-horizontal-split)))

  (defun my-helm-vertical-split ()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action 'my-helm-action-vertical-split))))

(use-package helm-mt
  :general
  (my-map
    "o t" 'helm-mt)

  (:keymaps 'helm-mt/keymap
   "M-h" 'my-helm-horizontal-split
   "M-v" 'my-helm-vertical-split)

  :config
  (helm-mt/wrap-shells t))

(use-package helm-open-github
  :defer t)

(use-package helm-unicode
  :general
  ([remap insert-char] 'helm-unicode))

(use-package helm-describe-modes
  :defer t)

(use-package helm-ag
  :defer t)

(use-package helm-gtags
  :diminish helm-gtags-mode

  :general
  (:keymaps 'helm-gtags-mode-map
   "M-." 'helm-gtags-dwim
   "C-M-." 'helm-gtags-select

   "M-," 'helm-gtags-pop-stack
   "C-M-," 'helm-gtags-show-stack

   "C-S-h" 'helm-gtags-previous-history
   "C-S-l" 'helm-gtags-next-history)

  :init
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-direct-helm-completing t
        helm-gtags-prefix-key "\C-t"
        helm-gtags-suggested-key-mapping t)

  (add-hook 'c-mode-hook #'helm-gtags-mode)
  (add-hook 'c++-mode-hook #'helm-gtags-mode)

  :config
  (with-eval-after-load 'evil
    (evil-make-overriding-map helm-gtags-mode-map)
    (add-hook 'helm-gtags-mode-hook #'evil-normalize-keymaps)))

(use-package helm-descbinds
  :config
  (helm-descbinds-mode))

(use-package helm-projectile
  :diminish projectile-mode

  :general
  ("C-<" 'my-open-buffer
   "C->" 'my-open-file)

  (:keymaps '(helm-projectile-find-file-map helm-projectile-projects-map)
   "M-h" 'my-helm-horizontal-split
   "M-v" 'my-helm-vertical-split)

  (my-map
    "o b" 'my-open-buffer
    "o f" 'my-open-file
    "e e" 'my-edit-inits)

  :config
  (defun my-edit-inits ()
    (interactive)
    (let ((default-directory "~/.dots"))
      (helm-projectile-find-file)))

  (defun my-open-file ()
    (interactive)
    (if (projectile-project-p)
        (helm-projectile)
      (call-interactively #'helm-find-files)))

  (defun my-open-buffer ()
    (interactive)
    (if (projectile-project-p)
        (helm-projectile-switch-to-buffer)
      (helm-buffers-list)))

  (helm-projectile-on))

(use-package helm-flycheck
  :general
  (:keymaps 'flycheck-mode-map
   "C-c ! h" 'helm-flycheck))

(use-package helm-flyspell
  :general
  (:keymaps 'normal
   "C-;" 'my-flyspell-last

   "[ s" 'flyspell-goto-previous-error
   "] s" 'flyspell-goto-next-error

   "[ S" 'check-previous-spelling-error
   "] S" 'check-next-spelling-error

   "z =" 'helm-flyspell-correct)

  ;; https://github.com/pronobis/helm-flyspell/issues/6
  :init
  (require 'cl)

  :config
  (defun my-flyspell-last ()
    (interactive)
    (save-excursion
      (check-previous-spelling-error)))

  (defun push-mark-no-activate ()
    "Pushes `point' to `mark-ring' and does not activate the region
 Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
    (interactive)
    (push-mark (point) t nil)
    (message "Pushed mark to ring"))

  (defun check-previous-spelling-error ()
    "Jump to previous spelling error and correct it"
    (interactive)
    (push-mark-no-activate)
    (flyspell-goto-previous-error 1)
    (call-interactively #'helm-flyspell-correct))

  (defun check-next-spelling-error ()
    "Jump to next spelling error and correct it"
    (interactive)
    (push-mark-no-activate)
    (flyspell-goto-next-error)
    (call-interactively #'helm-flyspell-correct))
  )

(use-package persp-projectile
  :disabled t

  :general
  (:keymaps 'projectile-command-map
   "p" 'projectile-persp-switch-project))

(use-package helm-make
  :defer t)

(use-package helm-company
  :general
  (:keymaps 'company-active-map
   "M-/" 'helm-company))

(use-package helm-css-scss
  :defer t

  :init
  ;; TODO
  ;; do local key bind
  (setq helm-css-scss-split-direction 'split-window-horizontally))

(use-package ace-jump-helm-line
  :general
  (:keymaps 'helm-map
   "C-'" 'ace-jump-helm-line)

  :init
  (setq ace-jump-helm-line-default-action 'select
        ace-jump-helm-line-style 'post

        ;; press 'o' before the avy anchor to only move to it
        ace-jump-helm-line-move-only-key ?o

        ;; press 'p' before the avy anchor to move to it and execute
        ;; it's persistent action
        ace-jump-helm-line-persistent-key ?p))

(provide 'conf/helm)
