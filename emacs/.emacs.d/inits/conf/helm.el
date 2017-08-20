(require 'use-package)
(require 'general)
(require 'conf/common)

(use-package helm
  :diminish helm-mode

  :general
  ([remap execute-extended-command] 'helm-M-x
   "M-y" 'helm-show-kill-ring
   "M-i" 'helm-semantic-or-imenu
   "M-I" 'helm-imenu-in-all-buffers
   "M-r" 'helm-resume

   "M-/" 'helm-occur

   "C-x b" 'helm-buffers-list
   "C-x C-f" 'helm-find-files
   "C-x C-r" 'helm-recentf)

  (:keymaps 'help-map
   "a" 'helm-apropos
   "i" 'helm-info-emacs)

  (:keymaps 'helm-map
   "<tab>" 'helm-execute-persistent-action
   "C-i" 'helm-execute-persistent-action
   "C-z" 'helm-select-action
   "C-w" 'my-backward-delete-word)

  (my-map
    "h" '(:command helm-command-prefix :which-key "helm")
    "o r" 'helm-recentf)

  :init
  (setq helm-split-window-in-side-p t
        helm-display-header-line nil)

  :config
  (require 'helm-config)
  (helm-mode 1)

  (helm-autoresize-mode t))

(use-package helm-regexp
  :ensure nil
  :defer t

  :config
  ;; Make helm-occur auto-enable follow-mode, acting more like swiper.
  ;; see https://github.com/emacs-helm/helm/issues/530#issuecomment-195350607
  (setq helm-source-occur
        (helm-make-source "Occur" 'helm-source-multi-occur
          :follow 1)))

(use-package helm-semantic
  :ensure nil
  :defer t

  :config
  (push '(c-mode . semantic-format-tag-summarize) helm-semantic-display-style)
  (push '(c++-mode . semantic-format-tag-summarize) helm-semantic-display-style))

(use-package helm-imenu
  :ensure nil
  :defer t

  :init
  (setq helm-imenu-execute-action-at-once-if-one nil))

(use-package helm-adaptive
  :ensure nil
  :defer t

  :init
  (setq helm-adaptive-history-file
        (my-cache-dir "helm-adaptive-history")))

(use-package helm-ext
  :config
  (helm-ext-ff-enable-split-actions t))

(use-package helm-locate
  :ensure nil

  :general
  (:keymaps 'helm-generic-files-map
   "C-c C-h" 'helm-ext-ff-execute-horizontal-split
   "C-c C-v" 'helm-ext-ff-execute-vertical-split))

(use-package helm-files
  :ensure nil

  :general
  (:keymaps '(helm-find-files-map helm-buffer-map)
   "C-c C-h" 'helm-ext-ff-execute-horizontal-split
   "C-c C-v" 'helm-ext-ff-execute-vertical-split))

(use-package helm-projectile
  :diminish projectile-mode

  :general
  ("C-<" 'my-open-buffer
   "C->" 'my-open-file
   "C-M-/" 'my-helm-ag)

  (:keymaps '(helm-projectile-find-file-map helm-projectile-projects-map)
   "C-c C-h" 'helm-ext-ff-execute-horizontal-split
   "C-c C-v" 'helm-ext-ff-execute-vertical-split)

  (my-map
    "o b" 'my-open-buffer
    "o f" 'my-open-file
    "o ." 'my-dots-file
    ". f" 'my-dots-file)

  :init
  (add-hook 'after-init-hook #'helm-projectile-on)

  :config
  (defun my-dots-file ()
    "Open a dotfile."
    (interactive)

    (ignore-errors
      (let* ((target my--dots-path)
             (default-directory target)
             (projectile-cached-project-root target)
             (projectile-require-project-root nil))
        (helm-projectile-find-file))))

  (defun my-open-file (arg)
    "Open a file.

When in a Projectile project, use Projectile. This can be
overridden with the prefix ARG."
    (interactive "P")

    (if (and (not arg) (projectile-project-p))
        (helm-projectile)
      (helm-find-files nil)))

  (defun my-open-buffer (arg)
    "Open a buffer.

When in a Projectile project, use Projectile. This can be
overridden with the prefix ARG."
    (interactive "P")

    (if (and (not arg) (projectile-project-p))
        (helm-projectile-switch-to-buffer)
      (helm-buffers-list)))

  (defun my-helm-ag (arg)
    "Search files.

When in a Projectile project, use Projectile. This can be
overridden with the prefix ARG."
    (interactive "P")

    (if (and (not arg) (projectile-project-p))
        ;; We are specifically not using helm-projectile-ag because it does
        ;; other things such as using grep-find-ignored-{files,directories}
        ;; without us asking it to. Beyond that, I see no reason to use it.
        (helm-do-ag (projectile-project-root))
      (helm-do-ag))))

(use-package helm-mt
  :general
  (my-map
    "o t" 'helm-mt)

  (:keymaps 'helm-mt/keymap
   "C-c C-h" 'helm-ext-ff-execute-horizontal-split
   "C-c C-v" 'helm-ext-ff-execute-vertical-split)

  :config
  (helm-mt/reroute-terminal-functions t))

(use-package helm-open-github :defer t)

(use-package helm-unicode
  :general
  ([remap insert-char] 'helm-unicode))

(use-package helm-describe-modes :defer t)

(use-package helm-ag
  :general
  (:keymaps 'helm-ag-map
   "C-c a" 'my-helm-ag-launch-ag

   "C-c C-h" 'my-helm-ag-split-horizontal
   "C-c C-v" 'my-helm-ag-split-vertical)

  (my-map
    ". s" 'my-dots-search)

  :init
  (defconst my--helm-ag-dotfile-options
    '("--hidden"
      "--ignore-dir .git"
      "--ignore .gitignore"
      "--ignore .projectile")
    "Options to use with ag when searching dotfiles.")

  (defun my--helm-ag-merge-options (options)
    (s-join " " (-union (if helm-ag--extra-options
                            (s-split " " helm-ag--extra-options)
                          '())
                        options)))

  ;; Note that we could use dir-locals but that would only take effect once a
  ;; file under the directory were accessed, AFAIK. It also wouldn't take effect
  ;; when running for example my-dots-search from outside of the dots dir.
  (define-advice helm-do-ag
      (:around (old-func &optional basedir targets query) show-hidden-dotfiles)
    "Show hidden files and folders when searching dotfiles."

    (if (and basedir (f-same? basedir my--dots-path))
        (let ((helm-ag--extra-options (my--helm-ag-merge-options my--helm-ag-dotfile-options)))
          (funcall old-func basedir targets query))
      (funcall old-func basedir targets query)))

  (defun my-dots-search ()
    "Search within dotfiles."
    (interactive)

    (helm-do-ag my--dots-path))

  (defun my-search-todo ()
    "Search for any TODO markers as specified in hl-todo-keyword-faces.

Note that this uses the word boundary \\b to avoid matching these
within other words, but this means that non-word keywords such as
???, which is in the list by default, will not be matched."
    (interactive)
    (require 'projectile)

    (let* ((grouped (funcall #'regexp-opt (--map (car it) hl-todo-keyword-faces)))
           (pcre (rxt-elisp-to-pcre (s-wrap grouped "\\b"))))
      (helm-do-ag (projectile-project-root) nil pcre)))

  (defun my--helm-ag-launch-ag (_candidate)
    (require 'ag)

    (let* ((ag-arguments (-union (or ag-arguments '())
                                 (cdr (butlast helm-ag--last-command))))
           (query helm-ag--last-query)
           (joined-patterns (helm-ag--join-patterns query)))
      (ag-regexp joined-patterns helm-ag--last-default-directory)))

  (defun my-helm-ag-launch-ag ()
    "Launch ag.el from the current helm-ag invocation."
    (interactive)

    (with-helm-alive-p
      (helm-exit-and-execute-action 'my--helm-ag-launch-ag)))

  (defun my--helm-ag-horizontal-find-func (buf)
    (select-window (split-window-below))
    (if (get-buffer buf)
        (switch-to-buffer buf)
      (find-file buf))
    (balance-windows))

  (defun my--helm-ag-action-find-file-horizontal (candidate)
    (dolist (buf (helm-marked-candidates))
      (helm-ag--find-file-action buf 'my--helm-ag-horizontal-find-func
                                 (helm-ag--search-this-file-p))))

  (defun my-helm-ag-split-horizontal ()
    "Open ag candidate(s) in horizontal splits."
    (interactive)

    (with-helm-alive-p
      (helm-exit-and-execute-action 'my--helm-ag-action-find-file-horizontal)))

  (defun my--helm-ag-vertical-find-func (buf)
    (select-window (split-window-right))
    (if (get-buffer buf)
        (switch-to-buffer buf)
      (find-file buf))
    (balance-windows))

  (defun my--helm-ag-action-find-file-vertical (candidate)
    (dolist (buf (helm-marked-candidates))
      (helm-ag--find-file-action buf 'my--helm-ag-vertical-find-func
                                 (helm-ag--search-this-file-p))))

  (defun my-helm-ag-split-vertical ()
    "Open ag candidate(s) in vertical splits."
    (interactive)

    (with-helm-alive-p
      (helm-exit-and-execute-action 'my--helm-ag-action-find-file-vertical))))

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
  :general
  ([remap describe-bindings] 'helm-descbinds))

(use-package helm-flycheck
  :general
  (my-map
    "c h" 'helm-flycheck)

  (:keymaps 'flycheck-mode-map
   "C-c ! h" 'helm-flycheck))

(use-package helm-flyspell
  :general
  (:keymaps '(normal insert)
   "C-;" 'my-flyspell-last)

  (:keymaps 'normal
   "[ s" 'my-flyspell-goto-previous-error
   "] s" 'flyspell-goto-next-error

   "[ S" 'my-check-previous-spelling-error
   "] S" 'my-check-next-spelling-error

   "z =" 'helm-flyspell-correct)

  :config
  (defun my-flyspell-last ()
    "Interactively correct the previous spelling error."
    (interactive)

    (save-excursion
      (my-check-previous-spelling-error)))

  (defun my--push-mark-no-activate ()
    "Pushes `point' to `mark-ring' and does not activate the region
 Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
    (push-mark (point) t nil))

  (defun my-check-previous-spelling-error ()
    "Jump to the previous spelling error and correct it."
    (interactive)

    (my--push-mark-no-activate)
    (my-flyspell-goto-previous-error 1)
    (call-interactively #'helm-flyspell-correct))

  (defun my-check-next-spelling-error ()
    "Jump to the next spelling error and correct it."
    (interactive)
    (my--push-mark-no-activate)
    (flyspell-goto-next-error)
    (call-interactively #'helm-flyspell-correct)))

(use-package helm-make :defer t)

(use-package helm-company
  :general
  (:keymaps 'company-active-map
   "M-/" 'helm-company))

(use-package helm-css-scss
  :general
  (:keymaps '(css-mode-map less-css-mode-map scss-mode-map)
   "M-i" 'helm-css-scss)

  :init
  (setq helm-css-scss-split-direction 'split-window-horizontally))

(use-package helm-tramp :defer t)

(use-package ace-jump-helm-line
  :general
  (:keymaps 'helm-map
   "C-'" 'ace-jump-helm-line)

  :init
  (setq ace-jump-helm-line-default-action 'select
        ace-jump-helm-line-style 'post

        ;; Press 'o' before the avy anchor to only move to it.
        ace-jump-helm-line-move-only-key ?o

        ;; Press 'p' before the avy anchor to move to it and execute.
        ;; it's persistent action
        ace-jump-helm-line-persistent-key ?p))

(provide 'conf/helm)
