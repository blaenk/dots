(require 'use-package)
(require 'general)
(require 'conf/common)

(use-package helm
  :diminish helm-mode
  :demand t

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
   "<f9>" 'my--helm-toggle-scroll-bar
   "<tab>" 'helm-execute-persistent-action
   "C-i" 'helm-execute-persistent-action
   "C-z" 'helm-select-action
   "C-w" 'my-backward-delete-word)

  (:keymaps 'helm-command-map
   "s" 'my-helm-solarized-colors)

  (my-map
    "h" '(:command helm-command-prefix :which-key "helm")

    "s i" 'helm-semantic-or-imenu
    "s I" 'helm-imenu-in-all-buffers
    "s o" 'helm-occur
    "s m" 'helm-all-mark-rings

    "o r" 'helm-recentf)

  :init
  (setq helm-display-header-line nil)

  (defun my--helm-toggle-scroll-bar ()
    "Toggle the scroll-bar within the Helm candidates window."
    (interactive)

    (with-helm-window
      (my-toggle-scroll-bar)))

  :config
  (require 'helm-config)
  (helm-mode 1)

  (helm-autoresize-mode t)

  (defun my--helm-solarized-colors ()
    "Produce an alist of color name and its hex color.

Note that this gives different colors based on the enabled
variant. It re-evaluates the macro in case the variant changes
mid-execution."
    (my-with-solarized-colors
     (--map `(,it . ,(symbol-value it))
            '(yellow-lc
              yellow
              yellow-hc
              orange-lc
              orange
              orange-hc
              red-lc
              red
              red-hc
              magenta-lc
              magenta
              magenta-hc
              violet-lc
              violet
              violet-hc
              blue-lc
              blue
              blue-hc
              cyan-lc
              cyan
              cyan-hc
              green-lc
              green
              green-hc
              base0
              base00
              base1
              base01
              base2
              base02
              base3
              base03))))

  (defun my--helm-solarized-format-display (color)
    "Propertize a candidate of the Helm window."
    (-let (((name . hex) color))
      (concat
       (symbol-name name)

       ;; Make 'foreground' go up to 5 characters away from 'bold'. Do this by
       ;; recognizing that ' background ' takes 12, then there's 5 spaces of
       ;; padding, then 'bold' which is 4, then there will be 10 characters for
       ;; 'foreground', and 5 spaces after it as a gap between it and 'bold'.
       (propertize " " 'display '(space :align-to (- right (+ (+ (+ (+ 12 4) 5) 10) 5))))
       (propertize "foreground" 'face `(:foreground ,hex))

       ;; Make 'bold' go up to 5 characters away from the ' background '. Do
       ;; this by recognizing that ' background ' itself takes up 12 characters,
       ;; 'bold' takes 4, and then the padding takes 5.
       (propertize " " 'display '(space :align-to (- right (+ (+ 12 4) 5))))
       (propertize "bold" 'face `(:foreground ,hex :weight bold))

       ;; Make ' background ' line up with the right margin. Since it's 12
       ;; characters long, make a space that goes up to 12 characters from the
       ;; right.
       (propertize " " 'display '(space :align-to (- right 12)))
       (propertize " background " 'face `(:background ,hex :foreground "white" :weight bold))
       )))

  (defun my--helm-solarized-format-candidate (color)
    `(,(my--helm-solarized-format-display color) . ,color))

  (defun my--helm-solarized-build-candidates ()
    (-map #'my--helm-solarized-format-candidate (my--helm-solarized-colors)))

  (defun my--helm-solarized-select-name (candidate)
    "Function to only match on the Solarized color name."
    (cadr (s-match "^\\([^ ]+\\)\\b" candidate)))

  (defun my--helm-solarized-action-insert-name (candidate)
    "Insert the color name into the buffer."
    (insert (symbol-name (car candidate))))

  (defun my--helm-solarized-action-insert-hex (candidate)
    "Insert the color's hex value into the buffer."
    (insert (cdr candidate)))

  (defun my--helm-solarized-action-copy-name (candidate)
    "Copy the color name."
    (kill-new (symbol-name (car candidate))))

  (defun my--helm-solarized-action-copy-hex (candidate)
    "Copy the color's hex value."
    (kill-new (cdr candidate)))

  (defconst my--helm-solarized-actions
    (helm-make-actions
     "Insert name" #'my--helm-solarized-action-insert-name
     "Insert hex" #'my--helm-solarized-action-insert-hex

     "Copy name" #'my--helm-solarized-action-copy-name
     "Copy hex" #'my--helm-solarized-action-copy-hex))

  (defun my--helm-solarized-remove-selection-highlight ()
    "Remove the Helm selection background color while using Helm Solarized.

It can clash with the colors being shown."
    (set-face-attribute 'helm-selection nil :background 'unspecified))

  (defun my--helm-solarized-restore-selection-highlight ()
    "Restore the Helm selection background color."
    (face-spec-set 'helm-selection
                   (get 'helm-selection 'face-defface-spec)
                   'face-defface-spec))

  (defconst my--helm-source-solarized-colors
    (helm-build-sync-source "solarized colors"
      :init 'my--helm-solarized-remove-selection-highlight
      :cleanup 'my--helm-solarized-restore-selection-highlight
      :candidates 'my--helm-solarized-build-candidates
      :action my--helm-solarized-actions
      :match-part 'my--helm-solarized-select-name))

  (defun my-helm-solarized-colors ()
    "List the Solarized color names and hex values."
    (interactive)
    (helm :sources 'my--helm-source-solarized-colors
          :buffer "*helm solarized colors*")))

(use-package helm-regexp
  :ensure nil
  :defer t

  :config
  ;; Make helm-occur auto-enable follow-mode, acting more like swiper.
  ;; see https://github.com/emacs-helm/helm/issues/530#issuecomment-195350607
  (setq helm-moccur-show-buffer-fontification t
        helm-source-occur
        (helm-make-source "Occur" 'helm-source-multi-occur
          :follow 1)))


(use-package helm-font
  :ensure nil
  :defer t

  :general
  (my-map
    "i c" 'helm-ucs))

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

(use-package helm-bookmark
  :ensure nil
  :defer t

  :general
  (:keymaps 'helm-bookmark-map
   "C-c C-k" 'helm-bookmark-run-delete

   "C-c C-h" 'my-helm-bookmarks-split-horizontal
   "C-c C-v" 'my-helm-bookmarks-split-vertical)

  (my-map
    "o m" 'my-open-bookmark)

  :init
  (setq helm-bookmark-show-location t)

  (defun my--helm-bookmark-projectile-p (bookmark)
    (and (projectile-project-p)
         (f-ancestor-of? (projectile-project-root)
                         (bookmark-get-filename bookmark))))

  (defun my--helm-bookmark-project-setup-alist ()
    (helm-bookmark-filter-setup-alist 'my--helm-bookmark-projectile-p))

  (defconst my--helm-source-bookmark-project
    (helm-make-source "Project Bookmarks" 'helm-source-filtered-bookmarks
      :init (lambda ()
              (bookmark-maybe-load-default-file)
              (helm-init-candidates-in-buffer
                  'global (my--helm-bookmark-project-setup-alist)))))

  (defconst my--helm-project-bookmarks-sources
    (append '(helm-source-bookmark-org
              my--helm-source-bookmark-project
              helm-source-bookmark-helm-find-files
              helm-source-bookmark-info
              helm-source-bookmark-gnus
              helm-source-bookmark-man
              helm-source-bookmark-images
              helm-source-bookmark-w3m)
            (list 'helm-source-bookmark-uncategorized
                  'helm-source-bookmark-set))
    "List of sources to use in `my-project-bookmarks'.")

  (defun my-open-bookmark (arg)
    "Open a bookmark.

When in a Projectile project, only show project bookmarks. This
can be overridden with the prefix ARG."
    (interactive "P")

    (if (and (not arg) (projectile-project-p))
        (let ((helm-bookmark-default-filtered-sources my--helm-project-bookmarks-sources))
          (helm-filtered-bookmarks))
      (helm-filtered-bookmarks)))

  (defun my--helm-bookmarks-action-vertical (candidate)
    (dolist (bookmark (helm-marked-candidates))
      (select-window (split-window-right))
      (bookmark-jump bookmark))

    (balance-windows))

  (defun my-helm-bookmarks-split-vertical ()
    "Open the bookmark in a vertical split."
    (interactive)

    (with-helm-alive-p
     (helm-exit-and-execute-action 'my--helm-bookmarks-action-vertical)))

  (defun my--helm-bookmarks-action-horizontal (candidate)
    (dolist (bookmark (helm-marked-candidates))
      (select-window (split-window-below))
      (bookmark-jump bookmark))

    (balance-windows))

  (defun my-helm-bookmarks-split-horizontal ()
    "Open the bookmark in a horizontal split."
    (interactive)

    (with-helm-alive-p
     (helm-exit-and-execute-action 'my--helm-bookmarks-action-horizontal))))

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
  (:keymaps 'helm-buffer-map
   "C-c C-k" 'helm-buffer-run-kill-persistent)

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
    "s a" 'my-helm-ag

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

With ARG, when in a project, show only project buffers."
    (interactive "P")

    (if (not arg)
        (helm-buffers-list)
      (helm-projectile-switch-to-buffer)))

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
  :if (executable-find "ag")

  :general
  (:keymaps 'helm-ag-map
   "C-c a" 'my-helm-ag-launch-ag

   "C-c C-h" 'my-helm-ag-split-horizontal
   "C-c C-v" 'my-helm-ag-split-vertical)

  (my-map
    "s t" 'my-search-todos
    ". p" 'my-search-packages
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

  (defun my-search-todos ()
    "Search for any TODO markers as specified in hl-todo-keyword-faces.

Note that this uses the word boundary \\b to avoid matching these
within other words, but this means that non-word keywords such as
???, which is in the list by default, will not be matched."
    (interactive)
    (require 'projectile)

    (let* ((grouped (funcall #'regexp-opt (--map (car it) hl-todo-keyword-faces)))
           (pcre (rxt-elisp-to-pcre (s-wrap grouped "\\b"))))
      (helm-do-ag (projectile-project-root) nil pcre)))

  (defun my-search-packages ()
    "List any use-package declarations."
    (interactive)
    (require 'projectile)

    (helm-do-ag my--dots-path nil "\\(use-package "))

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
  ([remap describe-bindings] 'helm-descbinds)

  :init
  (define-advice describe-bindings
      (:override (&optional prefix buffer) auto-load-helm-descbinds)
    (helm-descbinds prefix buffer)))

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
