(require 'straight)
(require 'use-package)
(require 'general)
(eval-when-compile
  (require 'conf/common))

(use-package helm
  :demand t

  :general-config
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
   "f" 'my-helm-faces-at-point)

  (my-map
    "h" (cons "helm" helm-command-map)

    "s i" 'helm-semantic-or-imenu
    "s I" 'helm-imenu-in-all-buffers
    "s o" 'helm-occur
    "s m" 'helm-all-mark-rings

    "o r" 'helm-recentf
    "o R" 'helm-register)

  :init
  (setq helm-display-header-line nil)

  (defun my--helm-toggle-scroll-bar ()
    "Toggle the scroll-bar within the Helm candidates window."
    (interactive)

    (with-helm-window
      (my-toggle-scroll-bar)))

  :config
  (require 'helm-lib)

  (helm-autoresize-mode t)

  (defun my--get-faces-at-point ()
    ;; NOTE
    ;; We can also get:
    ;; * the defface via property 'face-defface-spec
    ;; * the specs defined by themes via property 'theme-face
    (remq nil (-flatten
               (-concat
                (list (plist-get (text-properties-at (point)) 'face))
                (list (get-char-property (point) 'face))
                (list (get-char-property (point) 'read-face-name))))))

  (defun my--helm-faces-at-point-format-display (face)
    "Propertize a candidate of the Helm window."
    (concat
     (symbol-name face)
     (propertize " " 'display '(space :align-to (- right 8)))
     (propertize "example" 'face face)
     " "
     ))

  (defun my--helm-faces-at-point-format-candidate (face)
    `(,(my--helm-faces-at-point-format-display face) . ,face))

  (defun my--helm-faces-at-point-build-candidates ()
    (with-helm-current-buffer
      (-map #'my--helm-faces-at-point-format-candidate
            (my--get-faces-at-point))))

  (defun my--helm-faces-at-point-action-insert-name (candidate)
    "Insert the face name into the buffer."
    (insert (symbol-name candidate)))

  (defun my--helm-faces-at-point-action-copy-name (candidate)
    "Copy the face name."
    (kill-new (symbol-name candidate)))

  (defun my--helm-faces-at-point-action-describe (candidate)
    "Describe the face"
    (describe-face candidate))

  (defconst my--helm-faces-at-point-actions
    (helm-make-actions
     "Describe face" #'my--helm-faces-at-point-action-describe
     "Insert face name" #'my--helm-faces-at-point-action-insert-name
     "Copy face name" #'my--helm-faces-at-point-action-copy-name))

  (defun my--helm-faces-at-point-list-faces-action (candidate)
    (with-helm-current-buffer
      (list-faces-display
       (regexp-opt (-map #'symbol-name (my--get-faces-at-point))))))

  (defun my--helm-faces-at-point-list-faces ()
    (interactive)

    (with-helm-alive-p
      (helm-exit-and-execute-action 'my--helm-faces-at-point-list-faces-action)))

  (defvar my--helm-faces-at-point-keymap
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map helm-map)
      (define-key map (kbd "C-c C-l") 'my--helm-faces-at-point-list-faces)
      map)
    "Keymap for `my-helm-faces-at-point'.")

  (defconst my--helm-source-faces-at-point
    (helm-build-sync-source "Faces at point"
      :init 'my--helm-remove-selection-highlight
      :cleanup 'my--helm-restore-selection-highlight
      :candidates 'my--helm-faces-at-point-build-candidates
      :action my--helm-faces-at-point-actions
      :keymap my--helm-faces-at-point-keymap
      :match-part 'my--helm-select-first-word))

  (defun my-helm-faces-at-point ()
    "List the faces at the point."
    (interactive)
    (helm :sources 'my--helm-source-faces-at-point
          :buffer "*helm faces at point*")))

(use-package helm-mode
  :straight nil

  :config
  (helm-mode 1))

(use-package helm-regexp
  :straight nil
  :defer t

  :custom
  (helm-moccur-show-buffer-fontification t))

(use-package helm-font
  :straight nil

  :general-config
  (my-map
    "i c" 'helm-ucs))

(use-package helm-imenu
  :straight nil
  :defer t

  :init
  (setq helm-imenu-execute-action-at-once-if-one nil
        helm-imenu-extra-modes '(markdown-mode)))

(use-package helm-ext
  :after helm
  :config
  (helm-ext-ff-enable-split-actions t))

(use-package helm-locate
  :straight nil

  :general-config
  (:keymaps 'helm-generic-files-map
   "C-c C-h" 'helm-ext-ff-buffer-execute-horizontal-split
   "C-c C-v" 'helm-ext-ff-buffer-execute-vertical-split))

(use-package helm-buffers
  :straight nil

  :config
  (setq helm-source-buffers-list
          (helm-make-source "Buffers" 'helm-source-buffers))

  (helm-add-action-to-source
   "Split Horizontal" 'helm-ext-ff-buffer-action-horizontal-split helm-source-buffers-list)

  (helm-add-action-to-source
   "Split Horizontal" 'helm-ext-ff-buffer-action-horizontal-split helm-source-buffers-list))

(use-package helm-files
  :straight nil

  :general-config
  (:keymaps 'helm-buffer-map
   "C-c C-k" 'helm-buffer-run-kill-persistent)

  (:keymaps '(helm-find-files-map helm-buffer-map)
   "C-c C-h" 'helm-ext-ff-buffer-execute-horizontal-split
   "C-c C-v" 'helm-ext-ff-buffer-execute-vertical-split)

  :config
  (add-hook 'helm-find-files-before-init-hook
    (lambda ()
      (helm-add-action-to-source
       "Split Horizontal" 'helm-ext-ff-buffer-action-horizontal-split helm-source-find-files)
      (helm-add-action-to-source
       "Split Vertical" 'helm-ext-ff-buffer-action-vertical-split helm-source-find-files))))

(use-package helm-projectile
  :general-config
  ("C-<" 'my-open-buffer
   "C->" 'my-open-file
   "C-M-/" 'my-helm-ag)

  (:keymaps '(helm-projectile-find-file-map helm-projectile-projects-map)
   "C-c C-h" 'helm-ext-ff-buffer-execute-horizontal-split
   "C-c C-v" 'helm-ext-ff-buffer-execute-vertical-split)

  (my-map
    "s a" 'my-helm-ag

    "o b" 'my-open-buffer
    "o f" 'my-open-file
    "o ." 'my-dots-file
    ". f" 'my-dots-file)

  :hook
  (after-init . helm-projectile-on)

  :config
  (helm-add-action-to-source
   "Split Horizontal" 'helm-ext-ff-buffer-action-horizontal-split helm-source-projectile-files-list)
  (helm-add-action-to-source
   "Split Vertical" 'helm-ext-ff-buffer-action-vertical-split helm-source-projectile-files-list)

  (helm-add-action-to-source
   "Split Horizontal" 'helm-ext-ff-buffer-action-horizontal-split helm-source-projectile-buffers-list)
  (helm-add-action-to-source
   "Split Vertical" 'helm-ext-ff-buffer-action-vertical-split helm-source-projectile-buffers-list)

  (helm-add-action-to-source
   "Split Horizontal" 'helm-ext-ff-buffer-action-horizontal-split helm-source-projectile-projects)
  (helm-add-action-to-source
   "Split Vertical" 'helm-ext-ff-buffer-action-vertical-split helm-source-projectile-projects)

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

(use-package helm-unicode
  :general-config
  ([remap insert-char] 'helm-unicode)
  )

(use-package helm-rg
  :if (executable-find "rg")

  :defer t)

(use-package helm-ag
  :if (executable-find "ag")

  :general-config
  (:keymaps 'helm-ag-map
   "C-c a" 'my-helm-ag-launch-ag

   "C-c C-h" 'helm-ext-ff-helm-ag-execute-horizontal-split
   "C-c C-v" 'helm-ext-ff-helm-ag-execute-vertical-split)

  (my-map
    "s t" 'my-search-todos
    ". p" 'my-search-packages
    ". s" 'my-dots-search)

  :init
  (setq helm-ag-use-agignore t)

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

  :config
  (defun helm-do-ag--helm (&optional query)
    (let ((search-dir (if (not (helm-ag--windows-p))
                          helm-ag--default-directory
                        (if (helm-do-ag--target-one-directory-p helm-ag--default-target)
                            (car helm-ag--default-target)
                          helm-ag--default-directory))))
      (helm-attrset 'name (helm-ag--helm-header search-dir)
                    helm-source-do-ag)
      (helm :sources '(helm-source-do-ag) :buffer "*helm-ag*" :keymap helm-do-ag-map
            :input (or query
                       (helm-ag--marked-input t)
                       (helm-ag--insert-thing-at-point helm-ag-insert-at-point))
            :history 'helm-ag--helm-history)))

  (defun helm-do-ag (&optional basedir targets query)
    (interactive)
    (require 'helm-mode)
    (helm-ag--init-state)
    (let* ((helm-ag--default-directory (or basedir default-directory))
           (helm-ag--default-target (cond (targets targets)
                                          ((and (helm-ag--windows-p) basedir) (list basedir))
                                          (t
                                           (when (and (not basedir) (not helm-ag--buffer-search))
                                             (helm-read-file-name
                                              "Search in file(s): "
                                              :default default-directory
                                              :marked-candidates t :must-match t)))))
           (helm-do-ag--extensions (when helm-ag--default-target
                                     (helm-ag--do-ag-searched-extensions)))
           (one-directory-p (helm-do-ag--target-one-directory-p
                             helm-ag--default-target)))
      (helm-ag--set-do-ag-option)
      (helm-ag--set-command-features)
      (helm-ag--save-current-context)
      (helm-attrset 'search-this-file
                    (and (= (length helm-ag--default-target) 1)
                         (not (file-directory-p (car helm-ag--default-target)))
                         (car helm-ag--default-target))
                    helm-source-do-ag)
      (if (or (helm-ag--windows-p) (not one-directory-p)) ;; Path argument must be specified on Windows
          (helm-do-ag--helm query)
        (let* ((helm-ag--default-directory
                (file-name-as-directory (car helm-ag--default-target)))
               (helm-ag--default-target nil))
          (helm-do-ag--helm query)))))

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

  (use-package helm-ext
    :defer t
    :config
    (helm-ext-ff-define-split helm-ag horizontal
      (lambda (candidate)
        (helm-ag--find-file-action candidate
                                   'find-file
                                   (helm-ag--search-this-file-p)))
      balance)

    (helm-ext-ff-define-split helm-ag vertical
      (lambda (candidate)
        (helm-ag--find-file-action candidate
                                   'find-file
                                   (helm-ag--search-this-file-p)))
      balance)

    (helm-add-action-to-source
     "Split Horizontal" 'helm-ext-ff-helm-ag-action-horizontal-split helm-source-do-ag)
    (helm-add-action-to-source
     "Split Vertical" 'helm-ext-ff-helm-ag-action-vertical-split helm-source-do-ag)))

(use-package helm-descbinds
  :general-config
  ([remap describe-bindings] 'helm-descbinds)

  :init
  (define-advice describe-bindings
      (:override (&optional prefix buffer) auto-load-helm-descbinds)
    "Explicitly auto-load helm-descbinds."
    (helm-descbinds prefix buffer)))

(use-package helm-flyspell
  :general-config
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

(provide 'conf/helm)
