(require 'use-package)
(require 'general)
(require 'conf/common)

(use-package profiler
  :ensure nil

  :general
  (my-map
    "e p" '(:ignore t :which-key "profiler")
    "e p p" 'profiler-start
    "e p r" 'profiler-report
    "e p s" 'profiler-stop))

(use-package autorevert
  :ensure nil
  :defer t

  :init
  (setq auto-revert-check-vc-info t)

  (defun my-disable-auto-revert-vc-when-remote ()
    (when (and buffer-file-name (file-remote-p buffer-file-name))
      (setq-local auto-revert-check-vc-info nil)))

  (add-hook 'find-file-hook #'my-disable-auto-revert-vc-when-remote)

  ;; emacs 25 disables notify for global-auto-revert
  ;; because there can be a problem on OSX where it may use too many resources
  ;; or something
  (defun my-enable-notify ()
    (when (not (eq system-type 'darwin))
      (setq auto-revert-use-notify t)))

  (add-hook 'global-auto-revert-mode 'my-enable-notify)
  (global-auto-revert-mode 1))

(use-package delsel
  :ensure nil
  :defer t

  :init
  (delete-selection-mode 1))

(use-package iso-transl
  :ensure nil

  :general
  (:keymaps 'iso-transl-ctl-x-8-map
   "<right>" "→"
   "<left>" "←"
   "n" "ñ"))

(use-package winner
  :ensure nil

  :general
  (my-map
    "w u" 'winner-undo
    "w r" 'winner-redo)

  :init
  (winner-mode 1))

(use-package newcomment
  :ensure nil
  :defer t

  :init
  (define-advice comment-indent-new-line
      (:after (&optional soft) at-least-one-space)
    "Ensure that at least one space is added after the comment-start."
    (let ((start (regexp-quote comment-start)))
      (when (and (nth 4 (syntax-ppss))
                 (looking-back start (+ (point) (length start)))
                 (not (looking-back " "  (+ (point) 1))))
        (insert " ")))))

(use-package mule-util
  :ensure nil
  :defer nil

  :init
  (setq truncate-string-ellipsis "…"))

(use-package menu-bar
  :ensure nil
  :defer t

  :general
  ("<f10>" 'toggle-menu-bar-mode-from-frame)

  :init
  (menu-bar-mode -1))

(use-package tool-bar
  :ensure nil
  :defer t

  :init
  (tool-bar-mode -1))

(use-package scroll-bar
  :ensure nil

  :general
  ("<f9>" 'my-scroll-bar-toggle)

  :init
  (defun my-scroll-bar-toggle ()
    (interactive)

    (let ((window (selected-window))
          (args (if (< emacs-major-version 25)
                    (list 'left nil)
                  (list 'left nil nil))))
      (if (eq (window-scroll-bar-width window) 0)
          (apply #'set-window-scroll-bars window nil args)
        (apply #'set-window-scroll-bars window 0 args)))

    (redraw-frame))

  (scroll-bar-mode -1)
  (setq scroll-bar-mode 'left))

(use-package frame
  :ensure nil
  :defer t

  :init
  (blink-cursor-mode 0))

(use-package paren
  :ensure nil
  :defer t

  :init
  (setq show-paren-delay 0
        show-paren-when-point-inside-paren t)

  (show-paren-mode 1))

(use-package which-func
  :ensure nil

  :general
  (my-map
    "p f" 'my-which-func-print)

  :init
  (defun my-which-func-print ()
    (interactive)
    (which-func-update)
    (message "→ %s" (gethash (selected-window) which-func-table)))

  (which-function-mode 1))

(use-package saveplace
  :ensure nil
  :defer t
  :defines save-place-file

  :init
  (setq-default save-place t)
  (setq save-place-file (my-cache-dir "saved-places")))

(use-package smerge-mode
  :ensure nil
  :defer t

  :init
  ;; attempt to start smerge, automatically disabling it if not relevant
  (add-hook 'find-file-hook #'smerge-start-session))

(use-package bookmark
  :ensure nil
  :defer t
  :defines bookmark-default-file

  :init
  (setq bookmark-default-file (my-cache-dir "bookmarks")))

(use-package recentf
  :ensure nil
  :defines recentf-save-file
  :defer t

  :init
  (setq recentf-save-file (my-cache-dir "recentf")
        recentf-max-saved-items 50)

  (recentf-mode 1))

(use-package savehist
  :ensure nil
  :defer t

  :init
  (setq savehist-save-minibuffer-history 1
        savehist-file (my-cache-dir "history"))

  (savehist-mode 1))

(use-package ido
  :ensure nil
  :defer t
  :defines ido-save-directory-list-file

  :init
  (setq ido-save-directory-list-file (my-cache-dir "ido.last")))

(use-package eshell
  :ensure nil
  :defer t
  :defines eshell-directory

  :init
  (setq eshell-directory (my-cache-dir "eshell")))

(use-package apropos
  :ensure nil
  :defer t
  :defines apropos-do-all

  :init
  (setq apropos-do-all t))

;; NOTE gdb also requires argument `-i=mi`
(use-package gdb-mi
  :ensure nil
  :defer t
  :defines
  gdb-many-windows
  gdb-show-main

  :init
  (setq gdb-many-windows t
        gdb-show-main t))

(use-package shell
  :ensure nil
  :defer t
  :defines explicit-shell-file-name

  :init
  (setq explicit-shell-file-name "/usr/bin/zsh"))

(use-package whitespace
  :ensure nil
  :diminish whitespace-mode
  :defer t

  :init
  (setq whitespace-line-column nil

        whitespace-indentation-regexp
        '("^ *\\(\t+\\)[^\n]" . "^ *\\(\t+\\)[^\n]")

        ;; NOTE
        ;; lines-tail to see which lines go beyond max col
        whitespace-style
        '(face indentation trailing empty space-after-tab
               space-before-tab tab-mark)

        whitespace-display-mappings
        '((space-mark 32 [183] [46])
          (space-mark 160 [164] [95])
          (newline-mark 10 [36 10])
          (tab-mark 9 [9656 9] [183 9] [187 9] [92 9])))

  (global-whitespace-mode 1))

(use-package js
  :ensure nil
  :defer t

  :init
  (setq js-indent-level 2
        js-switch-indent-offset 2))

(use-package sh-script
  :ensure nil
  :functions sh-set-shell
  :mode ("\\.zsh\\(rc\\)?\\'" . sh-mode)

  :init
  (setq sh-learn-basic-offset t
        sh-basic-offset 2
        sh-indentation 2)

  (defun my-zsh-hook ()
    (when (string-match "\\.zsh\\(rc\\)?\\'" buffer-file-name)
      (sh-set-shell "zsh")))

  (add-hook 'sh-mode-hook 'my-zsh-hook))

(use-package python
  :ensure nil
  :defer t

  :init
  ;; TODO other PEP8 stuff
  (defun my-python-hook ()
    (setq fill-column 79))

  (add-hook 'python-mode-hook #'my-python-hook))

(use-package semantic
  :ensure nil
  :defer t
  :defines
  semanticdb-default-save-directory

  :init
  (add-hook 'c++-mode 'semantic-mode)

  :config
  (use-package semantic/db-mode
    :ensure nil
    :functions
    global-semanticdb-minor-mode
    global-semantic-idle-scheduler-mode

    :init
    (setq semanticdb-default-save-directory (my-cache-dir "semanticdb"))

    :config
    (global-semanticdb-minor-mode 1))

  (use-package semantic/idle
    :ensure nil

    :config
    (global-semantic-idle-scheduler-mode 1)))

(use-package cc-mode
  :ensure nil
  :defer t
  :functions
  projectile-project-p
  projectile-project-root

  :init
  (setq c-tab-always-indent nil)
  (defvaralias 'c-basic-offset 'tab-width)

  :config
  ;; use C++ mode in header files
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

  (defun my-insert-include-guard ()
    (interactive)
    (let* ((project-root (when (projectile-project-p)
                           (projectile-project-root)))
           (buf-name (or (buffer-file-name) (buffer-name)))
           (name (if project-root
                     (replace-regexp-in-string
                      (regexp-quote project-root) ""
                      buf-name)
                   buf-name))
           (filtered (replace-regexp-in-string
                      (regexp-opt '("source/" "src/")) "" name))
           (ident (concat
                   (upcase
                    (replace-regexp-in-string "[/.-]" "_" filtered))
                   "_")))
      (save-excursion
        (goto-char (point-min))
        (insert "#ifndef " ident "\n")
        (insert "#define " ident "\n\n")
        (goto-char (point-max))
        (insert "\n#endif  // " ident)))))

(use-package cc-menus
  :ensure nil
  :defer t

  :config
  ;; add googletest TESTs to imenu
  (add-to-list 'cc-imenu-c++-generic-expression
               '("Test" "^TEST\\(_F\\)?(\\([^)]+\\))" 2) t))

(use-package imenu
  :ensure nil
  :defer t

  :init
  (setq imenu-auto-rescan t))

(use-package lisp-mode
  :ensure nil
  :defer t
  :functions my-lisp-indent-function
  :defines calculate-lisp-indent-last-sexp

  :init
  ;; taken from Fuco1
  ;; https://github.com/Fuco1/.emacs.d/blob/master/site-lisp/my-redef.el#L18
  ;;
  ;; redefines the silly indent of keyword lists
  ;; before
  ;;   (:foo bar
  ;;         :baz qux)
  ;; after
  ;;   (:foo bar
  ;;    :baz qux)
  (defun my-lisp-indent-function (indent-point state)
    "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
    (let ((normal-indent (current-column))
          (orig-point (point)))
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (cond
       ;; car of form doesn't seem to be a symbol, or is a keyword
       ((and (elt state 2)
             (or (not (looking-at "\\sw\\|\\s_"))
                 (looking-at ":")))
        (if (not (> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp))
            (progn (goto-char calculate-lisp-indent-last-sexp)
                   (beginning-of-line)
                   (parse-partial-sexp (point)
                                       calculate-lisp-indent-last-sexp 0 t)))
        ;; Indent under the list or under the first sexp on the same
        ;; line as calculate-lisp-indent-last-sexp.  Note that first
        ;; thing on that line has to be complete sexp since we are
        ;; inside the innermost containing sexp.
        (backward-prefix-chars)
        (current-column))
       ((and (save-excursion
               (goto-char indent-point)
               (skip-syntax-forward " ")
               (not (looking-at ":")))
             (save-excursion
               (goto-char orig-point)
               (looking-at ":")))
        (save-excursion
          (goto-char (+ 2 (elt state 1)))
          (current-column)))
       (t
        (let ((function (buffer-substring (point)
                                          (progn (forward-sexp 1) (point))))
              method)
          (setq method (or (function-get (intern-soft function)
                                         'lisp-indent-function)
                           (get (intern-soft function) 'lisp-indent-hook)))
          (cond ((or (eq method 'defun)
                     (and (null method)
                          (> (length function) 3)
                          (string-match "\\`def" function)))
                 (lisp-indent-defform state indent-point))
                ((integerp method)
                 (lisp-indent-specform method state
                                       indent-point normal-indent))
                (method
                 (funcall method indent-point state))))))))

  (add-hook 'emacs-lisp-mode-hook
            (defun my-use-my-lisp-indent-function ()
              (setq-local lisp-indent-function #'my-lisp-indent-function))))

(use-package diff
  :ensure nil
  :defer t

  :init
  (defun my-diff ()
    "Generate unified diff of current buffer with backing file."
    (interactive)
    (diff-buffer-with-file (current-buffer))))

(use-package ediff
  :ensure nil
  :defer t
  :functions
  my-toggle-ediff-wide-display
  ediff-toggle-wide-display

  :general
  (my-map "b d" 'ediff-current-file)

  :init
  (setq ediff-custom-diff-options "-u"
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain)

  ;; NOTE
  ;; doing M-x ediff-show-diff-output from ediff-current-file doesn't work
  ;; https://emacs.stackexchange.com/questions/22090/

  (defvar my-ediff-last-windows nil)

  (defun my-toggle-ediff-wide-display ()
    (require 'ediff-util)
    "Turn off wide-display mode (if was enabled) before quitting ediff."
    (when ediff-wide-display-p
      (ediff-toggle-wide-display)))

  ;; this is run for each buffer A, B, C after they are setup. note that the
  ;; "current" buffer is itself one of those. this means for example that if
  ;; whitespace-mode was enabled in the current buffer, simply launching ediff
  ;; will turn it off
  (defun my-ediff-prepare ()
    (setq my-ediff-last-windows (current-window-configuration))

    (when (bound-and-true-p hs-minor-mode)
      (setq-local my-ediff-was-on-hs-minor-mode t)
      (hs-minor-mode -1))

    (when (bound-and-true-p fci-mode)
      (setq-local my-ediff-was-on-fci-mode t)
      (fci-mode -1))

    (when (bound-and-true-p visual-line-mode)
      (setq-local my-ediff-was-on-visual-line-mode t)
      (visual-line-mode -1))

    (when (bound-and-true-p whitespace-mode)
      (setq-local my-ediff-was-on-whitespace-mode t)
      (global-whitespace-mode -1)))

  (defun my-ediff-start ()
    (interactive)
    (my-fullscreen-if-wasnt))

  (defun my-ediff-quit ()
    (interactive)

    (when (bound-and-true-p my-ediff-was-on-hs-minor-mode)
      (kill-local-variable my-ediff-was-on-hs-minor-mode)
      (hs-minor-mode +1))

    (when (bound-and-true-p my-ediff-was-on-fci-mode)
      (kill-local-variable my-ediff-was-on-fci-mode)
      (fci-mode +1))

    (when (bound-and-true-p my-ediff-was-on-visual-line-mode)
      (kill-local-variable my-ediff-was-on-visual-line-mode)
      (visual-line-mode +1))

    (when (bound-and-true-p my-ediff-was-on-whitespace-mode)
      (kill-local-variable my-ediff-was-on-whitespace-mode)
      (global-whitespace-mode +1))

    (set-window-configuration my-ediff-last-windows)
    (my-toggle-ediff-wide-display)
    (my-unfullscreen-if-wasnt))

  (add-hook 'ediff-prepare-buffer-hook #'my-ediff-prepare)
  (add-hook 'ediff-startup-hook #'my-ediff-start)
  (add-hook 'ediff-suspend-hook #'my-ediff-quit 'append)
  (add-hook 'ediff-quit-hook #'my-ediff-quit 'append))

(use-package minibuffer
  :ensure nil
  :defer t

  :init
  (add-to-list 'completion-styles 'substring t)
  (add-to-list 'completion-styles 'initials t))

(use-package elec-pair
  :ensure nil
  :defer t

  :init
  ;; disable electric-pair-mode in the minibuffer unless we're in
  ;; eval-expression
  (defun my-electric-pair-inhibit-in-minibuffer (char)
    'inhibit)

  (defun my-minibuffer-setup-hook ()
    (when (not (eq this-command 'pp-eval-expression))
      (setq-local electric-pair-inhibit-predicate
                  #'my-electric-pair-inhibit-in-minibuffer)))

  (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)

  (electric-pair-mode 1))

(use-package sgml-mode
  :ensure nil
  :defer t

  :init
  (setq sgml-basic-offset 2))

(use-package css-mode
  :ensure nil
  :defer t

  :init
  (setq css-indent-offset 2))

(use-package bug-reference
  :ensure nil
  :defer t

  :init
  (setq bug-reference-bug-regexp "\\(\
[Ii]ssue ?#\\|\
[Bb]ug ?#\\|\
[Pp]atch ?#\\|\
RFE ?#\\|\
PR [a-z-+]+/\
\\)\\([0-9]+\\(?:#[0-9]+\\)?\\)")

  (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
  (add-hook 'text-mode-hook #'bug-reference-mode))

(use-package goto-addr
  :ensure nil
  :defer t

  :init
  (add-hook 'prog-mode-hook #'goto-address-prog-mode)
  (add-hook 'text-mode-hook #'goto-address-mode))

(use-package org-table
  :ensure nil
  :defer t

  :commands orgtbl-mode
  :functions
  org-at-table-p
  org-table-hline-and-move

  ;; TODO check
  :general
  (:keymaps 'orgtbl-mode-map
   "C-c RET" 'my-orgtbl-ret)

  :config
  (defun my-orgtbl-ret ()
    (interactive)
    (if (org-at-table-p)
        (org-table-hline-and-move)
      (let (orgtbl-mode)
        ;; TODO
        ;; encodes C-c
        (call-interactively (key-binding (kbd "C-c RET")))))))

(use-package dired
  :ensure nil
  :defer t

  :init
  (setq dired-auto-revert-buffer t
        dired-listing-switches "-alhF")

  (when (or (memq system-type '(gnu gnu/linux))
            (string= (file-name-nondirectory insert-directory-program) "gls"))
    (setq dired-listing-switches
          (concat dired-listing-switches " --group-directories-first -v"))))

(use-package dired-x
  :ensure nil
  :defer t

  :init
  (setq dired-omit-verbose nil)

  (add-hook 'dired-mode-hook #'dired-omit-mode))

(use-package simple
  :ensure nil
  :defer t

  :general
  (:keymaps 'normal
   "[ c" 'my-previous-error
   "] c" 'my-next-error)

  (my-map
    "c" '(:ignore t :which-key "check")
    "c j" 'my-next-error
    "c k" 'my-previous-error

    "t a" 'auto-fill-mode)

  :init
  (setq next-error-recenter '(4))

  (add-hook 'prog-mode-hook #'visual-line-mode)

  (defun my-next-error (&optional n reset)
    "Dispatch to flycheck or standard emacs error."
    (interactive "P")
    (if (and (boundp 'flycheck-mode)
             (symbol-value flycheck-mode)
             (not (get-buffer-window "*compilation*")))
        (call-interactively 'flycheck-next-error)
      (call-interactively 'next-error)))

  (defun my-previous-error (&optional n reset)
    "Dispatch to flycheck or standard emacs error."
    (interactive "P")
    (if (and (boundp 'flycheck-mode)
             (symbol-value flycheck-mode)
             (not (get-buffer-window "*compilation*")))
        (call-interactively 'flycheck-previous-error)
      (call-interactively 'previous-error)))

  (defun my-prog-auto-fill ()
    (setq-local comment-auto-fill-only-comments t)
    (auto-fill-mode 1))

  (add-hook 'prog-mode-hook #'my-prog-auto-fill)

  (column-number-mode 1))

(use-package ansi-color
  :ensure nil
  :defer t
  :defines
  compilation-filter-start
  :functions
  ansi-color-apply-on-region

  :init
  (ignore-errors
    (defun colorize-compilation-buffer ()
      (when (eq major-mode 'compilation-mode)
        (ansi-color-apply-on-region compilation-filter-start (point-max))))

    (add-hook 'compilation-filter-hook #'colorize-compilation-buffer)))

(use-package compile
  :ensure nil
  :defer t

  :init
  (setq compilation-scroll-output 'first-error
        compilation-ask-about-save nil
        compilation-skip-threshold 0
        compilation-always-kill t))

(use-package hl-line
  :ensure nil
  :defer t

  :init
  (setq hl-line-sticky-flag t)
  (add-hook 'prog-mode-hook #'hl-line-mode))

(use-package help-mode
  :ensure nil
  :defer t

  :general
  (:keymaps 'help-mode-map
    "[" 'help-go-back
    "]" 'help-go-forward))

(use-package hideshow
  :ensure nil
  :defer t

  :init
  (add-hook 'prog-mode-hook #'hs-minor-mode))

(use-package flyspell
  :ensure nil
  :defer t

  :functions
  push-mark-no-activate
  flyspell-goto-previous-error
  flyspell-overlay-p
  flyspell-goto-next-error

  :general
  (:keymaps 'flyspell-mode-map
   "C-c $" nil)

  :init
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)

  :config
  (defun flyspell-goto-previous-error (arg)
    "Go to arg previous spelling error."
    (interactive "p")
    (while (not (= 0 arg))
      (let ((pos (point))
            (min (point-min)))
        (if (and (eq (current-buffer) flyspell-old-buffer-error)
                 (eq pos flyspell-old-pos-error))
            (progn
              (if (= flyspell-old-pos-error min)
                  ;; goto beginning of buffer
                  (progn
                    (message "Restarting from end of buffer")
                    (goto-char (point-max)))
                (backward-word 1))
              (setq pos (point))))
        ;; seek the next error
        (while (and (> pos min)
                    (let ((ovs (overlays-at pos))
                          (r '()))
                      (while (and (not r) (consp ovs))
                        (if (flyspell-overlay-p (car ovs))
                            (setq r t)
                          (setq ovs (cdr ovs))))
                      (not r)))
          (backward-word 1)
          (setq pos (point)))
        ;; save the current location for next invocation
        (setq arg (1- arg))
        (setq flyspell-old-pos-error pos)
        (setq flyspell-old-buffer-error (current-buffer))
        (goto-char pos)
        (if (= pos min)
            (progn
              (message "No more miss-spelled word!")
              (setq arg 0)))))))

(provide 'conf/built-in)
