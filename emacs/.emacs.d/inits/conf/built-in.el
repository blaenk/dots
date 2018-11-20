(require 'straight)
(require 'use-package)
(require 'general)
(eval-when-compile
  (require 'conf/common))

(use-package help-fns
  :straight nil

  :general
  (:keymaps 'help-map
   "s" 'describe-symbol))

(use-package tramp
  :straight nil
  :defer t

  :init
  (setq tramp-auto-save-directory (my-cache-dir "tramp")))

(use-package windmove
  :straight nil

  :general
  ("C-M-S-h" 'my-move-splitter-left
   "C-M-S-l" 'my-move-splitter-right
   "C-M-S-k" 'my-move-splitter-up
   "C-M-S-j" 'my-move-splitter-down)

  (my-map
    "w r" 'my-window-resizer/body)

  :init
  (defun my-move-splitter-left (arg)
    "Move the right edge to the left.

Resizing the right edge is not possible if there is no window to
the right. If there's a window to the left, then the left edge is
resized instead.

Since this command is meant to be repeatable, it preserves the
prefix argument so that it's not necessary to keep setting the it
for each adjustment."
    (interactive "P")

    (universal-argument--preserve)

    (adjust-window-trailing-edge
     (or (and arg (window-in-direction 'left nil t))
         (and (not (window-in-direction 'right nil t))
              (window-in-direction 'left nil t)))
     -1 t))

  (defun my-move-splitter-right (arg)
    "Move the right edge to the right.

Resizing the right edge is not possible if there is no window to
the right. If there's a window to the left, then the left edge is
resized instead.

Since this command is meant to be repeatable, it preserves the
prefix argument so that it's not necessary to keep setting the it
for each adjustment."
    (interactive "P")

    (universal-argument--preserve)

    (adjust-window-trailing-edge
     (or (and arg (window-in-direction 'left nil t))
         (and (not (window-in-direction 'right nil t))
              (window-in-direction 'left nil t)))
     1 t))

  (defun my-move-splitter-up (arg)
    "Move the bottom edge upward.

Resizing the bottom edge is not possible if there is no window
below. If there is a window above, then the top edge is resized
instead.

Since this command is meant to be repeatable, it preserves the
prefix argument so that it's not necessary to keep setting the it
for each adjustment."
    (interactive "P")

    (universal-argument--preserve)

    (adjust-window-trailing-edge
     (or (and arg (window-in-direction 'above nil t))
         (and (not (window-in-direction 'below nil t))
              (window-in-direction 'above nil t)))
     -1))

  (defun my-move-splitter-down (arg)
    "Move the bottom edge downward.

Resizing the bottom edge is not possible if there is no window
below. If there is a window above, then the top edge is resized
instead.

Since this command is meant to be repeatable, it preserves the
prefix argument so that it's not necessary to keep setting the it
for each adjustment."
    (interactive "P")

    (universal-argument--preserve)

    (adjust-window-trailing-edge
     (or (and arg (window-in-direction 'above nil t))
         (and (not (window-in-direction 'below nil t))
              (window-in-direction 'above nil t)))
     1))

  (defun my-universal-argument-toggle ()
    "Toggle the universal argument.

If it was already set, unset it. Otherwise invoke
`universal-argument'."
    (interactive)

    (if current-prefix-arg
        (setq current-prefix-arg nil)
      (call-interactively #'universal-argument)))

  (with-eval-after-load 'hydra
    (eval-when-compile
      (require 'hydra))

    (defhydra my-window-resizer ()
      "Resize window."

      ("C-u" my-universal-argument-toggle "prefix")

      ("j" my-move-splitter-down "🡇")
      ("k" my-move-splitter-up "🡅")
      ("h" my-move-splitter-left "🡄")
      ("l" my-move-splitter-right "🡆")

      ("w" ace-window "ace")

      ("c" evil-window-delete "close")
      ("b" balance-windows "balance")

      ("q" nil "quit")
      ("," nil "quit")

      ("J" evil-window-down "go 🡇")
      ("K" evil-window-up "go 🡅")
      ("H" evil-window-left "go 🡄")
      ("L" evil-window-right "go 🡆")

      ("?" (my--hydra-cycle-verbosity 'my-window-resizer) "± verbosity"))

    (hydra-set-property 'my-window-resizer :verbosity 0)))

(use-package server
  :straight nil
  :defer t

  :init
  (setq server-auth-dir (my-cache-dir "server")))

(use-package profiler
  :straight nil
  :defer t

  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state #'profiler-report-mode 'emacs)))

(use-package browse-url
  :straight nil
  :defer t

  :init
  (defun my-browse-file-directory ()
    "Open the current file's directory however the OS would."
    (interactive)

    (if default-directory
        (browse-url-of-file (expand-file-name default-directory))
      (error "No `default-directory' to open"))))

(use-package edebug
  :straight nil

  :general
  (my-map
    "e e d" 'edebug-eval-defun)

  :init
  (add-hook 'edebug-mode-hook (my-create-evil-toggle-for-mode edebug-mode))

  (defconst my--defun-regexp
    "(\\s-*\\(defun\\|defmacro\\|use-package\\)\\s-+"
    "Regexp to find a defun or defmacro definition.")

  :config
  ;; edebug-eval-defun is advised to override eval-defun, so we need to advise
  ;; edebug-eval-defun otherwise if we advised eval-defun, our advice would be
  ;; side-stepped once the edebug-eval-defun autoload kicks in and overrides
  ;; eval-defun, meaning our advice would never run.
  (define-advice edebug-eval-defun
      (:around (old-func edebug-it) my--eval-use-package-defun)
    "Allow evaluation of indented defuns, particularly those in use-package forms."

    (save-excursion
      (if (or (looking-at-p my--defun-regexp)
              (search-backward-regexp my--defun-regexp))
          (progn
            (mark-sexp)

            (save-restriction
              (narrow-to-region (point) (mark))
              (funcall old-func edebug-it))

            (deactivate-mark))
        (funcall old-func edebug-it)))))

(use-package pp
  :straight nil
  :defer t

  :init
  (advice-add 'pp-eval-last-sexp :before #'my--exit-insert-state)
  (advice-add 'pp-macroexpand-last-sexp :before #'my--exit-insert-state)

  (define-advice eval-print-last-sexp
      (:around (old-func &optional eval-last-sexp-arg-internal) print-after-paren)
    "Make sure to print sexp after the closing parenthesis when in Evil mode."
    (interactive "P")

    (require 'smartparens)
    (require 'on-parens)

    (if (evil-normal-state-p)
        (progn
          (evil-append 1)
          (funcall old-func eval-last-sexp-arg-internal)
          (evil-normal-state))
      (funcall old-func eval-last-sexp-arg-internal)))

  (define-advice pp-display-expression
      (:after (expression out-buffer-name) auto-select-window)
    "Auto-select the *Pp Eval Output* window.

Also bind `q' to `quit-window'."
    (-when-let (window (get-buffer-window out-buffer-name))
      (select-window window)

      (with-current-buffer out-buffer-name
        (bind-local :states '(normal emacs) "q" 'quit-window)))))

(use-package fringe
  :straight nil

  :init
  (setq-default fringe-indicator-alist
        '((truncation left-arrow right-arrow)
          (continuation left-curly-arrow nil)
          (overlay-arrow . right-triangle)
          (up . up-arrow)
          (down . down-arrow)
          (top top-left-angle top-right-angle)
          (bottom bottom-left-angle bottom-right-angle top-right-angle top-left-angle)
          (top-bottom left-bracket right-bracket top-right-angle top-left-angle)
          (empty-line . empty-line)
          (unknown . question-mark))))

(use-package profiler
  :straight nil

  :general
  (my-map
    "e p" '(:ignore t :which-key "profiler")
    "e p p" 'profiler-start
    "e p r" 'profiler-report
    "e p s" 'profiler-stop))

(use-package autorevert
  :straight nil
  :defer t

  :hook
  ((global-auto-revert-mode . my--enable-notify)
   (find-file . my--disable-auto-revert-vc-when-remote))

  :init
  (setq-default auto-revert-check-vc-info t)

  ;; emacs 25 disables notify for global-auto-revert
  ;; because there can be a problem on OSX where it may use too many resources
  ;; or something
  (defun my--enable-notify ()
    "Enable auto-revert-use-notify unless on macos."

    (unless (eq system-type 'darwin)
      (setq-local auto-revert-use-notify t)))

  (defun my--disable-auto-revert-vc-when-remote ()
    "Disable auto-revert-mode for remote files."

    (when (and buffer-file-name (file-remote-p buffer-file-name))
      (setq-local auto-revert-check-vc-info nil)))

  (global-auto-revert-mode 1))

(use-package delsel
  :straight nil
  :defer t

  :init
  (delete-selection-mode 1))

(use-package iso-transl
  :straight nil

  :general
  (:keymaps 'iso-transl-ctl-x-8-map
   "<right>" "→"
   "<left>" "←"
   "." "…"
   "-" "—"
   "n" "ñ"))

(use-package winner
  :straight nil

  :general
  (my-map :infix "w"
    "<left>" 'winner-undo
    "C-S-h" 'winner-undo
    "<right>" 'winner-redo
    "C-S-l" 'winner-redo)

  :init
  (setq winner-dont-bind-my-keys t)

  (winner-mode 1))

(use-package newcomment
  :straight nil
  :defer t

  :init
  (define-advice comment-indent-new-line
      (:after (&optional soft) at-least-one-space)
    "Ensure that at least one space is added after the comment-start."

    (let* ((comment-start (s-trim-right comment-start))
           (start-pattern (regexp-quote comment-start))
           (in-comment (looking-back start-pattern (- (point) (length comment-start))))
           (not-padded (not (looking-back " "  (- (point) 1)))))
      (when (and in-comment not-padded)
        (insert " ")))))

(use-package mule-util
  :straight nil
  :defer nil

  :init
  (setq truncate-string-ellipsis "…"))

(use-package menu-bar
  :straight nil

  :general
  ("<f10>" 'toggle-menu-bar-mode-from-frame)

  :init
  (menu-bar-mode -1))

(use-package tool-bar
  :straight nil
  :defer t

  :init
  (tool-bar-mode -1))

(use-package scroll-bar
  :straight nil

  :general
  ("<f9>" 'my-toggle-scroll-bar)

  :init
  (defun my-toggle-scroll-bar ()
    "Toggle visibility of the scroll bar for the current window."
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
  :straight nil
  :defer t

  :init
  (blink-cursor-mode 0))

(use-package paren
  :straight nil
  :defer t

  :init
  (setq show-paren-delay 0
        show-paren-when-point-inside-paren t)

  (show-paren-mode 1))

(use-package which-func
  :straight nil

  :init
  (which-function-mode 1))

(use-package saveplace
  :straight nil
  :defer t
  :defines save-place-file

  :init
  (setq-default save-place t)
  (setq save-place-file (my-cache-dir "saved-places")))

(use-package smerge-mode
  :straight nil

  :general
  (my-map :infix "g c" :keymaps 'smerge-mode-map
    "" '(:ignore t :which-key "conflict")
    "n" 'smerge-next
    "p" 'smerge-prev
    "k" 'smerge-keep-current
    "a" 'smerge-keep-all
    "u" 'smerge-keep-upper
    "l" 'smerge-keep-lower
    "e" 'smerge-ediff)

  :hook
  ;; attempt to start smerge, automatically disabling it if not relevant
  (find-file . smerge-start-session)

  :init
  (with-eval-after-load 'evil
    (add-hook 'smerge-mode-hook #'evil-normalize-keymaps)))

(use-package bookmark
  :straight nil
  :defer t
  :defines bookmark-default-file

  :init
  (setq bookmark-default-file (my-cache-dir "bookmarks")
        bookmark-save-flag 1
        bookmark-automatically-show-annotations t))

(use-package recentf
  :straight nil
  :defines recentf-save-file
  :defer t

  :init
  (setq recentf-save-file (my-cache-dir "recentf")
        recentf-max-saved-items 50)

  (recentf-mode 1))

(use-package savehist
  :straight nil
  :defer t

  :init
  (setq savehist-save-minibuffer-history 1
        savehist-file (my-cache-dir "history"))

  (savehist-mode 1))

(use-package ido
  :straight nil
  :defer t
  :defines ido-save-directory-list-file

  :init
  (setq ido-save-directory-list-file (my-cache-dir "ido.last")))

(use-package eshell
  :straight nil
  :defer t
  :defines eshell-directory

  :init
  (setq eshell-directory (my-cache-dir "eshell")))

(use-package apropos
  :straight nil
  :defer t
  :defines apropos-do-all

  :init
  (setq apropos-do-all t))

;; NOTE gdb also requires argument `-i=mi`
(use-package gdb-mi
  :straight nil
  :defer t
  :defines
  gdb-many-windows
  gdb-show-main

  :init
  (setq gdb-many-windows t
        gdb-show-main t))

(use-package shell
  :straight nil
  :defer t
  :defines explicit-shell-file-name

  :init
  (setq explicit-shell-file-name "/usr/bin/zsh"))

(use-package whitespace
  :straight nil
  :defer t

  :init
  (setq whitespace-line-column nil

        whitespace-indentation-regexp
        '("^ *\\(\t+\\)[^\n]" . "^ *\\(\t+\\)[^\n]")

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
  :straight nil
  :defer t

  :init
  (setq js-indent-level 2
        js-switch-indent-offset 2))

(use-package sh-script
  :straight nil
  :functions sh-set-shell
  :mode ("\\.zsh\\(rc\\)?\\'" . sh-mode)

  :hook
  (sh-mode . my--zsh-hook)

  :init
  (setq sh-learn-basic-offset t
        sh-basic-offset 2
        sh-indentation 2)

  (defun my--zsh-hook ()
    (when (string-match "\\.zsh\\(rc\\)?\\'" buffer-file-name)
      (sh-set-shell "zsh"))))

(use-package python
  :straight nil
  :defer t

  :hook
  (python-mode . my--python-hook)

  :init
  (defun my--python-hook ()
    (setq fill-column 79)))

(use-package semantic
  :straight nil
  :defer t
  :defines
  semanticdb-default-save-directory

  :hook
  (c++-mode . semantic-mode))

(use-package semantic/db-mode
  :straight nil
  :after semantic
  :functions
  global-semanticdb-minor-mode
  global-semantic-idle-scheduler-mode

  :init
  (setq semanticdb-default-save-directory (my-cache-dir "semanticdb"))

  :config
  (global-semanticdb-minor-mode 1))

(use-package semantic/idle
  :straight nil
  :after semantic

  :config
  (global-semantic-idle-scheduler-mode 1))

(use-package cc-mode
  :straight nil
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
    "Insert C pre-processor include guards."
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
  :straight nil
  :defer t

  :config
  ;; Add googletest TESTs to imenu.
  (add-to-list 'cc-imenu-c++-generic-expression
               '("Test" "^TEST\\(_F\\)?(\\([^)]+\\))" 2) t))

(use-package imenu
  :straight nil
  :defer t

  :init
  (setq imenu-auto-rescan t))

(use-package lisp-mode
  :straight nil
  :defer t
  :functions my--lisp-indent-function
  :defines calculate-lisp-indent-last-sexp

  :hook
  (emacs-lisp-mode . my--use-my-lisp-indent-function)

  :init
  ;; Fixes list indentation.
  ;;
  ;; Before:
  ;;   (:foo bar
  ;;         :baz qux)
  ;; After:
  ;;   (:foo bar
  ;;    :baz qux)
  ;;
  ;; Source:
  ;; https://github.com/Fuco1/.emacs.d/blob/master/site-lisp/my-redef.el#L18
  (defun my--lisp-indent-function (indent-point state)
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

  (defun my--use-my-lisp-indent-function ()
    (setq-local lisp-indent-function #'my--lisp-indent-function)))

(use-package diff
  :straight nil
  :defer t

  :init
  (defun my-diff ()
    "Generate unified diff of current buffer with backing file."
    (interactive)

    (diff-buffer-with-file (current-buffer))))

(use-package ediff
  :straight nil
  :functions
  ediff-toggle-wide-display

  :general
  (my-map "b d" 'ediff-current-file)

  :hook
  ((ediff-before-setup . my--ediff-save-window-and-frame-configuration)
   (ediff-prepare-buffer . my--ediff-disable-problem-modes)
   (ediff-startup . my-turn-on-fullscreen)

   (ediff-cleanup . my--ediff-janitor))

  :init
  (setq ediff-custom-diff-options "-u"
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain)

  ;; NOTE
  ;; doing M-x ediff-show-diff-output from ediff-current-file doesn't work
  ;; https://emacs.stackexchange.com/questions/22090/

  (defvar my--ediff-previous-window-configuration nil
    "Stores the window configuration from before ediff was invoked.")

  (defvar my--ediff-previous-frame-configuration nil
    "Stores the frame configuration from before ediff was invoked.")

  (defvar-local my--ediff-problem-modes
    '(hs-minor-mode fci-mode visual-line-mode whitespace-mode)
    "Modes that are known to cause issues with ediff.

If any of these modes were enabled prior to entering ediff,
they'll be disabled and then re-enabled on exit.")

  (defvar-local my--ediff-disabled-problem-modes '()
    "Stores any problem modes that were disabled prior to entering ediff.")

  (defun my--ediff-disable-wide-display ()
    (require 'ediff-util)
    (when ediff-wide-display-p
      (ediff-toggle-wide-display)))

  (defun my--ediff-save-window-and-frame-configuration ()
    (setq my--ediff-previous-window-configuration (current-window-configuration)
          my--ediff-previous-frame-configuration (current-frame-configuration)))

  (defun my--ediff-restore-window-and-frame-configuration ()
    (set-frame-configuration my--ediff-previous-frame-configuration)
    (set-window-configuration my--ediff-previous-window-configuration)
    (setq my--ediff-previous-window-configuration nil
          my--ediff-previous-frame-configuration nil))

  (defun my--ediff-disable-problem-modes ()
    (dolist (mode my--ediff-problem-modes)
      (when (and (boundp mode) (symbol-value mode))
        (funcall mode -1)
        (push mode my--ediff-disabled-problem-modes))))

  (defun my--ediff-restore-problem-modes ()
    (dolist (mode my--ediff-disabled-problem-modes)
      (funcall mode +1))

    (setq-local my--ediff-disabled-problem-modes '()))

  (defun my--ediff-quit ()
    (my--ediff-restore-window-and-frame-configuration)
    (my--ediff-restore-problem-modes)
    (my--ediff-disable-wide-display))

  (defun my--ediff-janitor ()
    (ediff-janitor nil nil))

   (add-hook 'ediff-suspend-hook #'my--ediff-quit 'append)
   (add-hook 'ediff-quit-hook #'my--ediff-quit 'append))

(use-package minibuffer
  :straight nil
  :defer t

  :init
  (add-to-list 'completion-styles 'substring t)
  (add-to-list 'completion-styles 'initials t))

(use-package elec-pair
  :straight nil
  :defer t

  :hook
  (minibuffer-setup . my--minibuffer-setup-hook)

  :init
  ;; Disable electric-pair-mode in the minibuffer unless we're in
  ;; eval-expression.
  (defun my--electric-pair-inhibit-in-minibuffer (char)
    'inhibit)

  (defun my--minibuffer-setup-hook ()
    (when (not (eq this-command 'pp-eval-expression))
      (setq-local electric-pair-inhibit-predicate
                  #'my--electric-pair-inhibit-in-minibuffer)))

  (electric-pair-mode 1))

(use-package sgml-mode
  :straight nil
  :defer t

  :init
  (setq sgml-basic-offset 2))

(use-package css-mode
  :straight nil
  :defer t

  :init
  (setq css-indent-offset 2))

(use-package bug-reference
  :straight nil
  :defer t

  :hook
  ((prog-mode . bug-reference-prog-mode)
   (text-mode . bug-reference-mode))

  :init
  (setq bug-reference-bug-regexp "\\(\
[Ii]ssue ?#\\|\
[Bb]ug ?#?\\|\
[Pp]atch ?#\\|\
RFE ?#\\|\
GH-\\|\
PR \\(?:[a-z-+_]+/\\(?:[a-z-+_]+\\)?\\)?#?\
\\)\\([0-9]+\\(?:#[0-9]+\\)?\\)"))

(use-package goto-addr
  :straight nil
  :defer t

  :hook
  ((prog-mode . goto-address-prog-mode)
   (text-mode . goto-address-mode)))

(use-package org-table
  :straight nil
  :defer t

  :commands orgtbl-mode
  :functions
  org-at-table-p
  org-table-hline-and-move

  :general
  (:keymaps 'orgtbl-mode-map
   "C-c RET" 'my-orgtbl-ret)

  :config
  (defun my-orgtbl-ret ()
    "Insert hline-and-move if in an org table, else pass-through C-c RET."
    (interactive)

    ;; If we're within an org table then insert an hline and move, otherwise
    ;; make it appear as if orgtbl-mode is off and re-run the key sequence
    ;; interactively, effectively passing it through to whatever binding it may
    ;; have outside of orgtbl-mode.
    (if (org-at-table-p)
        (org-table-hline-and-move)
      (let (orgtbl-mode)
        (-when-let (binding (key-binding (this-command-keys-vector)))
          (call-interactively binding))))))

(use-package dired
  :straight nil
  :defer t

  :init
  (setq dired-auto-revert-buffer t
        dired-listing-switches "-alhF")

  (when (eq system-type 'darwin)
    (setq insert-directory-program "gls"))

  (when (or (memq system-type '(gnu gnu/linux))
            (string= (file-name-nondirectory insert-directory-program) "gls"))
    (setq dired-listing-switches
          (concat dired-listing-switches " --group-directories-first -v")))

  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state #'dired-mode 'emacs)))

(use-package dired-x :straight nil)

(use-package simple
  :straight nil
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

  :hook
  ((prog-mode . visual-line-mode)
   (prog-mode . auto-fill-mode))

  :init
  (customize-set-variable 'visual-line-fringe-indicators '(left-curly-arrow nil))
  (setq next-error-recenter '(4))

  (setq-default comment-auto-fill-only-comments t)

  (defun my-next-error (&optional n reset)
    "Dispatch to flycheck or standard emacs error."
    (interactive "P")

    (if (and (bound-and-true-p flycheck-mode)
             (not (get-buffer-window "*compilation*")))
        (call-interactively #'flycheck-next-error)
      (call-interactively #'next-error)))

  (defun my-previous-error (&optional n reset)
    "Dispatch to flycheck or standard emacs error."
    (interactive "P")

    (if (and (bound-and-true-p flycheck-mode)
             (not (get-buffer-window "*compilation*")))
        (call-interactively #'flycheck-previous-error)
      (call-interactively #'previous-error)))

  (column-number-mode 1))

(use-package ansi-color
  :straight nil
  :defer t
  :functions
  ansi-color-apply-on-region

  :hook
  (compilation-filter . my--colorize-compilation-buffer)

  :init
  (defun my--colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max)))))

(use-package compile
  :straight nil
  :defer t

  :hook
  (compilation-mode . my--compilation-mode-hook)

  :init
  (setq compilation-scroll-output 'first-error
        compilation-ask-about-save nil
        compilation-skip-threshold 0
        compilation-always-kill t)

  (defun my--compilation-mode-hook ()
    (setq-local truncate-lines nil)
    (setq-local truncate-partial-width-windows nil)))

(use-package hl-line
  :straight nil
  :defer t

  :hook
  (prog-mode . hl-line-mode)

  :init
  (setq hl-line-sticky-flag t))

(use-package help-mode
  :straight nil

  :general
  (:keymaps 'help-mode-map
    "[" 'help-go-back
    "]" 'help-go-forward

    "h" nil
    "l" nil
    "0" nil)

  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state #'help-mode 'normal)))

(use-package sql
  :straight nil
  :defer t

  :init
  (add-hook 'sql-mode-hook #'sql-highlight-postgres-keywords)
  (add-hook 'sql-mode-hook #'hl-todo-mode))

(use-package hideshow
  :straight nil
  :defer t

  :hook
  (prog-mode . hs-minor-mode))

(use-package man
  :straight nil
  :defer t

  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state #'Man-mode 'normal)))

(use-package ispell
  :straight nil

  :init
  (when (executable-find "hunspell")
    (setq ispell-program-name "hunspell"
          ispell-dictionary "en_US"
          ispell-really-hunspell t))

  :config
  (ispell-set-spellchecker-params))

(use-package flyspell
  :straight nil

  :functions
  push-mark-no-activate
  flyspell-overlay-p
  flyspell-goto-next-error
  my-flyspell-goto-previous-error

  :general
  (:keymaps 'flyspell-mode-map
   "C-c $" nil)

  :hook
  ((text-mode . flyspell-mode)
   (prog-mode . flyspell-prog-mode))

  :init
  (setq flyspell-issue-message-flag nil)

  :config
  (defun my-flyspell-goto-previous-error (arg)
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
