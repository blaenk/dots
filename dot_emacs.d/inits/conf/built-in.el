(require 'straight)
(require 'use-package)
(require 'general)
(eval-when-compile
  (require 'conf/common))

(use-package elisp-mode
  :straight nil
  :defer t
  :init
  (define-advice eval-print-last-sexp
      (:around (old-func &optional eval-last-sexp-arg-internal) print-after-paren)
    "Make sure to print sexp after the closing parenthesis when in Evil mode."
    (interactive "P")

    (if (evil-normal-state-p)
        (progn
          (evil-append 1)
          (funcall old-func eval-last-sexp-arg-internal)
          (evil-normal-state))
      (funcall old-func eval-last-sexp-arg-internal))))

(use-package pp
  :straight nil
  :defer t

  :init
  (advice-add 'pp-eval-last-sexp :before #'my--exit-insert-state)
  (advice-add 'pp-macroexpand-last-sexp :before #'my--exit-insert-state)

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

  :general-config
  (my-map
    "e p" (cons "profiler" (make-sparse-keymap))
    "e p p" 'profiler-start
    "e p r" 'profiler-report
    "e p s" 'profiler-stop))

(use-package autorevert
  :straight nil
  :defer t

  :init
  (global-auto-revert-mode 1))

(use-package iso-transl
  :straight nil

  :general-config
  (:keymaps 'iso-transl-ctl-x-8-map
   "<right>" "→"
   "<left>" "←"
   "." "…"
   "-" "—"
   "n" "ñ"))

(use-package winner
  :straight nil

  :general-config
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

(use-package menu-bar
  :straight nil

  :general-config
  ("<f10>" 'toggle-menu-bar-mode-from-frame)

  ;; :init
  ;; (menu-bar-mode -1)
  )

;; (use-package tool-bar
;;   :straight nil
;;   :defer t

;;   :init
;;   (tool-bar-mode -1))

(use-package scroll-bar
  :straight nil

  :general-config
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

(use-package smerge-mode
  :straight nil

  :general-config
  (my-map :infix "g c" :keymaps 'smerge-mode-map
    "" (cons "conflict" (make-sparse-keymap))
    "n" 'smerge-next
    "p" 'smerge-prev
    "k" 'smerge-keep-current
    "a" 'smerge-keep-all
    "u" 'smerge-keep-upper
    "l" 'smerge-keep-lower
    "e" 'smerge-ediff)

  :init
  (with-eval-after-load 'evil
    (add-hook 'smerge-mode-hook #'evil-normalize-keymaps))
  )

(use-package whitespace
  :straight nil

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

  (add-hook 'prog-mode-hook #'whitespace-mode))

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

  :general-config
  (my-map "b d" 'my-diff)

  :init
  (defun my-diff ()
    "Generate unified diff of current buffer with backing file."
    (interactive)

    (diff-buffer-with-file (current-buffer)))

  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state #'diff-mode 'emacs)))

(use-package ediff
  :straight nil
  :functions
  ediff-toggle-wide-display

  :general-config
  (my-map "b e" 'ediff-current-file)

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
  :disabled t
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


(use-package simple
  :straight nil
  :defer t

  :general-config
  (:keymaps 'normal
   "[ c" 'my-previous-error
   "] c" 'my-next-error)

  (my-map
    "c" (cons "check" (make-sparse-keymap))
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
(use-package hl-line
  :straight nil
  :defer t

  :hook
  (prog-mode . hl-line-mode)

  :init
  (setq hl-line-sticky-flag t))

(use-package help-mode
  :straight nil

  :general-config
  (:keymaps 'help-mode-map
    "[" 'help-go-back
    "]" 'help-go-forward

    "h" nil
    "l" nil
    "0" nil)

  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state #'help-mode 'emacs)))

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

(use-package ispell
  :straight nil

  :init
  (when (executable-find "hunspell")
    (setq ispell-hunspell-dictionary-alist
          '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)
            ("en_US-large" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))

    (setq ispell-program-name "hunspell"
          ispell-really-hunspell t)

  :config
  (ispell-set-spellchecker-params))

(use-package flyspell
  :straight nil

  :functions
  push-mark-no-activate
  flyspell-overlay-p
  flyspell-goto-next-error
  my-flyspell-goto-previous-error

  :general-config
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
