(require 'use-package)
(require 'general)
(require 'dash)

(use-package smartparens
  :diminish smartparens-mode

  :general
  (:keymaps '(emacs-lisp-mode-map web-mode-map lisp-interaction-mode-map)
    "M-S" 'sp-split-sexp
    "M-J" 'sp-join-sexp)

  (:keymaps '(emacs-lisp-mode-map web-mode-map lisp-interaction-mode-map)
   :states 'normal
    "> )" 'my-move-closing-paren-forward
    "< )" 'my-move-closing-paren-backward

    "> (" 'my-move-opening-paren-forward
    "< (" 'my-move-opening-paren-backward

    "C-S-<backspace>" 'sp-splice-sexp-killing-around
    "< u" 'sp-splice-sexp-killing-backward
    "> u" 'sp-splice-sexp-killing-forward

    "< d" 'my-delete-sexp-backward
    "> d" 'my-delete-sexp-forward

    "< e" 'my-move-symbol-backward
    "> e" 'my-move-symbol-forward

    "< i" 'my-insert-before-form
    "> i" 'my-insert-after-form

    "< f" 'my-restricted-move-form-backward
    "> f" 'my-restricted-move-form-forward)

  :config
  (require 'smartparens-html)

  (with-eval-after-load 'evil
    (use-package on-parens
      :after smartparens

      :config
      (eval-when-compile
        (require 'smartparens))

      ;; This is all inspired by this awesome vim package:
      ;; https://github.com/tpope/vim-sexp-mappings-for-regular-people

      (defun my-evil-goto-char (pos)
        (goto-char
         (if (evil-normal-state-p)
             (1- pos)
           pos)))

      ;; TODO evil-commentary delegating sp-comment wrapper?

      (defun my-move-closing-paren-forward ()
        (interactive)
        (on-parens-forward-slurp)

        ;; get back on paren
        (sp-get (sp-get-enclosing-sexp) (my-evil-goto-char :end)))

      (defun my-move-closing-paren-backward ()
        (interactive)
        (on-parens-forward-barf)

        ;; get back on paren
        (sp-restrict-to-object 'sp-prefix-pair-object 'sp-backward-down-sexp))

      (defun my-move-opening-paren-forward ()
        (interactive)
        (on-parens-backward-barf)

        ;; get back on paren
        (sp-restrict-to-object 'sp-prefix-pair-object 'sp-next-sexp))

      (defun my-move-opening-paren-backward ()
        (interactive)
        (on-parens-backward-slurp)

        ;; get back on paren
        (sp-get (sp-get-enclosing-sexp) (my-evil-goto-char (+ :beg 1))))

      ;; TODO
      ;; this should be turned off when smartparens is not on
      ;; NOTE can use evil-define-motion to create motions out of these
      ;; (define-key evil-normal-state-map (kbd "W") 'on-parens-forward-sexp)
      ;; (define-key evil-normal-state-map (kbd "E") 'on-parens-forward-sexp-end)
      ;; (define-key evil-normal-state-map (kbd "g E") 'on-parens-backward-sexp-end)
      ;; (define-key evil-normal-state-map (kbd "B") 'on-parens-backward-sexp)

      (defun my-delete-sexp-backward ()
        (interactive)
        (on-parens-kill-sexp '(-4)))

      (defun my-delete-sexp-forward ()
        (interactive)
        ;; FIXME
        ;; doesn't work when point is on/before closing brace
        ;; so we'll hack it by simply using forward-char
        ;; even on-parens-kill-sexp doesn't seem to work
        (forward-char)
        (on-parens-kill-sexp '(4)))

      (defun my-sp-get-current-non-string-sexp (pos)
        "get the enclosing, non-string sexp"
        (let ((current-sexp (sp-get-sexp)))
          (if (or (eq pos (sp-get current-sexp :beg))
                  (eq pos (sp-get current-sexp :end)))
              current-sexp
            (-when-let* ((enclosing-sexp (sp-get-enclosing-sexp))
                         (op (sp-get enclosing-sexp :op))
                         (end (sp-get enclosing-sexp :end)))
              (if (string-equal op "\"")
                  (my-sp-get-current-non-string-sexp (goto-char end))
                enclosing-sexp)))))

      (defun my-sp-end-of-current-sexp (pos)
        "jump to the end of the current, non-string sexp"
        (interactive "d")

        (-when-let (end (sp-get (my-sp-get-current-non-string-sexp pos) :end))
          (my-evil-goto-char end)))

      (defmacro my-save-position (&rest body)
        "restore column and form-relative line number"
        `(let* ((column (current-column))
                (pos (point))
                (begin-line (line-number-at-pos pos)))
           (my-sp-end-of-current-sexp pos)

           (when (evil-normal-state-p)
             (forward-char))

           (let* ((end-line (line-number-at-pos (point))))
             ,@body
             (forward-line (- begin-line end-line))
             (move-to-column column))))

      (defun my-move-form-forward (pos &optional arg)
        "move a form forward"
        (interactive "d *p")

        (my-save-position
          (sp-transpose-sexp)))

      (defun my-move-form-backward (pos &optional arg)
        "move a form backward"
        (interactive "d *p")

        (my-save-position
         (sp-transpose-sexp -1)))

      (defun my-move-symbol-backward (&optional arg)
        "move a symbol backward"
        (interactive "*p")

        (unless (on-parens-on-delimiter?)
          (evil-forward-word-end)
          (evil-backward-WORD-begin))

        (sp-transpose-sexp)
        (backward-char)
        (on-parens-backward-sexp 2))

      (defun my-move-symbol-forward (&optional arg)
        "move a symbol forward"
        (interactive "*p")

        (on-parens-forward-sexp arg)
        (sp-transpose-sexp)
        (backward-char)
        (on-parens-backward-sexp arg))

      (defun my-insert-before-form (&optional arg)
        "jump to the beginning of the sexp and go into insert mode"
        (interactive "*p")

        (sp-beginning-of-sexp)
        (evil-insert-state))

      (defun my-insert-after-form (&optional arg)
        "jump to the end of the sexp and go into insert mode"
        (interactive "*p")

        (sp-end-of-sexp)
        (evil-insert-state))

      (defun my-restricted-move-form-backward (&optional arg)
        (interactive "*p")

        (sp-restrict-to-object
         'sp-prefix-pair-object 'my-move-form-backward))

      (defun my-restricted-move-form-forward (&optional arg)
        (interactive "*p")

        (sp-restrict-to-object
         'sp-prefix-pair-object 'my-move-form-forward)))))

(provide 'conf/smartparens)
