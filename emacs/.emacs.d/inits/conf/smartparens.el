(require 'use-package)
(require 'general)

(use-package smartparens
  :diminish smartparens-mode

  :general
  (:keymaps 'smartparens-mode-map
    "M-S" 'sp-split-sexp
    "M-J" 'sp-join-sexp)

  (:keymaps 'smartparens-mode-map
   :states 'normal
    "> )" 'my-move-closing-paren-forward
    "< )" 'my-move-closing-paren-backward
    "> (" 'my-move-opening-paren-forward
    "< (" 'my-move-opening-paren-backward

    "< u" 'sp-splice-sexp-killing-backward
    "> u" 'sp-splice-sexp-killing-forward

    "< d" 'my-delete-sexp-backward
    "> d" 'my-delete-sexp-forward

    "< e" 'my-move-symbol-backward
    "> e" 'my-move-symbol-forward

    "< i" 'my-insert-before-form
    "> i" 'my-insert-after-form

    "< f" 'my-move-form-backward
    "> f" 'my-move-form-forward)

  :init
  (setq sp-show-pair-from-inside nil

        sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil

        sp-autoinsert-pair nil
        sp-autodelete-pair nil
        sp-autodelete-closing-pair nil
        sp-autodelete-opening-pair nil
        sp-autoskip-closing-pair nil
        sp-autoskip-opening-pair nil
        sp-cancel-autoskip-on-backward-movement nil
        sp-autodelete-wrap nil
        sp-autowrap-region nil)

  (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)

  :config
  (sp-use-paredit-bindings)

  (with-eval-after-load 'evil
    (use-package on-parens
      :after smartparens
      :config
      (eval-when-compile
        (require 'smartparens))

      ;; https://github.com/tpope/vim-sexp-mappings-for-regular-people

      (defun my-evil-goto-char (pos)
        (when (evil-normal-state-p) (decf pos))
        (goto-char pos))

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
        (sp-kill-sexp '(-4)))

      (defun my-delete-sexp-forward ()
        (interactive)
        ;; FIXME
        ;; doesn't work when point is on/before closing brace
        ;; so we'll hack it by simply using forward-char
        ;; even on-parens-kill-sexp doesn't seem to work
        (forward-char)
        (sp-kill-sexp '(4)))

      (defun my-sp-get-current-non-string-sexp (pos)
        "get the enclosing, non-string sexp"
        (let ((current-sexp (sp-get-sexp)))
          (if (or (eq pos (sp-get current-sexp :beg))
                  (eq pos (sp-get current-sexp :end)))
              current-sexp
            (let* ((enclosing-sexp (sp-get-enclosing-sexp))
                   (op (sp-get enclosing-sexp :op))
                   (end (sp-get enclosing-sexp :end)))
              (when enclosing-sexp
                (if (string-equal op "\"")
                    (my-sp-get-current-non-string-sexp (goto-char end))
                  enclosing-sexp))))))

      (defun my-sp-end-of-current-sexp (pos)
        "jump to the end of the current, non-string sexp"
        (interactive "d")

        (let ((end (sp-get (my-sp-get-current-non-string-sexp pos) :end)))
          (when end
            (my-evil-goto-char end))))

      (defmacro my-save-position (&rest body)
        "restore column and form-relative line number"
        `(let* ((column (current-column))
                (pos (point))
                (begin-line (line-number-at-pos pos)))
           (my-sp-end-of-current-sexp pos)
           (when (evil-normal-state-p) (forward-char))

           (let* ((end-line (line-number-at-pos (point))))
             ,@body
             (forward-line (- begin-line end-line))
             (move-to-column column))))

      (defun move-form-forward (pos &optional arg)
        "move a form forward"
        (interactive "d *p")

        (my-save-position
          (sp-transpose-sexp)))

      (defun move-form-backward (pos &optional arg)
        "move a form backward"
        (interactive "d *p")

        (my-save-position
         (sp-transpose-sexp -1)))

      (defun my-move-symbol-backward (&optional arg)
        "move a symbol backward"
        (interactive "*p")

        (unless (looking-at-p "\)\\|\(")
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

      (defun my-insert-before-form ()
        "jump to the beginning of the sexp and go into insert mode"
        (interactive)
        (sp-beginning-of-sexp)
        (evil-insert 0))

      (defun my-insert-after-form ()
        "jump to the end of the sexp and go into insert mode"
        (interactive)
        (sp-end-of-sexp)
        (evil-insert 0))

      (defun my-move-form-backward ()
        (interactive)
        (sp-restrict-to-object
         'sp-prefix-pair-object 'move-form-backward))

      (defun my-move-form-forward ()
        (interactive)
        (sp-restrict-to-object
         'sp-prefix-pair-object 'move-form-forward)))))

(provide 'conf/smartparens)
