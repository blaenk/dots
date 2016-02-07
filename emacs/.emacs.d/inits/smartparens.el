(require 'use-package)

(use-package paxedit
  :defer t)

(use-package on-parens)

(use-package smartparens
  :diminish smartparens-mode
  :defer t

  :init
  (setq sp-show-pair-from-inside t)
  (setq sp-show-pair-delay 0)
  (setq sp-highlight-pair-overlay nil)
  (setq sp-cancel-autoskip-on-backward-movement nil)

  (require 'smartparens-config)
  (sp-use-smartparens-bindings)

  (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
  (add-hook 'clojure-mode-hook 'smartparens-mode)
  (add-hook 'racket-mode-hook 'smartparens-mode)
  (add-hook 'scheme-mode-hook 'smartparens-mode)

  (bind-key "M-S" 'sp-split-sexp smartparens-mode-map)
  (bind-key "M-J" 'sp-join-sexp smartparens-mode-map)

  ;; TODO bind % to jump toggle matching pair

  (with-eval-after-load 'on-parens
    (with-eval-after-load 'evil
      ;; https://github.com/tpope/vim-sexp-mappings-for-regular-people

      (defun blaenk/evil-goto-char (pos)
        (when (evil-normal-state-p) (decf pos))
        (goto-char pos))

      ;; TODO evil-commentary delegating sp-comment wrapper?

      (defun blaenk/move-closing-paren-forward ()
        (interactive)
        (on-parens-forward-slurp)
        ;; get back on paren
        (sp-get (sp-get-enclosing-sexp) (blaenk/evil-goto-char :end)))

      (bind-key "> )" 'blaenk/move-closing-paren-forward evil-normal-state-map)

      (defun blaenk/move-closing-paren-backward ()
        (interactive)
        (on-parens-forward-barf)
        ;; get back on paren
        (sp-restrict-to-object 'sp-prefix-pair-object 'sp-backward-down-sexp))

      (bind-key "< )" 'blaenk/move-closing-paren-backward evil-normal-state-map)

      (defun blaenk/move-opening-paren-forward ()
        (interactive)
        (on-parens-backward-barf)
        ;; get back on paren
        (sp-restrict-to-object 'sp-prefix-pair-object 'sp-next-sexp))

      (bind-key "> (" 'blaenk/move-opening-paren-forward evil-normal-state-map)

      (defun blaenk/move-opening-paren-backward ()
        (interactive)
        (on-parens-backward-slurp)
        ;; get back on paren
        (sp-get (sp-get-enclosing-sexp) (blaenk/evil-goto-char (+ :beg 1))))

      (bind-key "< (" 'blaenk/move-opening-paren-backward evil-normal-state-map)

      ;; TODO
      ;; this should be turned off when smartparens is not on
      ;; NOTE can use evil-define-motion to create motions out of these
      ;; (define-key evil-normal-state-map (kbd "W") 'on-parens-forward-sexp)
      ;; (define-key evil-normal-state-map (kbd "E") 'on-parens-forward-sexp-end)
      ;; (define-key evil-normal-state-map (kbd "g E") 'on-parens-backward-sexp-end)
      ;; (define-key evil-normal-state-map (kbd "B") 'on-parens-backward-sexp)

      (bind-key "< u" 'sp-splice-sexp-killing-backward evil-normal-state-map)
      (bind-key "> u" 'sp-splice-sexp-killing-forward evil-normal-state-map)

      (defun blaenk/delete-sexp-backward ()
        (interactive)
        (sp-kill-sexp '(-4)))

      (bind-key "< d" 'blaenk/delete-sexp-backward evil-normal-state-map)

      (defun blaenk/delete-sexp-forward ()
        (interactive)
        ;; FIXME
        ;; doesn't work when point is on/before closing brace
        ;; so we'll hack it by simply using forward-char
        ;; even on-parens-kill-sexp doesn't seem to work
        (forward-char)
        (sp-kill-sexp '(4)))

      (bind-key "> d" 'blaenk/delete-sexp-forward evil-normal-state-map)

      (defun blaenk/sp-get-current-non-string-sexp (pos)
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
                    (blaenk/sp-get-current-non-string-sexp (goto-char end))
                  enclosing-sexp))))))

      (defun blaenk/sp-end-of-current-sexp (pos)
        "jump to the end of the current, non-string sexp"
        (interactive "d")

        (let ((end (sp-get (blaenk/sp-get-current-non-string-sexp pos) :end)))
          (when end
            (blaenk/evil-goto-char end))))

      (defmacro blaenk/save-position (&rest body)
        "restore column and form-relative line number"
        `(let* ((column (current-column))
                (pos (point))
                (begin-line (line-number-at-pos pos)))
           (blaenk/sp-end-of-current-sexp pos)
           (when (evil-normal-state-p) (forward-char))

           (let* ((end-line (line-number-at-pos (point))))
             ,@body
             (forward-line (- begin-line end-line))
             (move-to-column column))))

      (defun move-form-forward (pos &optional arg)
        "move a form forward"
        (interactive "d *p")

        (blaenk/save-position
          (sp-transpose-sexp)))

      (defun move-form-backward (pos &optional arg)
        "move a form backward"
        (interactive "d *p")

        (blaenk/save-position
         (sp-transpose-sexp -1)))

      (bind-key "< f" (sp-restrict-to-object-interactive 'sp-prefix-pair-object 'move-form-backward) evil-normal-state-map)

      (bind-key "> f" (sp-restrict-to-object-interactive 'sp-prefix-pair-object 'move-form-forward) evil-normal-state-map)

      (defun move-symbol-backward (&optional arg)
        "move a symbol backward"
        (interactive "*p")

        (unless (looking-at-p "\)\\|\(")
          (evil-forward-word-end)
          (evil-backward-WORD-begin))

        (sp-transpose-sexp)
        (backward-char)
        (on-parens-backward-sexp 2))

      (defun move-symbol-forward (&optional arg)
        "move a symbol forward"
        (interactive "*p")
        (on-parens-forward-sexp arg)
        (sp-transpose-sexp)
        (backward-char)
        (on-parens-backward-sexp arg))

      (bind-key "< s" 'move-symbol-backward evil-normal-state-map)
      (bind-key "> s" 'move-symbol-forward evil-normal-state-map)

      (defun insert-before-form ()
        "jump to the beginning of the sexp and go into insert mode"
        (interactive)
        (sp-beginning-of-sexp)
        (insert " ")
        (evil-backward-char)
        (evil-insert 0))

      (defun insert-after-form ()
        "jump to the end of the sexp and go into insert mode"
        (interactive)
        (sp-end-of-sexp)
        (evil-insert 0))

      (bind-key "< i" 'insert-before-form evil-normal-state-map)
      (bind-key "> i" 'insert-after-form evil-normal-state-map)
      )))
