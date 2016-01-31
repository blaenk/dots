(require 'use-package)

(use-package evil
  :defines blaenk/evil-join
  :init
  (setq evil-want-C-w-in-emacs-state t)
  (setq evil-text-object-change-visual-type nil)

  ;; TODO check if these should all be in this
  (setq evil-search-module 'evil-search)
  ;; (setq evil-cross-lines t)
  ;; TODO show trailing whitespace in combination with this?
  ;; (setq evil-move-cursor-back nil)
  (setq-default evil-symbol-word-search t)
  ;; TODO necessary?
  (setq-default evil-shift-width 2)

  (defun blaenk/evil--real-function (fun)
    "Figure out the actual symbol behind a function.
Returns a different symbol if FUN is an alias, otherwise FUN."
    (let ((symbol-function (symbol-function fun)))
      (if (symbolp symbol-function)
          symbol-function
        fun)))

  (defun blaenk/evil--derived-mode-p (mode modes)
    (let ((parent (blaenk/evil--real-function mode)))
      (while (and parent (not (memq parent modes)))
        (setq parent (blaenk/evil--real-function (get parent 'derived-mode-parent))))
      parent))

  (with-eval-after-load 'company
    (defun blaenk/evil-company (arg) (company-complete))

    (setq evil-complete-next-func 'blaenk/evil-company)
    (setq evil-complete-previous-func 'blaenk/evil-company))

  (with-eval-after-load 'evil-core
    (defun evil-initial-state (mode &optional default)
      "Return the Evil state to use for MODE.
Returns DEFAULT if no initial state is associated with MODE.
The initial state for a mode can be set with
`evil-set-initial-state'."
      (let (state modes)
        (catch 'done
          (dolist (entry (nreverse (evil-state-property t :modes)) default)
            (setq state (car entry)
                  modes (symbol-value (cdr entry)))
            (when (or (memq mode modes)
                      (blaenk/evil--derived-mode-p mode modes))
              (throw 'done state)))))))

  (setq evil-want-C-w-delete t)
  (setq evil-want-C-u-scroll t)
  (setq evil-default-state 'emacs)
  (setq evil-normal-state-modes
        '(text-mode
          prog-mode
          fundamental-mode
          css-mode
          conf-mode
          TeX-mode
          LaTeX-mode
          yaml-mode
          ))
  (setq evil-emacs-state-modes
        '(help-mode
          term-mode
          undo-tree-visualizer-mode))

  (add-hook 'with-editor-mode-hook 'evil-insert-state)

  :config
  ;; see skip-syntax-forward ^<
  ;; (skip characters whose syntax is now comment start)
  (eval-when-compile
    (require 'evil-macros)
    (require 'evil-types))
  (evil-define-operator blaenk/evil-join (beg end)
    "Join the selected lines."
    :motion evil-line
    (let ((count (count-lines beg end)))
      (when (> count 1)
        (setq count (1- count)))
      (dotimes (var count)
        (join-line 1)
        ;; remove comment delimiters
        (when (nth 4 (syntax-ppss))
          (forward-char)
          (while (looking-at
                  (concat
                   "\\s<"
                   "\\|" (substring comment-start 0 1) "\\|"
                   "\\s-"))
            (delete-char 1))))))

  (bind-key "J" 'blaenk/evil-join evil-normal-state-map)

  (eval-when-compile
    (require 'solarized))
  (solarized-with-color-variables 'light
    (setq evil-normal-state-cursor `(,blue-l box))
    (setq evil-insert-state-cursor `(,green-l box))
    (setq evil-visual-state-cursor `(,magenta-l box))
    (setq evil-replace-state-cursor `(,red-l (hbar . 4)))
    (setq evil-operator-state-cursor `((hbar . 6)))
    (setq evil-emacs-state-cursor `(,red-l box)))

  (with-eval-after-load 'ggtags
    (evil-make-overriding-map ggtags-mode-map)

    ;; force update evil keymaps after ggtags-mode loaded
    (add-hook 'ggtags-mode-hook #'evil-normalize-keymaps))

  (defun blaenk/evil-maybe-remove-spaces ()
    (unless (memq this-command '(evil-open-above evil-open-below))
      (remove-hook 'post-command-hook 'blaenk/evil-maybe-remove-spaces)
      (when (not (evil-insert-state-p))
        (delete-char 1))))

  (defun blaenk/evil-open-line ()
    (interactive)
    (end-of-visual-line)
    (if (elt (syntax-ppss) 4)
        (progn
          (comment-indent-new-line)
          (evil-insert-state 1)

          (add-hook 'post-command-hook #'blaenk/evil-maybe-remove-spaces)

          (when evil-auto-indent
            (indent-according-to-mode))
          (setq this-command 'evil-open-below))
      (progn
        (evil-open-below 1)
        (setq this-command 'evil-open-below))))

  ;; if the point is in a comment that has non-whitespace content, delete up
  ;; until the beginning of the comment. if already at the beginning of the
  ;; comment, delete up to the indentation point. if already at the indentation
  ;; point, delete to the beginning of the line
  (defun blaenk/kill-line ()
    (interactive)
    (let* ((starts
            (-non-nil
             ;; add comment-start-regexps to this as needed
             `("\\s<"
               ,(regexp-quote (s-trim-right comment-start))
               ,(bound-and-true-p c-comment-start-regexp))))
           (comment-starts (s-join "\\|" starts))
           (start-re (concat "\\(" comment-starts "\\)")))
      (if (and
           ;; in a comment
           (elt (syntax-ppss) 4)
           ;; we're in a single line comment
           (looking-back (concat start-re ".+")
                         (line-beginning-position))
           ;; not right after starting delimiter
           (not (looking-back (concat start-re "\\s-?")
                              (line-beginning-position))))
          (let ((beg (point)))
            (beginning-of-visual-line)
            ;; go after comment position
            (re-search-forward
             (concat ".+" start-re "\\s-?")
             (line-end-position))
            ;; kill rest of line
            (kill-region (point) beg))
        (if (looking-back "^[[:space:]]+")
            ;; kill entire line
            (kill-line 0)
          ;; kill up to indentation point
          (let ((beg (point)))
            (when (not (equal beg (line-beginning-position)))
              (back-to-indentation)
              (kill-region beg (point))))))))

  (bind-key "z =" 'helm-flyspell-correct evil-normal-state-map)

  (bind-key "[ s" 'flyspell-goto-previous-error evil-normal-state-map)
  (bind-key "] s" 'flyspell-goto-next-error evil-normal-state-map)

  (bind-key "[ S" 'check-previous-spelling-error evil-normal-state-map)
  (bind-key "] S" 'check-next-spelling-error evil-normal-state-map)

  (bind-key "<S-return>" 'comment-indent-new-line evil-insert-state-map)

  (bind-key "C-l" 'move-end-of-line evil-insert-state-map)

  (bind-key "C-u" 'blaenk/kill-line evil-insert-state-map)
  (bind-key "C-u" 'evil-scroll-up evil-normal-state-map)

  (bind-key "g p" 'exchange-point-and-mark evil-normal-state-map)

  (bind-key "C-w q" 'evil-window-delete evil-normal-state-map)

  (defun blaenk/evil-insert-mode-paste ()
    (interactive)
    (evil-paste-before 1)
    (forward-char))

  (bind-key "C-y" 'blaenk/evil-insert-mode-paste evil-insert-state-map)

  (bind-key "j" 'evil-next-visual-line evil-normal-state-map)
  (bind-key "k" 'evil-previous-visual-line evil-normal-state-map)

  (bind-key "<kp-add>" 'evil-numbers/inc-at-pt evil-normal-state-map)
  (bind-key "<kp-add>" 'evil-numbers/inc-at-pt evil-visual-state-map)

  (bind-key "<kp-subtract>" 'evil-numbers/dec-at-pt evil-normal-state-map)
  (bind-key "<kp-subtract>" 'evil-numbers/dec-at-pt evil-visual-state-map)

  ;; unmap these so they could be used as prefix keys
  ;; this is useful for smartparens
  (unbind-key "<" evil-normal-state-map)
  (unbind-key ">" evil-normal-state-map)

  ;; still able to shift things in normal mode
  (bind-key "< <" 'evil-shift-left-line evil-normal-state-map)
  (bind-key "> >" 'evil-shift-right-line evil-normal-state-map)

  (evil-define-operator visual-shift-left (beg end type)
    "shift text to the left"
    :keep-visual t
    :motion evil-line
    :type line
    (interactive "<r><vc>")
    (evil-shift-left beg end)
    ;; TODO necessary?
    (evil-normal-state)
    (evil-visual-restore))

  (evil-define-operator visual-shift-right (beg end type)
    "shift text to the right"
    :keep-visual t
    :motion evil-line
    :type line
    (interactive "<r><vc>")
    (evil-shift-right beg end)
    (evil-normal-state)
    (evil-visual-restore))

  (bind-key "<" 'visual-shift-left evil-visual-state-map)
  (bind-key ">" 'visual-shift-right evil-visual-state-map)

  ;; (with-eval-after-load 'buffer-move
  ;;   (define-key evil-window-map (kbd "m k") 'buf-move-up)
  ;;   (define-key evil-window-map (kbd "m j") 'buf-move-down)
  ;;   (define-key evil-window-map (kbd "m h") 'buf-move-left)
  ;;   (define-key evil-window-map (kbd "m l") 'buf-move-right))

  (evil-mode 1)

  (defun blaenk/evil-open-in-between ()
    (interactive)
    (end-of-line)
    (newline)
    (evil-open-above 1)
    (setq this-command 'evil-open-below))

  (defun blaenk/clear-search ()
    (interactive)
    (evil-ex-nohighlight)
    (force-mode-line-update))

  (with-eval-after-load 'bind-map
    (bind-keys :map blaenk/leader-map
      ("o" . blaenk/evil-open-in-between)
      ("l" . blaenk/clear-search)))

  (use-package evil-indent-plus
    :config
    (evil-indent-plus-default-bindings))

  (use-package evil-quickscope
    :config
    (global-evil-quickscope-mode 1))

  (use-package evil-textobj-anyblock
    :config
    (bind-key "b" 'evil-textobj-anyblock-inner-block evil-inner-text-objects-map)
    (bind-key "b" 'evil-textobj-anyblock-a-block evil-outer-text-objects-map))

  (use-package evil-anzu)

  (use-package evil-commentary
    :diminish evil-commentary-mode
    :config
    (evil-commentary-mode))

  (use-package evil-exchange
    :config
    (evil-exchange-install))

  (use-package evil-numbers)

  (use-package evil-surround
    :config
    (global-evil-surround-mode 1))

  (use-package evil-visual-mark-mode
    :config
    (bind-key "m" 'evil-visual-mark-mode blaenk/leader-map))

  (use-package evil-visualstar
    :config
    (global-evil-visualstar-mode))

  (use-package evil-args
    :config
    ;; bind evil-args text objects
    (bind-key "a" 'evil-inner-arg evil-inner-text-objects-map)
    (bind-key "a" 'evil-outer-arg evil-outer-text-objects-map)

    ;; bind evil-forward/backward-args
    (bind-key "L" 'evil-forward-arg evil-normal-state-map)
    (bind-key "H" 'evil-backward-arg evil-normal-state-map)

    (bind-key "L" 'evil-forward-arg evil-motion-state-map)
    (bind-key "H" 'evil-backward-arg evil-motion-state-map)

    ;; bind evil-jump-out-args
    (bind-key "K" 'evil-jump-out-args evil-normal-state-map)

    (defun evil-arg-swap-forward ()
      (interactive)
      (apply 'evil-exchange (evil-inner-arg))
      (call-interactively 'evil-forward-arg)
      (apply 'evil-exchange (evil-inner-arg)))

    (defun evil-arg-swap-backward ()
      (interactive)
      (apply 'evil-exchange (evil-inner-arg))
      (evil-forward-arg 1)
      (evil-backward-arg 2)
      (apply 'evil-exchange (evil-inner-arg)))

    (bind-key "< a" 'evil-arg-swap-backward evil-normal-state-map)
    (bind-key "> a" 'evil-arg-swap-forward evil-normal-state-map)
    ))
