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
    (defun company-complete-lambda (arg) (company-complete))

    (setq evil-complete-next-func 'company-complete-lambda)
    (setq evil-complete-previous-func 'company-complete-lambda))

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

  (define-key evil-normal-state-map "J" 'blaenk/evil-join)

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

  ;; (define-key evil-normal-state-map (kbd "o") 'blaenk/evil-open-line)
  ;; (define-key evil-normal-state-map (kbd "O")
  ;;   (lambda ()
  ;;     (interactive)
  ;;     (if (eq (line-number-at-pos (point)) 1)
  ;;         (evil-open-above 1)
  ;;         (progn
  ;;           (previous-line)
  ;;           (blaenk/evil-open-line)))))

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
               ,c-comment-start-regexp)))
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

  (define-key evil-normal-state-map (kbd "z =") 'helm-flyspell-correct)

  (define-key evil-normal-state-map (kbd "[ s") 'flyspell-goto-previous-error)
  (define-key evil-normal-state-map (kbd "] s") 'flyspell-goto-next-error)

  (define-key evil-normal-state-map (kbd "[ S") 'check-previous-spelling-error)
  (define-key evil-normal-state-map (kbd "] S") 'check-next-spelling-error)

  (define-key evil-insert-state-map (kbd "<S-return>") 'comment-indent-new-line)

  (define-key evil-insert-state-map (kbd "C-l") 'move-end-of-line)

  (define-key evil-insert-state-map (kbd "C-u") 'blaenk/kill-line)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

  (define-key evil-normal-state-map (kbd "g p") 'exchange-point-and-mark)

  (define-key evil-normal-state-map (kbd "C-w q") 'evil-window-delete)

  (define-key evil-insert-state-map (kbd "M-v") 'evil-paste-before)

  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

  (define-key evil-normal-state-map (kbd "<kp-add>") 'evil-numbers/inc-at-pt)
  (define-key evil-visual-state-map (kbd "<kp-add>") 'evil-numbers/inc-at-pt)

  (define-key evil-normal-state-map (kbd "<kp-subtract>") 'evil-numbers/dec-at-pt)
  (define-key evil-visual-state-map (kbd "<kp-subtract>") 'evil-numbers/dec-at-pt)

  ;; unmap these so they could be used as prefix keys
  ;; this is useful for smartparens
  (define-key evil-normal-state-map (kbd "<") nil)
  (define-key evil-normal-state-map (kbd ">") nil)

  ;; still able to shift things in normal mode
  (define-key evil-normal-state-map (kbd "< <") 'evil-shift-left-line)
  (define-key evil-normal-state-map (kbd "> >") 'evil-shift-right-line)

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

  (define-key evil-visual-state-map (kbd "<") 'visual-shift-left)
  (define-key evil-visual-state-map (kbd ">") 'visual-shift-right)

  (require 'buffer-move)
  (define-key evil-window-map (kbd "m k") 'buf-move-up)
  (define-key evil-window-map (kbd "m j") 'buf-move-down)
  (define-key evil-window-map (kbd "m h") 'buf-move-left)
  (define-key evil-window-map (kbd "m l") 'buf-move-right)

  (require 'hydra)
  (require 'frame-cmds)
  (defhydra hydra-move-to-window (evil-window-map "g")
    "move to window"
    ("q" nil)

    ("j" evil-window-down)
    ("k" evil-window-up)
    ("h" evil-window-left)
    ("l" evil-window-right))

  (defhydra hydra-move-buffer (evil-window-map "m")
    "move buffer"
    ("q" nil)

    ("j" buf-move-down)
    ("k" buf-move-up)
    ("h" buf-move-left)
    ("l" buf-move-right))

  (defhydra hydra-resize-frame (evil-window-map "f")
    "resize frame"
    ("q" nil)

    ("j" enlarge-frame)
    ("k" shrink-frame)
    ("h" shrink-frame-horizontally)
    ("l" enlarge-frame-horizontally))

  (defhydra hydra-resize-window (evil-window-map "r")
    "resize window"
    ("q" nil)

    ("=" balance-windows)
    ("m" evil-window-set-height)

    ("f" hydra-resize-frame/body "resize frame" :exit t)

    ("j" shrink-window)
    ("k" enlarge-window)
    ("h" shrink-window-horizontally)
    ("l" enlarge-window-horizontally))

  (evil-mode 1)

  (use-package evil-indent-plus
    :config
    (evil-indent-plus-default-bindings))

  (use-package evil-quickscope
    :config
    (global-evil-quickscope-mode 1))

  (use-package evil-textobj-anyblock
    :config
    (define-key evil-inner-text-objects-map "b" 'evil-textobj-anyblock-inner-block)
    (define-key evil-outer-text-objects-map "b" 'evil-textobj-anyblock-a-block))

  (use-package evil-anzu
    :requires evil)

  (use-package evil-commentary
    :diminish evil-commentary-mode
    :config
    (evil-commentary-mode))

  (use-package evil-exchange
    :config
    (evil-exchange-install))

  (use-package evil-leader
    :config
    (add-hook 'evil-mode-hook 'evil-leader-mode)
    (add-hook 'evil-local-mode-hook 'evil-leader-mode)

    (evil-leader/set-leader "<SPC>")

    (evil-leader/set-key
      "v" 'er/expand-region
      "o" (lambda ()
            (interactive)
            (end-of-line)
            (newline)
            (evil-open-above 1)
            (setq this-command 'evil-open-below))
      "l" (lambda ()
            (interactive)
            (evil-ex-nohighlight)
            (force-mode-line-update))
      "m" 'evil-visual-mark-mode))

  (use-package evil-numbers)

  (use-package evil-surround
    :config
    (global-evil-surround-mode 1))

  (use-package evil-visual-mark-mode)

  (use-package evil-visualstar
    :config
    (global-evil-visualstar-mode))

  (use-package evil-args
    :config
    ;; bind evil-args text objects
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

    ;; bind evil-forward/backward-args
    (define-key evil-normal-state-map "L" 'evil-forward-arg)
    (define-key evil-normal-state-map "H" 'evil-backward-arg)

    (define-key evil-motion-state-map "L" 'evil-forward-arg)
    (define-key evil-motion-state-map "H" 'evil-backward-arg)

    ;; bind evil-jump-out-args
    (define-key evil-normal-state-map "K" 'evil-jump-out-args)

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

    (define-key evil-normal-state-map (kbd "< a") 'evil-arg-swap-backward)
    (define-key evil-normal-state-map (kbd "> a") 'evil-arg-swap-forward)))
