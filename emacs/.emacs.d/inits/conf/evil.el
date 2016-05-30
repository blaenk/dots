(require 'use-package)
(require 'general)

(use-package evil
  :demand t
  :defines blaenk/evil-join

  :general
  (:states 'emacs
    "C-w" 'evil-window-map)

  (:states 'insert
    "<S-return>" 'comment-indent-new-line

    "C-y" 'blaenk/evil-insert-mode-paste

    "C-u" 'blaenk/kill-line
    "C-l" 'move-end-of-line)

  (:states 'normal
    ;; unmap these so they could be used as prefix keys
    ;; this is useful for smartparens
    "<" nil
    ">" nil

    ;; still able to shift things in normal mode
    "< <" 'evil-shift-left-line
    "> >" 'evil-shift-right-line

    "g p" 'exchange-point-and-mark

    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line

    "C-k" 'evil-scroll-up
    "C-j" 'evil-scroll-down

    "z =" 'helm-flyspell-correct

    "[ s" 'flyspell-goto-previous-error
    "] s" 'flyspell-goto-next-error

    "[ S" 'check-previous-spelling-error
    "] S" 'check-next-spelling-error)

  (:states '(normal insert)
    "C-;" 'blaenk/flyspell-last)

  (:states '(normal visual)
    "J" 'blaenk/evil-join
    "<kp-subtract>" 'evil-numbers/dec-at-pt
    "<kp-add>" 'evil-numbers/inc-at-pt)

  (:states 'visual
    ">" 'visual-shift-right
    "<" 'visual-shift-left)

  (:keymaps 'evil-window-map
    "m k" 'buf-move-up
    "m j" 'buf-move-down
    "m h" 'buf-move-left
    "m l" 'buf-move-right)

  :init
  (setq evil-want-C-w-in-emacs-state t)
  (setq evil-text-object-change-visual-type nil)

  ;; TODO check if these should all be in this
  (setq evil-search-module 'evil-search)
  (setq-default evil-symbol-word-search t)

  ;; (setq evil-cross-lines t)

  ;; TODO show trailing whitespace in combination with this?
  ;; (setq evil-move-cursor-back nil)

  ;; TODO necessary?
  (setq-default evil-shift-width 2)
  (setq-default evil-shift-round nil)
  (setq evil-want-C-w-delete t)
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

  :config
  (eval-when-compile
    (require 'evil-macros)
    (require 'evil-types))

  ;; if joined lines are comments, remove delimiters
  (evil-define-operator blaenk/evil-join (beg end)
    "Join the selected lines."
    :motion evil-line
    (let* ((count (count-lines beg end))
           ;; we join pairs at a time
           (count (if (> count 1) (1- count) count))
           ;; the mark at the middle of the joined pair of lines
           (fixup-mark (make-marker)))
      (dotimes (var count)
        (if (and (bolp) (eolp))
            (join-line 1)
          (let* ((end (line-beginning-position 3))
                 (fill-column (1+ (- end beg))))
            ;; save the mark at the middle of the pair
            (set-marker fixup-mark (line-end-position))
            ;; join it via fill
            (fill-region-as-paragraph beg end)
            ;; jump back to the middle
            (goto-char fixup-mark)
            ;; context-dependent whitespace fixup
            (fixup-whitespace))))
      ;; remove the mark
      (set-marker fixup-mark nil)))

  (with-eval-after-load 'solarized
    (eval-when-compile
      (require 'solarized))

    (solarized-with-color-variables 'light
      (setq evil-normal-state-cursor `(,blue-l box))
      (setq evil-insert-state-cursor `(,green-l box))
      (setq evil-visual-state-cursor `(,magenta-l box))
      (setq evil-replace-state-cursor `(,red-l (hbar . 4)))
      (setq evil-operator-state-cursor `((hbar . 6)))
      (setq evil-emacs-state-cursor `(,red-l box))))

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
             (concat ".*" start-re "\\s-?")
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

  (defun blaenk/flyspell-last ()
    (interactive)
    (save-excursion
      (check-previous-spelling-error)))

  (defun blaenk/evil-insert-mode-paste ()
    (interactive)
    (evil-paste-before 1)
    (forward-char))

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

  (bind*
    "o" 'blaenk/evil-open-in-between
    "l" 'blaenk/clear-search)

  (evil-mode 1)

  (use-package evil-indent-plus
    :config
    (evil-indent-plus-default-bindings))

  (use-package evil-quickscope
    :demand t
    :config
    (global-evil-quickscope-mode 1))

  (use-package evil-textobj-anyblock
    :defer t
    :general
    (:keymaps 'evil-inner-text-objects-map
      "b" 'evil-textobj-anyblock-inner-block)

    (:keymaps 'evil-outer-text-objects-map
      "b" 'evil-textobj-anyblock-a-block))

  (use-package evil-anzu)

  (use-package evil-commentary
    :diminish evil-commentary-mode
    :config
    (evil-commentary-mode))

  (use-package evil-exchange
    :config
    (evil-exchange-install))

  (use-package evil-numbers
    :defer t)

  (use-package evil-surround
    :config
    (setq-default
     evil-surround-pairs-alist
     (cons '(? . ("" . "")) evil-surround-pairs-alist))
    (global-evil-surround-mode 1))

  (use-package evil-visual-mark-mode
    :defer t
    :config
    (bind* "m" 'evil-visual-mark-mode))

  (use-package evil-visualstar
    :config
    (global-evil-visualstar-mode))

  (use-package evil-args
    :defer t
    :general
    ;; bind evil-args text objects
    (:keymaps 'evil-inner-text-objects-map
      "a" 'evil-inner-arg)

    (:keymaps 'evil-outer-text-objects-map
      "a" 'evil-outer-arg)

    ;; bind evil-forward/backward-args
    (:states '(normal motion)
      "L" 'evil-forward-arg
      "H" 'evil-backward-arg)

    ;; bind evil-jump-out-args
    (:states 'normal
      "K" 'evil-jump-out-args)

    (:states 'normal
      "> a" 'evil-arg-swap-forward
      "< a" 'evil-arg-swap-backward)

    :config
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
      (apply 'evil-exchange (evil-inner-arg)))))

(provide 'conf/evil)
