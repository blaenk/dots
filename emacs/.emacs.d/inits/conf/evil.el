(require 'straight)
(require 'use-package)
(require 'general)
(require 'conf/common)

(use-package evil
  :demand t
  :defines my-evil-join

  :general
  (:keymaps 'motion
   "M-l" 'my-clear-search

   "C-k" 'evil-scroll-up
   "C-j" 'evil-scroll-down)

  (:keymaps 'normal
   ;; Unbind {>,<} to be able to use them as prefixes for e.g. smartparens or
   ;; evil-args.
   "<" nil
   ">" nil

   ;; Twice the key to shift.
   "< <" 'evil-shift-left-line
   "> >" 'evil-shift-right-line

   "g p" 'exchange-point-and-mark

   "M-o" 'my-evil-open-in-between

   "]p" 'my-evil-paste-after-and-indent
   "]P" 'my-evil-paste-before-and-indent

   "[p" 'my-evil-paste-before-and-indent
   "[P" 'my-evil-paste-before-and-indent)

  (:keymaps 'visual
   ">" 'my-visual-shift-right
   "<" 'my-visual-shift-left)

  (:keymaps '(normal visual)
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line

   "gr" 'my-evil-visual-replace
   "gR" 'my-evil-replace

   "[y" 'my-evil-yank-without-indentation
   "]y" 'my-evil-yank-for-markdown)

  (:keymaps 'override :states '(normal visual)
   "gX" 'my-cancel-exchange-or-visual-replace)

  (:keymaps 'insert
   "C-y" 'my-evil-insert-mode-paste
   "C-u" 'my-kill-line
   "M-u" 'universal-argument
   "C-l" 'end-of-line)

  (:keymaps 'my-window-prefix-map
    "c" 'evil-window-delete

    "_" 'evil-window-set-height
    "|" 'evil-window-set-width

    "+" 'evil-window-increase-height
    "-" 'evil-window-decrease-height
    "<" 'evil-window-decrease-width
    ">" 'evil-window-increase-width

    "H" 'evil-window-move-far-left
    "J" 'evil-window-move-very-bottom
    "K" 'evil-window-move-very-top
    "L" 'evil-window-move-far-right

    "k" 'evil-window-up
    "j" 'evil-window-down
    "h" 'evil-window-left
    "l" 'evil-window-right

    "s v" 'evil-window-vsplit
    "s h" 'evil-window-split
    "s s" 'evil-window-split

    "s n" '(:ignore t :which-key "new")
    "s n v" 'evil-window-vnew
    "s n h" 'evil-window-new
    "s n n" 'evil-window-new)

  (my-map
    "i n" 'my-evil-open-in-between
    "k w" 'evil-window-delete)

  ;; Short-circuit the window map: C-w → C-c w
  (:keymaps '(emacs motion)
   "C-w" 'my-window-prefix-command)

  :hook
  (after-init . evil-mode)

  :init
  (setq evil-text-object-change-visual-type nil
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-want-Y-yank-to-eol t
        evil-echo-state nil
        evil-ex-substitute-global t)

  (setq-default evil-symbol-word-search t
                evil-shift-width 2
                evil-shift-round nil)

  (with-eval-after-load 'company
    (defun my--evil-company (arg)
      (call-interactively #'company-complete))

    (setq evil-complete-next-func #'my--evil-company
          evil-complete-previous-func #'my--evil-company))

  :config
  ;; don't auto-copy visual selections
  (fset #'evil-visual-update-x-selection #'ignore)

  (customize-set-variable 'evil-search-module 'evil-search)
  (customize-set-variable 'evil-want-C-w-delete t)

  ;; (add-to-list '("Pattern" . emacs) 'evil-buffer-regexps)

  (eval-when-compile
    (require 'evil-macros)
    (require 'evil-types))

  (my-with-solarized-colors
   (setq evil-normal-state-cursor `(,blue-lc box)
         evil-insert-state-cursor `(,green-lc box)
         evil-visual-state-cursor `(,magenta-lc box)
         evil-replace-state-cursor `(,red-lc (hbar . 4))
         evil-operator-state-cursor `((hbar . 6))
         evil-emacs-state-cursor `(,red-lc box)))

  ;; FIXME
  ;; when done on line:
  ;;     (insert "\n#endif  // " ident)))))
  ;; it modifies the string and becomes:
  ;;     (insert "\n#endif // " ident)))))
  ;;
  ;; if joined lines are comments, remove delimiters
  (evil-define-operator my-evil-join (beg end)
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

  ;; If the point is in a comment that has non-whitespace content, delete up
  ;; until the beginning of the comment. If already at the beginning of the
  ;; comment, delete up to the indentation point. If already at the indentation
  ;; point, delete to the beginning of the line.
  (defun my-kill-line ()
    (interactive)
    (let* ((starts
            (-non-nil
             ;; add comment-start-regexps to this as needed
             `("\\s<"
               ,(when (bound-and-true-p comment-start)
                  (regexp-quote (s-trim-right comment-start)))
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

  (defun my-evil-insert-mode-paste ()
    (interactive)
    (evil-paste-before 1)
    (forward-char))

  (defun my-evil-open-in-between ()
    "Open a new line in between the current line."
    (interactive)

    (evil-with-single-undo
      (evil-open-below 1)
      (evil-maybe-remove-spaces t)
      (evil-open-above 1)))

  (defun my-cancel-exchange-or-visual-replace ()
    "Cancel any pending evil-exchange or visual-replace."
    (interactive)

    (when (bound-and-true-p evil-exchange--position)
      (evil-exchange--clean))

    (when (bound-and-true-p my--evil-visual-replace-position)
      (my--evil-visual-replace-clean)))

  (defun my-clear-search ()
    "Clear the evil search persisted highlight."
    (interactive)

    (evil-ex-nohighlight)
    (force-mode-line-update))

  (evil-define-operator my-visual-shift-left (beg end type)
    "shift text to the left"
    :keep-visual t
    :motion evil-line
    :type line
    (interactive "<r><vc>")
    (call-interactively #'evil-shift-left)
    (evil-normal-state)
    (evil-visual-restore))

  (evil-define-operator my-visual-shift-right (beg end type)
    "shift text to the right"
    :keep-visual t
    :motion evil-line
    :type line
    (interactive "<r><vc>")
    (call-interactively #'evil-shift-right)
    (evil-normal-state)
    (evil-visual-restore))

  (evil-define-command my-evil-paste-before-and-indent
    (count &optional register yank-handler)
    "Pastes the latest yanked text before point
and gives it the same indentation as the surrounding code.
The return value is the yanked text."
    (interactive "P<x>")
    (evil-with-single-undo
      (let ((text (evil-paste-before count register yank-handler)))
        (evil-indent (evil-get-marker ?\[) (evil-get-marker ?\]))
        text)))

  (evil-define-command my-evil-paste-after-and-indent
    (count &optional register yank-handler)
    "Pastes the latest yanked text behind point
and gives it the same indentation as the surrounding code.
The return value is the yanked text."
    (interactive "P<x>")
    (evil-with-single-undo
      (let ((text (evil-paste-after count register yank-handler)))
        (evil-indent (evil-get-marker ?\[) (evil-get-marker ?\]))
        text)))

  (evil-define-operator my-evil-yank-without-indentation (beg end type)
    "Saves the characters in motion into the kill-ring."
    :move-point nil
    :repeat nil
    (interactive "<R>")
    (unless (eq type 'block)
      (my-copy-region-unindented nil beg end)))

  (evil-define-operator my-evil-yank-for-markdown (beg end type)
    "Saves the characters in motion into the kill-ring."
    :move-point nil
    :repeat nil
    (interactive "<R>")
    (unless (eq type 'block)
      (my-copy-region-unindented '(4) beg end)))

  (evil-define-operator my-evil-replace (beg end type)
    "Replace the characters in motion with ex replace."
    :move-point nil
    :repeat nil
    (interactive "<R>")
    (let ((region (buffer-substring-no-properties beg end)))
      (evil-ex (concat "%s/" region "/"))))

  ;; This defines an operator named my-evil-visual-replace which I bind to gR in
  ;; normal and visual modes. First make a selection or motion with gR, this
  ;; selects that region as the text to be replaced. Next select a region within
  ;; which to replace that text and press gR again, which pre-fills an
  ;; ex-command for making the replacement.
  ;;
  ;; I noticed that the evil-exchange command was very much like this (i.e. a
  ;; 2-step operation) and noticed that its code was pleasantly simple, so most
  ;; of this code comes from there.
  (defvar my--evil-visual-replace-position nil
    "Position to replace.")

  (defvar my--evil-visual-replace-overlays nil
    "Overlays for highlighting to-replace area.")

  (defun my--evil-visual-replace-highlight (beg end)
    (let ((o (make-overlay beg end nil t nil)))
      (overlay-put o 'face 'highlight)
      (add-to-list 'my--evil-visual-replace-overlays o)))

  (defun my--evil-visual-replace-clean ()
    (setq my--evil-visual-replace-position nil)
    (mapc 'delete-overlay my--evil-visual-replace-overlays)
    (setq my--evil-visual-replace-overlays nil))

  (evil-define-operator my-evil-visual-replace (beg end type)
    "Replace the first selection within the second."
    :move-point nil
    (interactive "<R>")

    (let ((beg-marker (copy-marker beg t))
          (end-marker (copy-marker end nil)))
      (if (null my--evil-visual-replace-position)
          ;; call without my--evil-visual-replace-position set: store region
          (progn
            (setq my--evil-visual-replace-position (list (current-buffer) beg-marker end-marker type))

            ;; highlight area marked to exchange
            (if (eq type 'block)
                (evil-apply-on-block #'my--evil-visual-replace-highlight beg end nil)
              (my--evil-visual-replace-highlight beg end)))
        ;; secondary call: do exchange
        (cl-destructuring-bind
            (orig-buffer orig-beg orig-end orig-type) my--evil-visual-replace-position
          (let ((region (buffer-substring-no-properties orig-beg orig-end)))
            (evil-ex (concat "'<,'>s/" region "/"))
            (my--evil-visual-replace-clean))
          )))
    ;; place cursor on beginning of line
    (when (and (called-interactively-p 'any) (eq type 'line))
      (evil-first-non-blank)))

  (defun my-evil-visual-replace-cancel ()
    "Cancel current pending visual replace."
    (interactive)

    (if (null my--evil-visual-replace-position)
        (message "No pending visual replace")
      (my--evil-visual-replace-clean)
      (message "Visual Replace canceled")))
  )

(use-package evil-indent-plus
  :after evil
  :config
  (evil-indent-plus-default-bindings))

(use-package evil-quickscope
  :after evil
  :config
  (global-evil-quickscope-mode 1))

(use-package evil-lion
  :after evil
  :config
  (evil-lion-mode))

(use-package evil-textobj-anyblock
  :general
  (:keymaps 'inner
   "b" 'evil-textobj-anyblock-inner-block)

  (:keymaps 'outer
   "b" 'evil-textobj-anyblock-a-block))

(use-package evil-anzu
  :after evil)

(use-package evil-commentary
  :after evil

  :config
  (evil-commentary-mode))

(use-package evil-exchange
  :after evil

  :config
  (evil-exchange-install))

(use-package evil-numbers
  :general
  (:keymaps '(normal visual)
   "<kp-subtract>" 'evil-numbers/dec-at-pt
   "<kp-add>" 'evil-numbers/inc-at-pt))

(use-package evil-surround
  :after evil
  :config
  (setq-default
   evil-surround-pairs-alist
   (cons '(? . ("" . "")) evil-surround-pairs-alist))

  (global-evil-surround-mode 1))

(use-package evil-visualstar
  :after evil
  :config
  (global-evil-visualstar-mode))

(use-package evil-args
  :general
  (:keymaps 'normal
   "K" 'evil-jump-out-args

   "> a" 'evil-arg-swap-forward
   "< a" 'evil-arg-swap-backward)

  (:keymaps 'inner
   "a" 'evil-inner-arg)

  (:keymaps 'outer
   "a" 'evil-outer-arg)

  (:keymaps '(normal motion)
   "L" 'evil-forward-arg
   "H" 'evil-backward-arg)

  :config
  (defun evil-arg-swap-forward ()
    "Swap the current argument with the next one."
    (interactive)

    (apply #'evil-exchange (evil-inner-arg))
    (call-interactively #'evil-forward-arg)
    (apply #'evil-exchange (evil-inner-arg)))

  (defun evil-arg-swap-backward ()
    "Swap the current argument with the previous one."
    (interactive)

    (apply #'evil-exchange (evil-inner-arg))
    (evil-forward-arg 1)
    (evil-backward-arg 2)
    (apply #'evil-exchange (evil-inner-arg))))

(use-package evil-goggles
  :after evil

  :init
  (setq evil-goggles-duration 0.100

        ;; to disable the hint when pasting:
        ;; evil-goggles-enable-paste nil

        ;; list of all on/off variables, their default value is `t`:
        ;;
        ;; evil-goggles-enable-delete
        ;; evil-goggles-enable-indent
        ;; evil-goggles-enable-yank
        ;; evil-goggles-enable-join
        ;; evil-goggles-enable-fill-and-move
        ;; evil-goggles-enable-paste
        ;; evil-goggles-enable-shift
        ;; evil-goggles-enable-surround
        ;; evil-goggles-enable-commentary
        ;; evil-goggles-enable-replace-with-register
        ;; evil-goggles-enable-set-marker
        )

  :config
  (evil-goggles-mode))

(use-package evil-string-inflection
  :after evil)

(provide 'conf/evil)
