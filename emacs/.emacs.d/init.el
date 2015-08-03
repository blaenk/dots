(require 'package)

;; TODO
;;
;; * check out eldoc mode

;; backups

(setq backup-by-copying t)

(require 'saveplace)
(setq-default save-place t)

(let* ((backup-dir (expand-file-name "backups/" user-emacs-directory))
       (undo-history-dir (expand-file-name "undos/" user-emacs-directory))
       (auto-save-dir (expand-file-name "autosaves/" user-emacs-directory))
       (auto-save-list-prefix (expand-file-name "saves-" auto-save-dir))
       (place-dir (expand-file-name "saved-places" user-emacs-directory)))
  (setq backup-directory-alist `((".*" . ,backup-dir)))
  (setq version-control t)
  (setq delete-old-versions t)
  (setq undo-tree-history-directory-alist `((".*" . ,undo-history-dir)))
  (setq auto-save-list-file-prefix auto-save-list-prefix)
  (setq auto-save-file-name-transforms `((".*" ,auto-save-dir t)))
  (setq save-place-file place-dir))

;; TODO
;; * continue comment on newline

(setq debug-on-error t)
(setq whitespace-style '(face trailing lines-tail empty))

(when window-system (set-frame-size (selected-frame) 96 41))

(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)

(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

;; NOTE should only apply when in vm
;; TODO vm-local settings/env-vars
(setq browse-url-browser-function 'kill-new)
(setq whitespace-line-column nil)
(setq-default fill-column 80)
(setq-default cursor-type 'box)
(setq inhibit-startup-message t)
(setq show-paren-delay 0)
(setq ring-bell-function 'ignore)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed t)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)
(setq gdb-many-windows t)
(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default echo-keystrokes 0.5)
;; 64 mb before gc kicks in
(setq gc-cons-threshold 64000000)
(fset 'yes-or-no-p 'y-or-n-p)
(setq split-height-threshold 0)
(setq split-width-threshold 0)
(setq uniquify-buffer-name-style 'forward)
(add-to-list 'auto-coding-alist '("\\.nfo\\'" . ibm437))
(setq frame-title-format
      '((:eval (replace-regexp-in-string "^ +" "" (buffer-name)))))
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq eldoc-idle-delay 0.1)
(setq x-underline-at-descent-line t)
(setq bug-reference-bug-regexp
 "\\([Ii]ssue ?#\\|[Bb]ug ?#\\|[Pp]atch ?#\\|RFE ?#\\|PR [a-z-+]+/\\)\\([0-9]+\\(?:#[0-9]+\\)?\\)")

(setq default-frame-alist '((font . "DejaVu Sans Mono-10.5")))

(defun my-fix-emojis (&optional frame)
  (set-fontset-font "fontset-default" nil "Symbola" frame 'append))

(my-fix-emojis)

(add-hook 'after-make-frame-functions 'my-fix-emojis)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)

(savehist-mode)
(recentf-mode)
(visual-line-mode)
(column-number-mode)
(flyspell-prog-mode)
(winner-mode)
(goto-address-mode)
(electric-pair-mode)
(show-paren-mode)
(desktop-save-mode 1)

;; NOTE
;; on empty lines, line number gone
;; https://github.com/alpaker/Fill-Column-Indicator/issues/4
(add-hook 'prog-mode-hook 'whitespace-mode)

(defvaralias 'c-basic-offset 'tab-width)

;; ediff

(setq ediff-split-window-function 'split-window-horizontally)
;; NOTE can toggle this in real-time with ediff-toggle-multiframe
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(defun is-fullscreen ()
  (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth)))

(defun my-go-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen 'fullboth))

(defun my-un-fullscreen ()
  (set-frame-parameter nil 'fullscreen nil))

(defun my-toggle-ediff-wide-display ()
  "Turn off wide-display mode (if was enabled) before quitting ediff."
  (when ediff-wide-display-p
    (ediff-toggle-wide-display)))

(add-hook 'ediff-prepare-buffer-hook 'turn-off-hideshow)
(add-hook 'ediff-prepare-buffer-hook 'turn-off-fci-mode)
(add-hook 'ediff-prepare-buffer-hook (lambda () (visual-line-mode -1)))
(add-hook 'ediff-prepare-buffer-hook (lambda () (whitespace-mode -1)))

(defun my-ediff-start ()
  (interactive)
  (my-go-fullscreen))

(defun my-ediff-quit ()
  (interactive)
  (my-toggle-ediff-wide-display)
  (my-un-fullscreen))

(add-hook 'ediff-startup-hook 'my-ediff-start)
(add-hook 'ediff-suspend-hook 'my-ediff-quit 'append)
(add-hook 'ediff-quit-hook 'my-ediff-quit 'append)

(global-unset-key (kbd "C-x C-c"))

(global-set-key (kbd "C-x 2")
                (lambda ()
                  (interactive)
                  (select-window (split-window-below))))

(defun my-kill-line ()
  (interactive)
  (if (looking-back "^[[:space:]]+")
      (kill-line 0)
    (progn
      (let ((beg (point)))
        (back-to-indentation)
        (kill-region beg (point))))))

(define-key global-map (kbd "M-u") 'universal-argument)
(define-key universal-argument-map (kbd "M-u") 'universal-argument-more)

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

;; commands

(defun get-faces (pos)
  "Get the font faces at POS."
  (remq nil
        (list
         (get-char-property pos 'read-face-name)
         (get-char-property pos 'face)
         (plist-get (text-properties-at pos) 'face))))

(defun what-face (pos)
  (interactive "d")
  (let ((face (get-faces pos)))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; packages

(add-to-list 'package-archives
             '("elpa" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(package-install 'use-package)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(with-eval-after-load 'whitespace
  (diminish 'whitespace-mode))

;; http://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

(defun my-is-evil-on ()
  (if (evil-emacs-state-p)
      nil
    (or
     (bound-and-true-p evil-mode)
     (bound-and-true-p evil-local-mode))))

;; FIXME
;; cloud icon and others take up more than one column width
;; NOTE may need substring-no-properties
(defun simple-mode-line-render (left right)
  (let* ((available-width (- (window-total-width) 1 (string-width left)))
         (pad-width (- available-width (string-width right)))
         (fmt (format "%%s %%%ds" available-width)))
    (format fmt left right)))

(defun my-remote-mode-line ()
  (when (and (stringp default-directory)
             (file-remote-p default-directory))
    (concat " " (fontawesome "cloud") " ")))

(defun my-evil-indicator ()
  (let* ((is-evil (my-is-evil-on))
        (indicator (if is-evil "V" "E")))
    (propertize (concat " " indicator " ")
                'face
                (solarized-with-color-variables 'light
                 (if is-evil
                      `(:background ,blue-l :foreground "white" :weight bold)
                    `(:background ,red-l :foreground "white" :weight bold))))))

(defun my-branch ()
  (let* ((no-props (substring-no-properties vc-mode))
         (branch (replace-regexp-in-string
                  (format "^ %s[:-]" (vc-backend buffer-file-name)) "" no-props))
         (branch (string-trim branch)))
    (format " %s " branch)))

;; TODO
;; remote notification
(setq mode-line-left
      `(
        (:propertize "%3c " face mode-line-column)
        (:eval (my-evil-indicator))
        ;; (:propertize
        ;;  (:eval (format " %s " mode-name))
        ;;  face mode-line-mode-name-face)
        (:propertize " %b" face mode-line-buffer-id)
        ))

;; TODO
;; flycheck integration
(setq mode-line-right
      `(
        (:propertize
         (:eval (when (and (not buffer-read-only) (buffer-modified-p (window-buffer nil))) " + "))
         face mode-line-modified-face)
        (:propertize
         (:eval (when buffer-read-only (concat " " (fontawesome "lock") " ")))
         face mode-line-read-only)
        ;; (:eval (my-remote-mode-line))
        (:propertize (:eval (my-branch)) face mode-line-branch)
        ))

;; FIXME
;; anzu prepends to mode-line, need a way to take that into account
;; in calculation, since otherwise it bumps the branch out of the view
(setq-default
 mode-line-format
 `((:eval (simple-mode-line-render
           (format-mode-line mode-line-left)
           (format-mode-line mode-line-right)))))

;; TODO
;; use (member "Symbola" (font-family-list))
;; to fall back on unicode icons
(use-package fontawesome
  :ensure t)

(use-package paradox
  :ensure t)

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light t)

  (make-face 'mode-line-column)
  (make-face 'mode-line-branch)
  (make-face 'mode-line-mode-name-face)
  (make-face 'mode-line-read-only)
  (make-face 'mode-line-modified-face)
  (make-face 'mode-line-remote-face)

  (solarized-with-color-variables 'light
    (custom-theme-set-faces
     'solarized-light
     `(whitespace-trailing ((,class (:background ,red-l))))
     `(whitespace-tab ((,class (:background ,red-l))))
     `(whitespace-line ((,class (:underline t))))

     `(region ((,class (:background ,base02))))

     `(rainbow-delimiters-depth-1-face ((,class (:foreground ,base01))))
     `(rainbow-delimiters-depth-2-face ((,class (:foreground ,cyan))))
     `(rainbow-delimiters-depth-3-face ((,class (:foreground ,yellow))))
     `(rainbow-delimiters-depth-4-face ((,class (:foreground ,blue))))
     `(rainbow-delimiters-depth-5-face ((,class (:foreground ,violet))))
     `(rainbow-delimiters-depth-6-face ((,class (:foreground ,green))))
     `(rainbow-delimiters-depth-7-face ((,class (:foreground ,yellow))))
     `(rainbow-delimiters-depth-8-face ((,class (:foreground ,blue))))
     `(rainbow-delimiters-depth-9-face ((,class (:foreground ,violet))))
     `(rainbow-delimiters-depth-10-face ((,class (:foreground ,green))))
     `(rainbow-delimiters-depth-11-face ((,class (:foreground ,yellow))))
     `(rainbow-delimiters-depth-12-face ((,class (:foreground ,blue))))
     `(rainbow-delimiters-depth-13-face ((,class (:foreground ,violet))))
     `(rainbow-delimiters-unmatched-face
       ((,class (:foreground ,base0 :background ,base03 :inverse-video t))))

     `(mode-line
       ((,class (:inverse-video unspecified
                 :overline ,s-mode-line-underline
                 :underline ,s-mode-line-underline
                 :foreground ,s-mode-line-fg
                 :background ,s-mode-line-bg
                 ))))

     `(mode-line-column
       ((,class (:background ,base03))))

     `(mode-line-branch
       ((,class (:background ,base0
                 :foreground "white"
                 :weight bold))))

     `(mode-line-mode-name-face
       ((,class (:background ,cyan-l
                 :foreground "white"
                 :weight bold))))

     `(mode-line-read-only
       ((,class (:background ,red-l
                 :foreground "white"))))

     `(mode-line-modified-face
       ((,class (:background ,green-l
                 :foreground "white"
                 :weight bold
                 ))))

     `(mode-line-remote-face
       ((,class (:background ,green-l
                 :foreground "white"
                 :weight bold
                 ))))

     `(mode-line-inactive
       ((,class (:inverse-video unspecified
                 :overline ,s-mode-line-underline
                 :underline ,s-mode-line-underline
                 :foreground ,s-mode-line-inactive-fg
                 :background ,s-mode-line-inactive-bg
                 ))))

     `(sp-show-pair-match-face ((,class (:foreground unspecified
                                         :background ,base02
                                         :weight normal))))

     `(sp-show-pair-mismatch-face ((,class (:foreground unspecified
                                            :background ,red
                                            :weight normal))))

     `(show-paren-match ((,class (:foreground unspecified
                                  :background ,base02
                                  :weight normal))))
     )
  ))

(use-package auto-package-update
  :ensure t)

(use-package lua-mode
  :ensure t
  :mode "\\.lua$"
  :interpreter "lua")

(use-package ag
  :ensure t
  :config
  (setq ag-highlight-search t))

(use-package anzu
  :ensure t
  :diminish anzu-mode
  :config
  (global-anzu-mode +1))

(use-package browse-at-remote
  :ensure t
  :bind
  ("C-x g o" . browse-at-remote/to-clipboard))

(use-package cider
  :ensure t

  :init
  (setq cider-auto-mode nil)

  :config
  (add-hook 'cider-mode-hook #'eldoc-mode))

(use-package company
  :ensure t
  :config
  ;; TODO audit
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers t
        company-require-match 'never
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t))

(use-package clojure-mode
  :ensure t)

(use-package diminish
  :ensure t)

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (setq which-key-use-C-h-for-paging nil)
  :config
  (which-key-mode))

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode 1))

(use-package evil-anzu
  :ensure t
  :requires evil)

(use-package evil-commentary
  :ensure t
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode))

(use-package evil-exchange
  :ensure t
  :config
  (evil-exchange-install))

(use-package evil-leader
  :ensure t

  :config
  (add-hook 'evil-mode-hook 'evil-leader-mode)
  (add-hook 'evil-local-mode-hook 'evil-leader-mode)
  (evil-leader/set-leader "<SPC>")

  (evil-leader/set-key
    "o" (lambda ()
          (interactive)
          (evil-open-above 1)
          (newline-and-indent))
    "l" 'evil-ex-nohighlight
    "m" 'evil-visual-mark-mode))

(use-package evil-numbers
  :ensure t)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-visual-mark-mode
  :ensure t)

(use-package evil-visualstar
  :ensure t
  :config
  (global-evil-visualstar-mode))

(use-package evil
  :ensure t

  :init
  ;; TODO check if these should all be in this
  ;; (setq evil-search-module 'evil-search)
  ;; (setq evil-cross-lines t)
  ;; TODO show trailing whitespace in combination with this?
  ;; (setq evil-move-cursor-back nil)
  (setq-default evil-symbol-word-search t)

  (defun my-real-function (fun)
    "Figure out the actual symbol behind a function.
Returns a different symbol if FUN is an alias, otherwise FUN."
    (let ((symbol-function (symbol-function fun)))
      (if (symbolp symbol-function)
          symbol-function
        fun)))

  (defun my-derived-mode-p (mode modes)
    (let ((parent (my-real-function mode)))
      (while (and parent (not (memq parent modes)))
        (setq parent (my-real-function (get parent 'derived-mode-parent))))
      parent))

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
                      (my-derived-mode-p mode modes))
              (throw 'done state)))))))

  (setq evil-want-C-w-in-emacs-state t)
  (setq evil-want-C-w-delete t)
  (setq evil-want-C-u-scroll t)
  (setq evil-default-state 'emacs)
  (setq evil-normal-state-modes '(text-mode prog-mode fundamental-mode
                                  css-mode conf-mode TeX-mode LaTeX-mode
                                  yaml-mode))
  (setq evil-emacs-state-modes '(help-mode))

  (add-hook 'with-editor-mode-hook 'evil-insert-state)

  :config
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

  (define-key evil-insert-state-map (kbd "RET") 'comment-indent-new-line)

  ;; FIXME
  ;; a problem is that this leaves whitespace residue
  ;; e.g. o then escape then o?
  ;; (defun my-open-line ()
  ;;   (interactive)
  ;;   (end-of-visual-line)
  ;;   (if (elt (syntax-ppss) 4)
  ;;       (progn
  ;;         (comment-indent-new-line)
  ;;         (evil-insert-state 1)

  ;;         (when evil-auto-indent
  ;;           (indent-according-to-mode))

  ;;         (add-hook 'post-command-hook #'evil-maybe-remove-spaces)
  ;;         (setq this-command 'evil-open-below))
  ;;     (evil-open-below 1)))

  ;; (define-key evil-normal-state-map (kbd "o") 'my-open-line)
  ;; (define-key evil-normal-state-map (kbd "O")
  ;;   (lambda ()
  ;;     (interactive)
  ;;     (if (eq (line-number-at-pos (point)) 1)
  ;;         (evil-open-above 1)
  ;;         (progn
  ;;           (previous-line)
  ;;           (my-open-line)))))

  (define-key evil-insert-state-map (kbd "C-u") 'my-kill-line)
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

  (evil-mode 1))

(use-package flycheck
  :ensure t)

(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))

(use-package ggtags
  :ensure t

  :config
  (add-hook 'prog-mode-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1)))))

(use-package gist
  :ensure t
  :bind
  (("C-x p s" . gist-region-or-buffer-private)
   ("C-x p p" . gist-region-or-buffer)))

(use-package json-mode
  :ensure t)

(use-package gitconfig-mode
  :ensure t)

(use-package gitignore-mode
  :ensure t)

(use-package gitattributes-mode
  :ensure t)

(use-package git-gutter
  :ensure t
  :bind
  ;; NOTE mnemonic is 'git ruler'
  (("C-x g r t" . git-gutter:toggle)
   ("C-x g r n" . git-gutter:next-hunk)
   ("C-x g r p" . git-gutter:previous-hunk)))

(use-package markdown-mode
  :ensure t
  :mode ("\\.markdown\\'" "\\.md\\'"))

(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode))

(use-package helm
  :ensure t
  :diminish helm-mode

  :bind
  (("M-x" . helm-M-x)
   ("M-i" . helm-imenu)
   ("C-c h" . helm-command-prefix)
   ("C-x b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files)
   ("C-h a" . helm-apropos)
   ("C-h i" . helm-info-emacs)
   ("C-x r" . helm-recentf))

  :init
  (setq helm-quick-update t)
  (setq helm-split-window-in-side-p t)
  (setq helm-display-header-line nil)
  (setq helm-autoresize-max-height 30)
  (setq helm-autoresize-min-height 30)

  :config
  (require 'helm-config)
  (helm-autoresize-mode t)

  (global-unset-key (kbd "C-x c"))

  (helm-mode 1))

(use-package helm-ag
  :ensure t)

(use-package helm-gtags
  :ensure t
  :diminish helm-gtags-mode

  :init
  (setq helm-gtags-prefix-key "C-t")
  (setq helm-gtags-suggested-key-mapping t)

  :config
  (helm-gtags-mode))

(use-package helm-descbinds
  :ensure t
  :config
  (helm-descbinds-mode))

(use-package helm-projectile
  :ensure t
  :diminish projectile-mode

  :config
  (add-to-list 'helm-projectile-sources-list
               'helm-source-projectile-recentf-list)

  (helm-projectile-on)

  (defmacro if-projectile (is-projectile is-not)
    `(lambda ()
       (interactive)
       (if (projectile-project-p)
           (,is-projectile)
         (,is-not))))

  (define-key projectile-mode-map (kbd "M-p") 'helm-projectile)

  (with-eval-after-load 'evil-leader
    (evil-leader/set-key
      "f" (if-projectile helm-projectile helm-find-files)
      "b" (if-projectile helm-projectile-switch-to-buffer helm-buffers-list))))

(use-package visual-regexp
  :ensure t)

(use-package multiple-cursors
  :ensure t)

(use-package sx
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package erlang
  :ensure t)

(use-package scala-mode2
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package less-css-mode
  :ensure t)

(use-package robe
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'robe-mode) )

(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'")

(use-package elixir-mode
  :ensure t)

(use-package alchemist
  :ensure t)

(use-package racket-mode
  :ensure t)

(use-package helm-flyspell
  :ensure t)

(use-package swiper
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t)

  :bind
  (("C-s" . swiper)
   ("C-r" . swiper)
   ("C-c C-r" . ivy-resume)
   ([f6] . ivy-resume)))

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)

  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :interpreter "node")

(use-package cmake-mode
  :ensure t)

(use-package cmake-font-lock
  :ensure t
  :config
  (autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
  (add-hook 'cmake-mode-hook 'cmake-font-lock-activate))

(use-package shackle
  :ensure t
  :init
  (setq shackle-rules '((compilation-mode :noselect t))
        shackle-default-rule '(:select t)))

(use-package magit
  :ensure t

  :bind
  (("C-x g s" . magit-status)
   ("C-x g p" . magit-dispatch-popup))

  :config
  (defadvice magit-status (after magit-fullscreen activate)
    (delete-other-windows))

  (with-eval-after-load 'magit-ediff
    (add-hook 'magit-ediff-quit-hook 'my-ediff-quit))

  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
  (add-hook 'git-commit-setup-hook 'fci-mode))

(use-package magit-filenotify
  :ensure t
  :config
  (add-hook 'magit-status-mode-hook 'magit-filenotify-mode))

(use-package magit-gh-pulls
  :ensure t
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

(use-package magit-gitflow
  :ensure t)

(use-package multiple-cursors
  :ensure t)

(use-package projectile
  :ensure t

  :init
  (setq projectile-completion-system 'helm)

  :config
  (projectile-global-mode))

(with-eval-after-load 'rust-mode
  (use-package racer
    :ensure t
    :no-require t

    :init
    (setq racer-rust-src-path "~/code/rust/rust/src")
    (setq racer-cmd "~/code/rust/racer/target/release/racer")
    (add-to-list 'load-path "~/code/rust/racer/editors/emacs")

    :config
    (add-hook 'rust-mode-hook
              '(lambda ()
                 (racer-activate)
                 ;; (local-set-key (kbd "M-.") #'racer-find-definition)
                 (local-set-key (kbd "TAB") #'racer-complete-or-indent)))))

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :config
  ;; disable highlighting color names
  (setq rainbow-x-colors nil)
  (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package relative-line-numbers
  :ensure t
  :init
  (defun abs-rel-numbers (offset)
    (if (= offset 0)
        (format "%3d " (line-number-at-pos))
      (format "%3d " (abs offset))))

  (setq relative-line-numbers-format #'abs-rel-numbers)

  :config
  (with-eval-after-load 'evil-leader
    (evil-leader/set-key "n" 'relative-line-numbers-mode))

  (setq relative-line-numbers-motion-function 'forward-visible-line)
  (add-hook 'prog-mode-hook 'relative-line-numbers-mode))

(use-package rust-mode
  :ensure t)

(use-package on-parens
  :ensure t)

(use-package smartparens
  :ensure t
  :diminish smartparens-mode

  :init
  (setq sp-show-pair-from-inside t)
  (setq sp-show-pair-delay 0)
  (setq sp-highlight-pair-overlay nil)
  (setq sp-autoescape-string-quote nil)
  (setq sp-cancel-autoskip-on-backward-movement nil)
  (setq sp-autoescape-string-quote nil)

  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)

  (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
  (add-hook 'clojure-mode-hook 'smartparens-mode)
  (add-hook 'racket-mode-hook 'smartparens-mode)
  (add-hook 'scheme-mode-hook 'smartparens-mode)

  (define-key smartparens-mode-map (kbd "M-S") 'sp-split-sexp)
  (define-key smartparens-mode-map (kbd "M-J") 'sp-join-sexp)

  ;; TODO bind % to jump toggle matching pair

  (with-eval-after-load 'on-parens
    (with-eval-after-load 'evil
      ;; https://github.com/tpope/vim-sexp-mappings-for-regular-people

      (defun my-evil-goto-char (pos)
        (when (evil-normal-state-p) (decf pos))
        (goto-char pos))

      ;; TODO evil-commentary delegating sp-comment wrapper?

      (define-key evil-normal-state-map (kbd "> )")
        (lambda ()
          (interactive)
          (on-parens-forward-slurp)
          ;; get back on paren
          (sp-get (sp-get-enclosing-sexp) (my-evil-goto-char :end))))

      (define-key evil-normal-state-map (kbd "< )")
        (lambda ()
          (interactive)
          (on-parens-forward-barf)
          ;; get back on paren
          (sp-restrict-to-object 'sp-prefix-pair-object 'sp-backward-down-sexp)))

      (define-key evil-normal-state-map (kbd "> (")
        (lambda ()
          (interactive)
          (on-parens-backward-barf)
          ;; get back on paren
          (sp-restrict-to-object 'sp-prefix-pair-object 'sp-next-sexp)))

      (define-key evil-normal-state-map (kbd "< (")
        (lambda ()
          (interactive)
          (on-parens-backward-slurp)
          ;; get back on paren
          (sp-get (sp-get-enclosing-sexp) (my-evil-goto-char (+ :beg 1)))))

      ;; NOTE can use evil-define-motion to create motions out of these
      (define-key evil-normal-state-map (kbd "W") 'on-parens-forward-sexp)
      (define-key evil-normal-state-map (kbd "E") 'on-parens-forward-sexp-end)
      (define-key evil-normal-state-map (kbd "g E") 'on-parens-backward-sexp-end)
      (define-key evil-normal-state-map (kbd "B") 'on-parens-backward-sexp)

      (define-key evil-normal-state-map (kbd "< u") 'sp-splice-sexp-killing-backward)
      (define-key evil-normal-state-map (kbd "> u") 'sp-splice-sexp-killing-forward)

      (define-key evil-normal-state-map (kbd "< d")
        (lambda ()
          (interactive)
          (sp-kill-sexp '(-4))))

      (define-key evil-normal-state-map (kbd "> d")
        (lambda ()
          (interactive)
          (sp-kill-sexp '(4))))

      (defun sp-get-current-non-string-sexp (pos)
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
                    (sp-get-current-non-string-sexp (goto-char end))
                  enclosing-sexp))))))

      (defun sp-end-of-current-sexp (pos)
        "jump to the end of the current, non-string sexp"
        (interactive "d")

        (let ((end (sp-get (sp-get-current-non-string-sexp pos) :end)))
          (when end
            (my-evil-goto-char end))))

      (defmacro my-save-position (&rest body)
        "restore column and form-relative line number"
        `(let* ((column (current-column))
                (pos (point))
                (begin-line (line-number-at-pos pos)))
           (sp-end-of-current-sexp pos)
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

      (define-key evil-normal-state-map (kbd "< f")
        (sp-restrict-to-object-interactive 'sp-prefix-pair-object 'move-form-backward))

      (define-key evil-normal-state-map (kbd "> f")
        (sp-restrict-to-object-interactive 'sp-prefix-pair-object 'move-form-forward))

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

      (define-key evil-normal-state-map (kbd "< s") 'move-symbol-backward)
      (define-key evil-normal-state-map (kbd "> s") 'move-symbol-forward)

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

      (define-key evil-normal-state-map (kbd "< i") 'insert-before-form)
      (define-key evil-normal-state-map (kbd "> i") 'insert-after-form))))

(use-package toml-mode
  :ensure t)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (setq undo-tree-auto-save-history t)

  :config
  (global-undo-tree-mode)

  (defadvice undo-tree-make-history-save-file-name
      (after undo-tree activate)
    (setq ad-return-value (concat ad-return-value ".gz"))))

(use-package web-mode
  :ensure t)

(use-package wgrep
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all))

(use-package fill-column-indicator
  :ensure t
  :config
  (with-eval-after-load 'magit
    (add-hook 'git-commit-setup-hook 'fci-mode)))

(use-package bug-reference-github
  :ensure t

  :config
  (add-hook 'find-file-hook 'bug-reference-github-set-url-format))

(use-package buffer-move
  :ensure t
  :config
  (with-eval-after-load 'evil
    (define-key evil-window-map (kbd "m k") 'buf-move-up)
    (define-key evil-window-map (kbd "m j") 'buf-move-down)
    (define-key evil-window-map (kbd "m h") 'buf-move-left)
    (define-key evil-window-map (kbd "m l") 'buf-move-right)))

(use-package ace-window
  :ensure t
  :bind
  ("C-x o" . ace-window)
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package tex-site
  :ensure auctex
  :init
  (setq TeX-PDF-mode t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)

  (setq TeX-view-program-selection
        '((output-dvi "DVI Viewer")
          (output-pdf "PDF Viewer")
          (output-html "HTML Viewer")))

  (setq TeX-view-program-list
        '(("DVI Viewer" "open %o")
          ("PDF Viewer" "open %o")
          ("HTML Viewer" "open %o")))
  :config
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode))
