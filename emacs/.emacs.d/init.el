(require 'package)

(setq load-prefer-newer t)

(setq backup-by-copying t)

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

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(setq use-package-always-ensure t)

;; TODO
;; use this or systemctl --user import-environment?
;; this is probably more predictable
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-copy-envs
   '("VM" "PATH" "GTAGSCONF" "GTAGSLABEL" "SSH_AUTH_SOCK")))

(defun blaenk/edit-init ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(global-set-key (kbd "C-c e") 'blaenk/edit-init)

(when window-system (set-frame-size (selected-frame) 96 41))

(when (getenv "VM")
  (setq browse-url-browser-function 'kill-new))

(setq inhibit-x-resources t)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(setq x-underline-at-descent-line t)
(setq save-interprogram-paste-before-kill t)

(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

(setq apropos-do-all t)

(setq mouse-yank-at-point t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed t)
(setq mouse-wheel-follow-mouse 't)

(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)

;; NOTE gdb also requires argument `-i=mi`
(setq gdb-many-windows t)
(setq gdb-show-main t)

(setq show-paren-delay 0)

(setq-default fill-column 80)

(setq visible-bell t)
(setq ring-bell-function 'ignore)

(setq delete-by-moving-to-trash t)

(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)

(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)
(setq tab-width 2)

(fset 'yes-or-no-p 'y-or-n-p)

(setq explicit-shell-file-name "/bin/zsh")

(setq sentence-end-double-space nil)
(setq-default cursor-type 'box)
(setq-default echo-keystrokes 0.1)
(setq gc-cons-threshold 64000000)
(setq eldoc-idle-delay 0.1)
(setq uniquify-buffer-name-style 'forward)
(setq frame-title-format '(:eval (blaenk/file-name t)))
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq hl-line-sticky-flag t)

(add-to-list 'auto-coding-alist '("\\.nfo\\'" . ibm437))
(setq default-frame-alist '((font . "DejaVu Sans Mono-10.5")))

(global-set-key [remap eval-expression] 'pp-eval-expression)

(define-key global-map (kbd "M-u") 'universal-argument)

(defun blaenk/kill-this-buffer ()
  (interactive)
  (let ((buffer-modified-p nil))
    (kill-buffer (current-buffer))))

(define-key global-map (kbd "C-c s") 'save-buffer)

(defun blaenk/q-switch-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(define-key global-map (kbd "C-c o") 'blaenk/q-switch-buffer)
(define-key global-map (kbd "C-c k") 'blaenk/kill-this-buffer)
(define-key global-map (kbd "C-c b") 'bury-buffer)
(define-key universal-argument-map (kbd "M-u") 'universal-argument-more)

(cond
 ((eq system-type 'darwin)
  (set-fontset-font "fontset-default" nil "Symbola" nil 'append))
 ((eq system-type 'gnu/linux)
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family "Apple Symbols") nil 'append)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)

(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(savehist-mode)
(recentf-mode)
(visual-line-mode)
(column-number-mode)
(flyspell-prog-mode)
(winner-mode)
(electric-pair-mode)
(show-paren-mode)

(defun blaenk/pop-to-frame ()
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

(global-set-key (kbd "C-c f") 'blaenk/pop-to-frame)
(defun blaenk/flyspell-goto-previous-error (arg)
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
            (setq arg 0))
        (forward-word)))))

(defun blaenk/force-save ()
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(global-set-key (kbd "C-S-x C-S-s") 'blaenk/force-save)

(defun blaenk/get-faces (pos)
  "Get the font faces at POS."
  (remq nil
        (list
         (get-char-property pos 'read-face-name)
         (get-char-property pos 'face)
         (plist-get (text-properties-at pos) 'face))))

(defun what-face (pos)
  (interactive "d")
  (let ((face (blaenk/get-faces pos)))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun toggle-header-line ()
  (interactive)
  (if header-line-format
      (progn
        (setq header-line-format-save header-line-format)
        (setq header-line-format nil))
    (setq header-line-format header-line-format-save)))

(defun blaenk/setup-mode-line ()
  (defun blaenk/is-evil-on ()
    (if (evil-emacs-state-p)
        nil
      (or
       (bound-and-true-p evil-mode)
       (bound-and-true-p evil-local-mode))))

  (defun blaenk/render-mode-line (left right)
    (let* ((available-width (- (window-total-width) (string-width left)))
           (pad-width (- available-width (string-width right)))
           (specified-space (propertize " " 'display `((space :width ,pad-width))))
           (fmt (concat "%s" specified-space "%s")))
      (format fmt left right)))

  (defun blaenk/is-remote-buffer ()
    (and (stringp default-directory)
         (file-remote-p default-directory)))

  (defun blaenk/remote-mode-line ()
    (when (blaenk/is-remote-buffer)
      (concat " " (fontawesome "cloud") " ")))

  (defun blaenk/evil-indicator ()
    (let* ((is-evil (blaenk/is-evil-on))
           (indicator (if is-evil "V" "E")))
      (propertize
       (concat " " indicator " ")
       'face
       (solarized-with-color-variables
         'light
         (if is-evil
             `(:background ,blue-l :foreground "white" :weight bold)
           `(:background ,red-l :foreground "white" :weight bold))))))

  (defun blaenk/vc-branch ()
    (let ((backend (vc-backend (buffer-file-name))))
      (when backend
        (format " %s " (substring
                        vc-mode
                        (+ (length (symbol-name backend)) 2))))))

  (defun blaenk/is-modified ()
    (and
     (not buffer-read-only)
     (buffer-file-name)
     (buffer-modified-p (window-buffer nil))))

  (setq-default
   header-line-format-save
   `(
     (:propertize
      (:eval (format " %s " (format-mode-line mode-name)))
      face mode-line-mode-name-face)
     ))

  (defun blaenk/file-name (for-title)
    (let* ((name (buffer-file-name)))
      (if name
          (let* ((project-root (if (projectile-project-p)
                                   (projectile-project-root)
                                 nil))
                 (name (if project-root
                           (replace-regexp-in-string
                            (regexp-quote project-root) ""
                            name)
                         (abbreviate-file-name name)))
                 (directory (or (file-name-directory name) ""))
                 (file-name (file-name-nondirectory name)))
            (format "%s%s %s%s "
                    (if project-root
                        (propertize
                         (format " %s " (projectile-project-name))
                         'face 'mode-line-branch-face)
                      "")
                    (if for-title "→" "")
                    (propertize directory 'face 'mode-line-stem-face)
                    (propertize file-name 'face 'mode-line-buffer-id)))
        (propertize " %b " 'face 'mode-line-buffer-id))))

  (setq mode-line-left
        `(
          (:propertize "%3c " face mode-line-column-face)
          (anzu-mode
           (:propertize
            (:eval (anzu--update-mode-line))
            face
            mode-line-anzu-face))
          (:eval (blaenk/evil-indicator))
          (:propertize
           (:eval (blaenk/remote-mode-line))
           face mode-line-remote-face)
          (:eval (blaenk/file-name nil))
          ))

  ;; TODO
  ;; flycheck integration
  (setq mode-line-right
        `(
          (:propertize
           (:eval
            (when (blaenk/is-modified) " + "))
           face mode-line-modified-face)
          (:propertize
           (:eval (when buffer-read-only (concat " " (fontawesome "lock") " ")))
           face mode-line-read-only-face)
          (:propertize (:eval (blaenk/vc-branch))
                       face mode-line-branch-face)
          ))

  (setq-default
   mode-line-format
   `(:eval (blaenk/render-mode-line
            (format-mode-line mode-line-left)
            (format-mode-line mode-line-right)))))

(blaenk/setup-mode-line)

(use-package whitespace
  :ensure nil
  :defer t
  :diminish whitespace-mode

  :init
  ;; NOTE
  ;; lines-tail to see which lines go beyond max col
  (setq whitespace-style
        '(face indentation trailing empty space-after-tab
          space-before-tab tab-mark))
  (setq whitespace-line-column nil)
  (add-hook 'prog-mode-hook 'whitespace-mode))

(use-package sh-script
  :ensure nil
  :mode ("\\.zsh\\(rc\\)?\\'" . sh-mode)
  :config
  (add-hook 'sh-mode-hook
            (lambda ()
              (if (string-match "\\.zsh\\(rc\\)?$" (or (buffer-file-name) ""))
                  (sh-set-shell "zsh")))))

(use-package tramp
  :ensure nil
  :defer t

  :init
  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash")))

(use-package saveplace
  :ensure nil
  :init
  (setq-default save-place t))

(use-package imenu
  :ensure nil
  :defer t

  :init
  (defun imenu-use-package ()
    (add-to-list 'imenu-generic-expression
                 '("Used Packages"
                   "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))

  (add-hook 'emacs-lisp-mode-hook 'imenu-use-package))

(use-package ediff
  :ensure nil
  :init
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)

  (defun blaenk/is-fullscreen ()
    (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth)))

  (defun blaenk/go-fullscreen ()
    (interactive)
    (set-frame-parameter nil 'fullscreen 'fullboth))

  (defun blaenk/un-fullscreen ()
    (set-frame-parameter nil 'fullscreen nil))

  (defun blaenk/toggle-ediff-wide-display ()
    "Turn off wide-display mode (if was enabled) before quitting ediff."
    (when ediff-wide-display-p
      (ediff-toggle-wide-display)))

  (defun blaenk/ediff-prepare ()
    (turn-off-hideshow)
    (turn-off-fci-mode)
    (visual-line-mode -1)
    (whitespace-mode -1))

  (defun blaenk/ediff-start ()
    (interactive)
    (blaenk/go-fullscreen))

  (defun blaenk/ediff-quit ()
    (interactive)
    (blaenk/toggle-ediff-wide-display)
    (blaenk/un-fullscreen))

  (add-hook 'ediff-prepare-buffer-hook 'blaenk/ediff-prepare)
  (add-hook 'ediff-startup-hook 'blaenk/ediff-start)
  (add-hook 'ediff-suspend-hook 'blaenk/ediff-quit 'append)
  (add-hook 'ediff-quit-hook 'blaenk/ediff-quit 'append))

(use-package stickyfunc-enhance
  :config
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  (semantic-mode 1))

(use-package dtrt-indent)

(use-package clean-aindent-mode)

(use-package python
  :ensure nil
  :defer t

  :config
  ;; TODO other PEP8 stuff
  (add-hook 'python-mode-hook (lambda () (setq fill-column 79)))

  (let ((ipython (executable-find "ipython")))
    (when ipython
      (setq python-shell-interpreter ipython))))

(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'eldoc-mode))

(use-package company-anaconda
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-anaconda)))

;; TODO
;; use (member "Symbola" (font-family-list))
;; to fall back on unicode icons
(use-package fontawesome
  :init
  (defun blaenk/set-char-widths (alist)
    (while (char-table-parent char-width-table)
      (setq char-width-table (char-table-parent char-width-table)))
    (dolist (pair alist)
      (let ((width (car pair))
            (chars (cdr pair))
            (table (make-char-table nil)))
        (dolist (char chars)
          (set-char-table-range table char width))
        (optimize-char-table table)
        (set-char-table-parent table char-width-table)
        (setq char-width-table table))))

  (blaenk/set-char-widths
   `((2 . (,(string-to-char (fontawesome "cloud")))))))

(use-package tern
  :config
  (add-hook 'js2-mode-hook 'tern-mode))

(use-package company-tern
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-tern)))

(use-package company-cabal
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-cabal)))

(use-package latex-preview-pane)

(use-package paradox)

(use-package solarized-theme
  :config
  (make-face 'mode-line-column-face)
  (make-face 'mode-line-branch-face)
  (make-face 'mode-line-anzu-face)
  (make-face 'mode-line-mode-name-face)
  (make-face 'mode-line-read-only-face)
  (make-face 'mode-line-modified-face)
  (make-face 'mode-line-remote-face)
  (make-face 'mode-line-stem-face)

  (defvar blaenk/theme-loaded nil)

  (defun blaenk/load-theme ()
    (if blaenk/theme-loaded
        (enable-theme 'solarized-light)
      (progn
        (load-theme 'solarized-light t)
        (setq blaenk/theme-loaded t)))

    (solarized-with-color-variables 'light
      (custom-theme-set-faces
        'solarized-light
        `(whitespace-trailing ((,class (:background ,red-l))))
        `(whitespace-tab ((,class (:background ,red-l))))
        `(whitespace-line ((,class (:underline t))))

        `(evil-quickscope-first-face ((,class (:weight bold :underline t))))
        `(evil-quickscope-second-face ((,class (:foreground ,red-lc :weight bold))))

        `(mmm-default-submode-face ((,class (:background unspecified))))

        `(hl-line ((,class (:background
                            ,(solarized-color-blend
                              base02 base03 0.5)))))
        `(region ((,class (:background ,base02))))

        `(sp-show-pair-match-face ((,class (:foreground unspecified
                                            :background ,base02
                                            :weight bold))))

        `(show-paren-match ((,class (:foreground unspecified
                                      :background ,base02
                                      :weight bold
                                      ))))

        `(sp-show-pair-mismatch-face ((,class (:foreground unspecified
                                                :background ,red
                                                :weight normal))))

        `(highlight-quoted-quote ((,class (:foreground ,red-hc))))

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

        `(header-line
          ((,class (:inverse-video unspecified
                    :overline ,s-mode-line-underline
                    :underline ,s-mode-line-underline
                    :foreground ,s-mode-line-fg
                    :background ,s-mode-line-bg
                    ))))

        `(mode-line
          ((,class (:inverse-video unspecified
                    :overline ,s-mode-line-underline
                    :underline ,s-mode-line-underline
                    :foreground ,s-mode-line-fg
                    :background ,s-mode-line-bg
                    ))))

        `(mode-line-column-face
          ((,class (:background ,base03))))

        `(mode-line-branch-face
          ((,class (:background ,base0
                    :foreground "white"
                    :weight bold))))

        `(mode-line-anzu-face
          ((,class (:background ,orange-l
                    :foreground "white"
                    :weight bold))))

        `(mode-line-mode-name-face
          ((,class (:background ,cyan-l
                    :foreground "white"
                    :weight bold))))

        `(mode-line-read-only-face
          ((,class (:background ,red-l
                    :foreground "white"))))

        `(mode-line-modified-face
          ((,class (:background ,green-l
                    :foreground "white"
                    :weight bold
                    ))))

        `(mode-line-remote-face
          ((,class (:background ,green-lc
                    :foreground "white"
                    :weight bold
                    ))))

        `(mode-line-stem-face
          ((,class (:foreground ,base0))))

        `(mode-line-inactive
          ((,class (:inverse-video unspecified
                    :overline ,s-mode-line-underline
                    :underline ,s-mode-line-underline
                    :foreground ,s-mode-line-inactive-fg
                    :background ,s-mode-line-inactive-bg
                    ))))
        )))

  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (blaenk/load-theme))))
    (blaenk/load-theme)))

(use-package auto-package-update)

(use-package lua-mode
  :mode "\\.lua$"
  :interpreter "lua")

(use-package ag
  :init
  (setq ag-reuse-buffers t)
  (setq ag-project-root-function
        (lambda (file-or-dir-name)
          (let ((default-directory file-or-dir-name))
            (projectile-project-root))))
  (setq ag-highlight-search t))

(use-package anzu
  :diminish anzu-mode
  :init
  (defun blaenk/anzu-update (here total)
    (when anzu--state
      (let ((status (cond
                     ((eq anzu--state 'search) (format " %s of %d%s "
                                                       (anzu--format-here-position here total)
                                                       total (if anzu--overflow-p "+" "")))
                     ((eq anzu--state 'replace-query) (format " %d replace " total))
                     ((eq anzu--state 'replace) (format " %d of %d " here total)))))
        (propertize status 'face 'anzu-mode-line))))

  (setq anzu-mode-line-update-function 'blaenk/anzu-update)
  (setq anzu-cons-mode-line-p nil)
  :config
  (add-hook 'anzu-mode-hook (lambda () (make-local-variable 'anzu--state)))
  (global-anzu-mode +1))

(use-package browse-at-remote
  :bind
  ("C-c g o" . browse-at-remote/to-clipboard))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

;; TODO
;; configure thoroughly when used
;; https://github.com/clojure-emacs/cider
(use-package cider
  :init
  (setq cider-auto-mode nil)

  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (add-hook 'cider-mode-hook 'company-mode))

;; TODO
;; requires extra setup
;; choose between ghc and haskell-mode
(use-package ghc)

(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode))

(use-package company
  :config
  (setq company-idle-delay nil
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-require-match 'never
        company-global-modes '(not git-commit-mode)
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t)

  :config
  ;; get back the use of kill word even if company is active
  (define-key company-active-map (kbd "C-w") nil)

  (add-hook 'prog-mode-hook 'company-mode))

(use-package company-statistics
  :config
  (add-hook 'after-init-hook 'company-statistics-mode)
  (company-statistics-mode))

(use-package company-quickhelp
  :init
  (setq company-quickhelp-delay nil)

  :config
  (company-quickhelp-mode 1))

(use-package company-web)

(use-package clojure-mode)

(use-package diminish)

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package emmet-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode))

;; NOTE
;; should adapt helm-descbinds to save prefix keys
(use-package which-key
  :disabled t
  :diminish which-key-mode
  :init
  (setq which-key-use-C-h-for-paging nil))

(use-package evil
  :init
  (setq evil-want-C-w-in-emacs-state t)

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

  ;; FIXME
  ;; a problem is that this leaves whitespace residue
  ;; e.g. o then escape then o?
  (defun blaenk/evil-open-line ()
    (interactive)
    (end-of-visual-line)
    (if (elt (syntax-ppss) 4)
        (progn
          (comment-indent-new-line)
          (evil-insert-state 1)

          (when evil-auto-indent
            (indent-according-to-mode))

          (add-hook 'post-command-hook #'evil-maybe-remove-spaces)
          (setq this-command 'evil-open-below))
      (evil-open-below 1))
    (setq this-command 'evil-open-below))

  (define-key evil-normal-state-map (kbd "o") 'blaenk/evil-open-line)
  ;; (define-key evil-normal-state-map (kbd "O")
  ;;   (lambda ()
  ;;     (interactive)
  ;;     (if (eq (line-number-at-pos (point)) 1)
  ;;         (evil-open-above 1)
  ;;         (progn
  ;;           (previous-line)
  ;;           (blaenk/evil-open-line)))))

  (defun blaenk/kill-line ()
    (interactive)
    (if (looking-back "^[[:space:]]+")
        (kill-line 0)
      (progn
        (let ((beg (point)))
          (back-to-indentation)
          (kill-region beg (point))))))

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

  (evil-mode 1))

(use-package evil-quickscope
  :config
  (global-evil-quickscope-mode 1))

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

(use-package evil-smartparens
  :disabled t)

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
  (define-key evil-normal-state-map (kbd "> a") 'evil-arg-swap-forward))

(use-package buffer-move
  :config
  (with-eval-after-load 'evil
    (define-key evil-window-map (kbd "m k") 'buf-move-up)
    (define-key evil-window-map (kbd "m j") 'buf-move-down)
    (define-key evil-window-map (kbd "m h") 'buf-move-left)
    (define-key evil-window-map (kbd "m l") 'buf-move-right)))

(use-package frame-cmds)

(use-package hydra
  :config
  (defun blaenk/move-splitter-left (arg)
    "Move window splitter left."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun blaenk/move-splitter-right (arg)
    "Move window splitter right."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun blaenk/move-splitter-up (arg)
    "Move window splitter up."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (enlarge-window arg)
      (shrink-window arg)))

  (defun blaenk/move-splitter-down (arg)
    "Move window splitter down."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (shrink-window arg)
      (enlarge-window arg)))

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

    ;; NOTE
    ;; not as intuitive when multiple windows
    ;; ("j" blaenk/move-splitter-down)
    ;; ("k" blaenk/move-splitter-up)
    ;; ("h" blaenk/move-splitter-left)
    ;; ("l" blaenk/move-splitter-right))

    ("j" shrink-window)
    ("k" enlarge-window)
    ("h" shrink-window-horizontally)
    ("l" enlarge-window-horizontally))
  )

(use-package olivetti)

(use-package ace-link
  :config
  (ace-link-setup-default))

(use-package flycheck
  :preface
  (defun blaenk/flycheck-cargo-rust-predicate () (flycheck-buffer-saved-p))

  :init
  (setq-default flycheck-disabled-checkers '(rust emacs-lisp-checkdoc))

  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)

  (flycheck-define-checker blaenk/cargo-rust
    "A Rust syntax checker using cargo rustc.
This syntax checker needs Rust 1.1 or newer.
See URL `http://www.rust-lang.org'."
    :command ("cargo" "rustc" "--" "-Z" "no-trans")
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ": "
            (one-or-more digit) ":" (one-or-more digit) " error: "
            (or
             ;; Multiline errors
             (and (message (minimal-match (one-or-more anything)))
                  " [" (id "E" (one-or-more digit)) "]")
             (message))
            line-end)
     (warning line-start (file-name) ":" line ":" column ": "
              (one-or-more digit) ":" (one-or-more digit) " warning: "
              (message) line-end)
     (info line-start (file-name) ":" line ":" column ": "
           (one-or-more digit) ":" (one-or-more digit) " " (or "note" "help") ": "
           (message) line-end))
    :modes rust-mode
    :predicate blaenk/flycheck-cargo-rust-predicate)

  (add-to-list 'flycheck-checkers 'blaenk/cargo-rust)

  (flycheck-define-checker javascript-flow
    "A JavaScript syntax and style checker using Flow.
See URL `http://flowtype.org/'."
    :command ("flow" source-original)
    :error-patterns
    ((error line-start
            (file-name)
            ":"
            line
            ":"
            (minimal-match (one-or-more not-newline))
            ": "
            (message (minimal-match (and (one-or-more anything) "\n")))
            line-end))
    :modes js-mode)

  (add-to-list 'flycheck-checkers 'javascript-flow))

(use-package flycheck-irony
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package flycheck-rust
  :disabled t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package hl-todo
  :config
  (add-hook 'prog-mode-hook 'hl-todo-mode))

;; NOTE
;; see moo-jump-local
(use-package function-args
  :init
  (set-default 'semantic-case-fold t)
  :config
  (fa-config-default))

(use-package ggtags
  :config
  (add-hook 'prog-mode-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'rust-mode)
                (ggtags-mode 1)))))

(use-package gist
  :bind
  (("C-c g g s" . gist-region-or-buffer-private)
   ("C-c g g p" . gist-region-or-buffer)))

(use-package json-mode
  :init
  (setq json-reformat:indent-width 2))

(use-package systemd)

(use-package highlight-quoted
  :init
  (setq highlight-quoted-highlight-symbols nil)
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode))

(use-package gitconfig-mode)

(use-package gitignore-mode)

(use-package gitattributes-mode)

(use-package git-gutter-fringe
  :bind
  ;; NOTE mnemonic is 'git ruler'
  (("C-c g r t" . git-gutter:toggle)
   ("C-c g r n" . git-gutter:next-hunk)
   ("C-c g r p" . git-gutter:previous-hunk)))

(use-package markdown-mode
  :mode
  (("\\.markdown\\'" . gfm-mode)
   ("\\.md\\'" . gfm-mode))

  :init
  (setq markdown-enable-math t)

  :config
  (add-hook 'gfm-mode-hook (lambda ()
                             (interactive)
                             (set-face-attribute
                              'markdown-comment-face nil
                              :strike-through nil)
                             (evil-leader/set-key
                               "k" 'beginning-of-defun
                               "j" 'end-of-defun)))
  (add-hook 'gfm-mode-hook 'whitespace-mode)
  (add-hook 'gfm-mode-hook 'flyspell-mode))

(use-package yaml-mode)

(use-package mmm-mode
  :demand t

  :bind
  ("C-c m" . mmm-parse-buffer)

  :init
  (setq mmm-global-mode 'maybe)
  (setq mmm-parse-when-idle t)

  :config
  (mmm-add-classes
   '((gfm-toml-metadata
      :submode toml-mode
      :front "\\`---$"
      :back "\n---$")))

  (mmm-add-mode-ext-class 'gfm-mode nil 'gfm-toml-metadata)

  (defun blaenk/mmm-markdown-auto-class (lang &optional submode)
    "Define a mmm-mode class for LANG in `markdown-mode' using SUBMODE.
If SUBMODE is not provided, use `LANG-mode' by default."
    (let ((class (intern (concat "gfm-" lang)))
          (submode (or submode (intern (concat lang "-mode"))))
          (front (concat "^``` ?" lang "[\n\r]+"))
          (back "^```"))
      (mmm-add-classes (list (list class :submode submode :front front :back back)))
      (mmm-add-mode-ext-class 'gfm-mode nil class)))

  (mapc 'blaenk/mmm-markdown-auto-class
        '("c"
          "cpp"
          "haskell"
          "html"
          "java"
          "python"
          "ruby"
          "rust"
          ))

  ;; NOTE for when language and mode-name differ
  (blaenk/mmm-markdown-auto-class "shell" 'sh-mode)
  (blaenk/mmm-markdown-auto-class "bash" 'sh-mode)
  (blaenk/mmm-markdown-auto-class "elisp" 'emacs-lisp-mode))

(use-package undo-tree
  :diminish undo-tree-mode

  :init
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)

  :config
  (global-undo-tree-mode)

  (defadvice undo-tree-make-history-save-file-name
      (after undo-tree activate)
    (setq ad-return-value (concat ad-return-value ".gz"))))

(use-package inf-ruby
  :config
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
  (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)
  (inf-ruby-switch-setup))

(use-package enh-ruby-mode)

(use-package paxedit)

(use-package multi-term
  :init
  (setq multi-term-buffer-name "term")
  (setq multi-term-program "/bin/zsh"))

(use-package helm
  :diminish helm-mode

  :bind
  (("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("M-i" . helm-imenu)
   ("C-c h" . helm-command-prefix)
   ("C-x b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files)
   ("C-h a" . helm-apropos)
   ("C-h i" . helm-info-emacs))

  :init
  (setq helm-quick-update t)
  (setq helm-split-window-in-side-p t)
  (setq helm-display-header-line nil)
  (setq helm-autoresize-max-height 30)
  (setq helm-autoresize-min-height 30)
  (setq helm-imenu-execute-action-at-once-if-one nil)

  :config
  (require 'helm-config)
  (helm-autoresize-mode t)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)

  ;; open in horizontal split
  (require 'helm-files)

  (defun blaenk/helm-action-horizontal-split (candidate)
    "Display buffer in horizontal split"
    ;; Select the bottom right window
    (require 'winner)
    ;; Display buffers in new windows
    (dolist (buf (helm-marked-candidates))
      (select-window (split-window-below))
      (if (get-buffer buf)
          (switch-to-buffer buf)
        (find-file buf)))
    (balance-windows))

  (defun blaenk/helm-action-vertical-split (candidate)
    "Display buffer in vertical split"
    ;; Select the bottom right window
    (require 'winner)
    ;; Display buffers in new windows
    (dolist (buf (helm-marked-candidates))
      (select-window (split-window-right))
      (if (get-buffer buf)
          (switch-to-buffer buf)
       (find-file buf)))
    (balance-windows))

  (add-to-list 'helm-find-files-actions
               '("Display buffer in horizontal split" .
                 blaenk/helm-action-horizontal-split) t)

  (add-to-list 'helm-type-buffer-actions
               '("Display buffer in horizontal split" .
                 blaenk/helm-action-horizontal-split) t)

  (add-to-list 'helm-find-files-actions
               '("Display buffer in vertical split" .
                 blaenk/helm-action-vertical-split) t)

  (add-to-list 'helm-type-buffer-actions
               '("Display buffer in vertical split" .
                 blaenk/helm-action-vertical-split) t)

  (defun blaenk/helm-horizontal-split ()
    (interactive)
    (with-helm-alive-p
      (helm-quit-and-execute-action 'blaenk/helm-action-horizontal-split)))

  (defun blaenk/helm-vertical-split ()
    (interactive)
    (with-helm-alive-p
      (helm-quit-and-execute-action 'blaenk/helm-action-vertical-split)))

  (define-key helm-find-files-map
    (kbd "M-h") 'blaenk/helm-horizontal-split)

  (define-key helm-buffer-map
    (kbd "M-h") 'blaenk/helm-horizontal-split)

  (define-key helm-find-files-map
    (kbd "M-v") 'blaenk/helm-vertical-split)

  (define-key helm-buffer-map
    (kbd "M-v") 'blaenk/helm-vertical-split)

  ;; helm-solarized-colors
  (defun blaenk/solarized-put-color (color table)
    (puthash (downcase (symbol-value color)) (symbol-name color) table))

  (defmacro blaenk/create-solarized-color-table ()
    (let ((table (make-hash-table :test 'equal))
          (colors '(yellow orange red magenta
                    violet blue cyan green))
          (grays '(base03 base02 base01 base00
                   base0 base1 base2 base3)))
      (dolist (color grays table)
        (blaenk/solarized-put-color color table))

      (dolist (color colors table)
        (let* ((s-n (symbol-name color))
               (light (intern (concat s-n "-lc")))
               (dark (intern (concat s-n "-hc"))))
          (blaenk/solarized-put-color dark table)
          (blaenk/solarized-put-color color table)
          (blaenk/solarized-put-color light table)))))

  ;; TODO infer variant
  (defvar blaenk/solarized-colors-table
    (solarized-with-color-variables 'light
      (blaenk/create-solarized-color-table)))

  (defun blaenk/hash-table-keys (table)
    (let (keys)
      (maphash (lambda (k v) (push k keys)) table)
      keys))

  (defun blaenk/solarized-colors-get-hex (candidate)
    "Get color name."
    (string-trim
     (with-temp-buffer
       (insert candidate)
       (goto-char (point-min))
       (search-forward-regexp "\\s-\\{2,\\}")
       (delete-region (point) (point-max))
       (buffer-string))))

  (defun blaenk/solarized-colors-init-source ()
    (unless (helm-candidate-buffer)
      (save-selected-window
        (list-colors-display
         (blaenk/hash-table-keys blaenk/solarized-colors-table)
         "*Solarized Colors*")
        (message nil))
      (helm-init-candidates-in-buffer
          'global
        (with-current-buffer (get-buffer "*Solarized Colors*")
          (buffer-string)))
      (let ((windows (get-buffer-window-list "*Solarized Colors*")))
        (while windows
          (delete-window (pop windows))))
      (kill-buffer "*Solarized Colors*")
      ))

  (defun blaenk/solarized-colors-get-name (candidate)
    (gethash (blaenk/solarized-colors-get-hex candidate) blaenk/solarized-colors-table))

  (defun blaenk/solarized-color-insert-name (candidate)
    (with-helm-current-buffer
      (insert (blaenk/solarized-colors-get-name candidate))))

  (defun blaenk/solarized-color-run-insert-name ()
    "Insert name of color from `helm-source-colors'"
    (interactive)
    (with-helm-alive-p
      (helm-quit-and-execute-action 'blaenk/solarized-color-insert-name)))

  (defun blaenk/solarized-color-kill-name (candidate)
    (kill-new (blaenk/solarized-colors-get-name candidate)))

  (defun blaenk/solarized-color-run-kill-name ()
    "Kill name of color from `helm-source-colors'"
    (interactive)
    (with-helm-alive-p
      (helm-quit-and-execute-action 'blaenk/solarized-color-kill-name)))

  (defvar blaenk/solarized-color-map
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map helm-map)
      (define-key map (kbd "C-c n") 'blaenk/solarized-color-run-kill-name)
      (define-key map (kbd "C-c N") 'blaenk/solarized-color-run-insert-name)
      map))

  (defvar blaenk/solarized-colors-source
    (helm-build-in-buffer-source "Solarized Colors"
      :init 'blaenk/solarized-colors-init-source
      :get-line 'buffer-substring
      :keymap blaenk/solarized-color-map
      :persistent-help "Insert name"
      :persistent-action 'blaenk/solarized-color-insert-name
      :action
      '(("Insert Name (C-c N)" . blaenk/solarized-color-insert-name)
        ("Copy Name (C-c n)" . blaenk/solarized-color-kill-name))))

  (defun helm-solarized-colors ()
    (interactive)
    (helm :sources '(blaenk/solarized-colors-source)
          :buffer "*helm solarized colors*"))

  (helm-mode 1))

(use-package helm-mt
  :bind ("C-c t" . helm-mt)

  :config
  (define-key helm-mt/keymap
    (kbd "M-h") 'blaenk/helm-horizontal-split)

  (define-key helm-mt/keymap
    (kbd "M-v") 'blaenk/helm-vertical-split))

(use-package helm-open-github)

(use-package helm-mode-manager)

(use-package helm-c-yasnippet)

(use-package helm-unicode
  :bind ("C-x 8 RET" . helm-unicode))

(use-package helm-ag)

(use-package helm-gtags
  :diminish helm-gtags-mode

  ;; TODO audit
  :init
  (setq
   helm-gtags-ignore-case t
   helm-gtags-auto-update t
   helm-gtags-use-input-at-cursor t
   helm-gtags-pulse-at-cursor t)

  :config
  (helm-gtags-mode))

(use-package helm-descbinds
  :config
  (helm-descbinds-mode))

(use-package helm-projectile
  :diminish projectile-mode

  :config
  (define-key helm-projectile-find-file-map
    (kbd "M-h") 'blaenk/helm-horizontal-split)

  (define-key helm-projectile-find-file-map
    (kbd "M-v") 'blaenk/helm-vertical-split)

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

(use-package visual-regexp)

(use-package multiple-cursors)

(use-package sx)

(use-package erlang
  :defer t)

(use-package scala-mode2)

(use-package go-mode)

(use-package company-go
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) '(company-go))
              (company-mode))))

(use-package less-css-mode)

(use-package robe
  :config
  (with-eval-after-load 'company
    (push 'company-robe company-backends))

  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'enh-ruby-mode-hook 'robe-mode))

(use-package scss-mode
  :mode "\\.scss\\'")

(use-package elixir-mode)

(use-package alchemist)

(use-package racket-mode)

(use-package helm-flycheck)

(use-package helm-flyspell)

(use-package swiper
  :init
  (setq ivy-use-virtual-buffers t)

  :bind
  (("C-s" . swiper)
   ("C-r" . swiper)
   ("C-c C-r" . ivy-resume)
   ([f6] . ivy-resume)))

(use-package irony
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)

  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun blaenk/irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'blaenk/irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; TODO
;; requires completion server?
(use-package company-irony
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-irony))

  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))

(use-package swift-mode
  :if (eq system-type 'darwin)

  :config
  (with-eval-after-load 'flycheck
    (add-to-list 'flycheck-checkers 'swift)))

(use-package irony-eldoc)

(use-package vimrc-mode)

;; TODO ensure imenu
(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node"

  :init
  (setq-default js2-basic-offset 2))

(use-package cmake-mode)

(use-package cmake-font-lock
  :config
  (autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
  (add-hook 'cmake-mode-hook 'cmake-font-lock-activate))

(use-package skewer-mode)

(use-package git-messenger)

(use-package git-timemachine)

(use-package magit
  :diminish
  (magit-wip-after-save-local-mode
   magit-wip-before-change-mode)

  :bind
  (("C-c g s" . magit-status)
   ("C-c g p" . magit-dispatch-popup))

  :init
  (setq magit-save-repository-buffers 'dontask)
  (setq magit-push-always-verify 'dontask)
  (setq magit-refs-show-commit-count 'all)

  :config
  (magit-add-section-hook
   'magit-status-sections-hook
   'magit-insert-unpulled-module-commits)

  (defun blaenk/pull-request-url ()
    "Build the URL or the pull requestion on GitHub corresponding
to the current branch. Uses Magit."
    (interactive)
    (format "%s/compare/%s"
            (replace-regexp-in-string
             (rx (and
                  string-start
                  (1+ any)
                  "github.com:"
                  (group (1+ any))
                  ".git"
                  string-end))
             "https://github.com/\\1"
             (magit-get "remote" (magit-get-remote) "url"))
            (magit-get-current-branch)))

  (defun blaenk/open-pr ()
    (interactive)
    (browse-url (blaenk/pull-request-url)))

  (add-hook 'magit-status-mode-hook 'delete-other-windows)

  (with-eval-after-load 'magit-ediff
    (add-hook 'magit-ediff-quit-hook 'blaenk/ediff-quit))

  ;; NOTE remove if perf hit
  (magit-wip-after-save-mode)
  (magit-wip-after-apply-mode)
  (magit-wip-before-change-mode)

  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
  (add-hook 'git-commit-setup-hook 'fci-mode))

(use-package magit-gh-pulls
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

(use-package magit-gitflow)

(use-package projectile
  :init
  (setq projectile-completion-system 'helm)

  :config
  (projectile-global-mode))

(use-package perspective
  :config
  (persp-mode))

(use-package persp-projectile
  :config
  (define-key projectile-command-map
    (kbd "p") 'projectile-persp-switch-project))

(use-package zeal-at-point)

(use-package dash-at-point)

(use-package racer
  :init
  (setq racer-rust-src-path "~/code/rust/rust/src")
  (setq racer-cmd "~/code/rust/racer/target/release/racer")

  :config
  (setq company-tooltip-align-annotations t)
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package rainbow-mode
  :demand t
  :diminish rainbow-mode
  :bind
  ("C-c r c" . rainbow-mode)

  :config
  ;; disable highlighting color names
  (setq rainbow-x-colors nil)
  (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package rainbow-blocks
  :bind
  ("C-c r b" . rainbow-blocks-mode))

(use-package rainbow-delimiters
  :demand t
  :bind
  ("C-c r d" . rainbow-delimiters-mode)

  :config
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(use-package color-identifiers-mode
  :bind
  ("C-c r i" . color-identifiers-mode))

(use-package relative-line-numbers
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
  :init
  (add-hook 'rust-mode-hook
            (lambda ()
              (set (make-local-variable 'compile-command) "cargo build"))))

(use-package on-parens)

(use-package smartparens
  :diminish smartparens-mode

  :init
  (setq sp-show-pair-from-inside t)
  (setq sp-show-pair-delay 0)
  (setq sp-highlight-pair-overlay nil)
  (setq sp-cancel-autoskip-on-backward-movement nil)

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

      (defun blaenk/evil-goto-char (pos)
        (when (evil-normal-state-p) (decf pos))
        (goto-char pos))

      ;; TODO evil-commentary delegating sp-comment wrapper?

      (define-key evil-normal-state-map (kbd "> )")
        (lambda ()
          (interactive)
          (on-parens-forward-slurp)
          ;; get back on paren
          (sp-get (sp-get-enclosing-sexp) (blaenk/evil-goto-char :end))))

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
          (sp-get (sp-get-enclosing-sexp) (blaenk/evil-goto-char (+ :beg 1)))))

      ;; TODO
      ;; this should be turned off when smartparens is not on
      ;; NOTE can use evil-define-motion to create motions out of these
      ;; (define-key evil-normal-state-map (kbd "W") 'on-parens-forward-sexp)
      ;; (define-key evil-normal-state-map (kbd "E") 'on-parens-forward-sexp-end)
      ;; (define-key evil-normal-state-map (kbd "g E") 'on-parens-backward-sexp-end)
      ;; (define-key evil-normal-state-map (kbd "B") 'on-parens-backward-sexp)

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
            (blaenk/evil-goto-char end))))

      (defmacro blaenk/save-position (&rest body)
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

        (blaenk/save-position
          (sp-transpose-sexp)))

      (defun move-form-backward (pos &optional arg)
        "move a form backward"
        (interactive "d *p")

        (blaenk/save-position
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

(use-package toml-mode)

(use-package web-mode
  :mode "\\.html?\\'"
  :init
  (setq web-mode-enable-current-element-highlight t))

(use-package wgrep)

(use-package wgrep-ag)

(use-package discover-my-major
  :bind
  ("C-h C-m" . discover-my-major))

(use-package yasnippet
  :commands (yas-reload-all yas-global-mode yas-minor-mode))

(use-package fill-column-indicator
  :config
  (with-eval-after-load 'magit
    (add-hook 'git-commit-setup-hook 'fci-mode)))

(use-package bug-reference
  :ensure nil
  :defer t

  :init
  (setq bug-reference-bug-regexp "\\(\
[Ii]ssue ?#\\|\
[Bb]ug ?#\\|\
[Pp]atch ?#\\|\
RFE ?#\\|\
PR [a-z-+]+/\
\\)\\([0-9]+\\(?:#[0-9]+\\)?\\)")

  (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
  (add-hook 'prog-mode-hook #'bug-reference-prog-mode))

(use-package sx)

(use-package goto-addr
  :ensure nil
  :defer t

  :init
  (add-hook 'prog-mode-hook #'goto-address-prog-mode)
  (add-hook 'text-mode-hook #'goto-address-mode)
  (goto-address-mode))

(use-package bug-reference-github
  :config
  (add-hook 'find-file-hook 'bug-reference-github-set-url-format))

(use-package ace-window
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

(use-package shackle
  :init
  (setq shackle-rules
        '((help-mode :select t)
          (compilation-mode :noselect t)))

  :config
  (shackle-mode))

(use-package dired
  :ensure nil
  :defer t

  :init
  (setq dired-auto-revert-buffer t)
  (setq dired-listing-switches "-alhF")

  (when (or (memq system-type '(gnu gnu/linux))
            (string= (file-name-nondirectory insert-directory-program) "gls"))
    (setq dired-listing-switches
          (concat dired-listing-switches " --group-directories-first -v"))))

(use-package dired-x
  :ensure nil
  :bind
  (("C-x C-j" . dired-jump))

  :init
  (add-hook 'dired-mode-hook #'dired-omit-mode)

  :config
  (setq dired-omit-verbose nil)

  (when (eq system-type 'darwin)
    (setq dired-guess-shell-gnutar "tar")))

(use-package reveal-in-osx-finder
  :if (eq system-type 'darwin))

(use-package highlight-numbers
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package helm-make)

;; TODO
;; this also cons mode-line
;; need a more robust way of reformatting mode-line
;; perhaps advice on force-mode-line-update?
(use-package eldoc
  :ensure nil
  :defer t

  :config
  (add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode))

(use-package restclient)

(use-package company-restclient
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-restclient)))

(use-package company-emoji)

(use-package compile
  :ensure nil
  :config
  (setq compilation-scroll-output 'first-error)
  (setq compilation-ask-about-save nil)
  (setq compilation-set-skip-threshold 0)
  (setq compilation-always-kill t))

(use-package floobits)

(use-package list-environment)

(use-package pkgbuild-mode)

(use-package groovy-mode
  :mode "\\.gradle\\'")

(use-package gradle-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
