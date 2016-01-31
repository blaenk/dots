(require 'use-package)

(use-package solarized
  :if window-system
  :ensure solarized-theme
  :config
  (eval-when-compile
    (require 'solarized))

  (make-face 'mode-line-column-face)
  (make-face 'mode-line-branch-face)
  (make-face 'mode-line-anzu-face)
  (make-face 'mode-line-mode-name-face)
  (make-face 'mode-line-read-only-face)
  (make-face 'mode-line-emacs-mode-indicator-face)
  (make-face 'mode-line-evil-mode-indicator-face)
  (make-face 'mode-line-flycheck-warnings-face)
  (make-face 'mode-line-flycheck-checking-face)
  (make-face 'mode-line-flycheck-errors-face)
  (make-face 'mode-line-which-func-arrow-face)
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
        `(evil-quickscope-second-face ((,class (:foreground ,red-lc
                                                :weight bold))))

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

        `(mmm-default-submode-face ((,class (:background unspecified))))

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

        `(mode-line-emacs-mode-indicator-face
          ((,class (:background ,red-l
                    :foreground "white"
                    :weight bold))))

        `(mode-line-evil-mode-indicator-face
          ((,class (:background ,blue-l
                    :foreground "white"
                    :weight bold))))

        `(mode-line-flycheck-warnings-face
          ((,class (:background ,yellow-lc
                    :foreground "white"
                    :weight bold))))

        `(mode-line-flycheck-checking-face
          ((,class (:background ,base01
                    :foreground "white"
                    :weight bold))))

        `(mode-line-flycheck-errors-face
          ((,class (:background ,red-l
                    :foreground "white"
                    :weight bold))))

        `(mode-line-which-func-arrow-face
          ((,class (:foreground ,green
                    :weight bold))))

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
        ))
    )

  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (blaenk/load-theme))))
    ;; can get current frame with
    ;; (window-frame (get-buffer-window))
    (blaenk/load-theme)))

;; (use-package helm
;;   :no-require t
;;   :config
;;   ;; helm-solarized-colors
;;   (defun blaenk/solarized-put-color (color table)
;;     (puthash (downcase (symbol-value color)) (symbol-name color) table))

;;   (defmacro blaenk/create-solarized-color-table ()
;;     (let ((table (make-hash-table :test 'equal))
;;           (colors '(yellow orange red magenta
;;                     violet blue cyan green))
;;           (grays '(base03 base02 base01 base00
;;                    base0 base1 base2 base3)))
;;       (dolist (color grays table)
;;         (blaenk/solarized-put-color color table))

;;       (dolist (color colors table)
;;         (let* ((s-n (symbol-name color))
;;                (light (intern (concat s-n "-lc")))
;;                (dark (intern (concat s-n "-hc"))))
;;           (blaenk/solarized-put-color dark table)
;;           (blaenk/solarized-put-color color table)
;;           (blaenk/solarized-put-color light table)))))

;;   ;; TODO infer variant
;;   (defvar blaenk/solarized-colors-table
;;     (solarized-with-color-variables 'light
;;       (blaenk/create-solarized-color-table)))

;;   (defun blaenk/hash-table-keys (table)
;;     (let (keys)
;;       (maphash (lambda (k v) (push k keys)) table)
;;       keys))

;;   (defun blaenk/solarized-colors-get-hex (candidate)
;;     "Get color name."
;;     (string-trim
;;      (with-temp-buffer
;;        (insert candidate)
;;        (goto-char (point-min))
;;        (search-forward-regexp "\\s-\\{2,\\}")
;;        (delete-region (point) (point-max))
;;        (buffer-string))))

;;   (defun blaenk/solarized-colors-init-source ()
;;     (unless (helm-candidate-buffer)
;;       (save-selected-window
;;         (list-colors-display
;;          (blaenk/hash-table-keys blaenk/solarized-colors-table)
;;          "*Solarized Colors*")
;;         (message nil))
;;       (helm-init-candidates-in-buffer
;;           'global
;;         (with-current-buffer (get-buffer "*Solarized Colors*")
;;           (buffer-string)))
;;       (let ((windows (get-buffer-window-list "*Solarized Colors*")))
;;         (while windows
;;           (delete-window (pop windows))))
;;       (kill-buffer "*Solarized Colors*")
;;       ))

;;   (defun blaenk/solarized-colors-get-name (candidate)
;;     (gethash (blaenk/solarized-colors-get-hex candidate) blaenk/solarized-colors-table))

;;   (defun blaenk/solarized-color-insert-name (candidate)
;;     (with-helm-current-buffer
;;       (insert (blaenk/solarized-colors-get-name candidate))))

;;   (defun blaenk/solarized-color-run-insert-name ()
;;     "Insert name of color from `helm-source-colors'"
;;     (interactive)
;;     (with-helm-alive-p
;;       (helm-quit-and-execute-action 'blaenk/solarized-color-insert-name)))

;;   (defun blaenk/solarized-color-kill-name (candidate)
;;     (kill-new (blaenk/solarized-colors-get-name candidate)))

;;   (defun blaenk/solarized-color-run-kill-name ()
;;     "Kill name of color from `helm-source-colors'"
;;     (interactive)
;;     (with-helm-alive-p
;;       (helm-quit-and-execute-action 'blaenk/solarized-color-kill-name)))

;;   (defvar blaenk/solarized-color-map
;;     (let ((map (make-sparse-keymap)))
;;       (set-keymap-parent map helm-map)
;;       (define-key map (kbd "C-c n") 'blaenk/solarized-color-run-kill-name)
;;       (define-key map (kbd "C-c N") 'blaenk/solarized-color-run-insert-name)
;;       map))

;;   (defvar blaenk/solarized-colors-source
;;     (helm-build-in-buffer-source "Solarized Colors"
;;       :init 'blaenk/solarized-colors-init-source
;;       :get-line 'buffer-substring
;;       :keymap blaenk/solarized-color-map
;;       :persistent-help "Insert name"
;;       :persistent-action 'blaenk/solarized-color-insert-name
;;       :action
;;       '(("Insert Name (C-c N)" . blaenk/solarized-color-insert-name)
;;         ("Copy Name (C-c n)" . blaenk/solarized-color-kill-name))))

;;   (defun helm-solarized-colors ()
;;     (interactive)
;;     (helm :sources '(blaenk/solarized-colors-source)
;;           :buffer "*helm solarized colors*")))
