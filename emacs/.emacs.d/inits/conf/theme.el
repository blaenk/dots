(require 'use-package)
(require 'conf/common)

(use-package solarized
  :ensure solarized-theme
  :defer t

  :init
  (eval-when-compile
    (require 'solarized))

  (defvar my-theme-loaded nil)

  (defvar my-theme-variant
    (if (getenv "USE_SOLARIZED_DARK")
        'dark
      'light))

  (defvar my-theme-name
    (if (getenv "USE_SOLARIZED_DARK")
        'solarized-dark
      'solarized-light))

  (defun my-load-theme ()
    (if my-theme-loaded
        (enable-theme my-theme-name)
      (progn
        (load-theme my-theme-name t)
        (setq my-theme-loaded t)))

    (solarized-with-color-variables my-theme-variant
      ;; Taken from magit because they're very nice faces, usable in many
      ;; scenarios. To avoid eager-loading _all_ of magit or waiting for it to
      ;; load, let's pre-define them here.
      (defface magit-diff-added-highlight
        '((((class color) (background light))
           :background "#cceecc"
           :foreground "#22aa22")
          (((class color) (background dark))
           :background "#336633"
           :foreground "#cceecc"))
        "Face for lines in a diff that have been added."
        :group 'magit-faces)

      (defface magit-diff-removed-highlight
        '((((class color) (background light))
           :background "#eecccc"
           :foreground "#aa2222")
          (((class color) (background dark))
           :background "#663333"
           :foreground "#eecccc"))
        "Face for lines in a diff that have been removed."
        :group 'magit-faces)

      ;; Mode line faces
      (defface mode-line-column-face
        `((,class (:background ,s-fringe-bg :foreground ,base1 :weight bold)))
        "column number in mode-line"
        :group 'mode-line-faces
        :group 'basic-faces)

      (defface mode-line-branch-face
        `((,class (:background ,base0
                   :foreground "white"
                   :weight bold)))
        "branch for mode-line"
        :group 'mode-line-faces
        :group 'basic-faces)

      (defface mode-line-anzu-face
        `((,class (:background ,orange-l
                   :foreground "white"
                   :weight bold)))
        "anzu for mode-line"
        :group 'mode-line-faces
        :group 'basic-faces)

      (defface mode-line-mode-name-face
        `((,class (:background ,base0
                   :foreground "white"
                   :weight bold)))
        "mode name for mode-line"
        :group 'mode-line-faces
        :group 'basic-faces)

      (defface mode-line-read-only-face
        `((,class (:background ,red-l
                   :foreground "white")))
        "read-only marker for mode-line"
        :group 'mode-line-faces
        :group 'basic-faces)

      (defface mode-line-emacs-mode-indicator-face
        `((,class (:background ,red-l
                   :foreground "white"
                   :weight bold)))
        "emacs mode marker for mode-line"
        :group 'mode-line-faces
        :group 'basic-faces)

      (defface mode-line-evil-mode-indicator-face
        `((,class (:background ,blue-l
                   :foreground "white"
                   :weight bold)))
        "evil mode marker for mode-line"
        :group 'mode-line-faces
        :group 'basic-faces)

      (defface mode-line-flycheck-no-errors-face
        `((,class (:background ,base0
                   :foreground "white"
                   :weight bold)))
        "flycheck no errors marker for mode-line"
        :group 'mode-line-faces
        :group 'basic-faces)

      (defface mode-line-flycheck-warnings-face
        `((,class (:background ,yellow-lc
                   :foreground "white"
                   :weight bold)))
        "flycheck warnings marker for mode-line"
        :group 'mode-line-faces
        :group 'basic-faces)

      (defface mode-line-flycheck-infos-face
        `((,class (:background ,blue-l
                   :foreground "white"
                   :weight bold)))
        "flycheck infos marker for mode-line"
        :group 'mode-line-faces
        :group 'basic-faces)

      (defface mode-line-flycheck-checking-face
        `((,class (:background ,base01
                   :foreground "white"
                   :weight bold)))
        "flycheck checking marker for mode-line"
        :group 'mode-line-faces
        :group 'basic-faces)

      (defface mode-line-flycheck-errors-face
        `((,class (:background ,red-l
                   :foreground "white"
                   :weight bold)))
        "flycheck errors marker for mode-line"
        :group 'mode-line-faces
        :group 'basic-faces)

      (defface mode-line-which-func-arrow-face
        `((,class (:foreground ,green
                               :weight bold)))
        "which-func separator for mode-line"
        :group 'mode-line-faces
        :group 'basic-faces)

      (defface mode-line-modified-face
        `((,class (:background ,green-l
                   :foreground "white"
                   :weight bold)))
        "buffer modified marker for mode-line"
        :group 'mode-line-faces
        :group 'basic-faces)

      (defface mode-line-remote-face
        `((,class (:background ,green-lc
                   :foreground "white"
                   :weight bold)))
        "remote marker for mode-line"
        :group 'mode-line-faces
        :group 'basic-faces)

      (defface mode-line-stem-face
        `((,class (:foreground ,base0)))
        "buffer file path dirname for mode-line"
        :group 'mode-line-faces
        :group 'basic-faces)

      (custom-theme-set-faces
        my-theme-name
        ;; rtags
        `(rtags-warnline ((,class (:underline "goldenrod4"))))
        `(rtags-errline ((,class (:underline "red"))))
        `(rtags-fixitline ((,class (:underline "blue"))))
        `(rtags-skippedline ((,class (:background ,base02))))

        ;; ucs char
        `(helm-ucs-char ((,class (:foreground unspecified))))

        ;; pulse highlight
        `(pulse-highlight-start-face ((,class (:background ,base02))))

        ;; markdown
        `(markdown-pre-face ((,class (:foreground unspecified))))
        `(markdown-language-keyword-face ((,class (:weight bold))))
        `(markdown-comment-face ((,class (:strike-through nil))))

        ;; fringe
        `(fringe ((,class (:foreground ,base02))))

        ;; line numbers
        `(line-number ((,class (:weight normal :underline nil
                                :foreground ,s-fringe-fg
                                :background ,s-fringe-bg))))
        `(line-number-current-line ((,class (:inherit line-number
                                             :foreground ,base1
                                             :weight bold))))

        ;; whitespace
        `(whitespace-trailing ((,class (:background ,red-l))))
        `(whitespace-tab ((,class (:background ,red-l))))
        `(whitespace-line ((,class (:underline t))))
        `(whitespace-space-after-tab ((,class (:foreground ,red-l))))
        `(whitespace-space-before-tab ((,class (:foreground ,red-l))))
        `(whitespace-indentation ((,class (:background unspecified
                                           :foreground ,base02
                                           :inverse-video unspecified
                                           :weight unspecified))))

        ;; cargo
        `(cargo-process--standard-face ((,class (:weight bold))))
        `(cargo-process--warning-face ((,class (:background ,base02))))
        `(cargo-process--error-face ((,class (:inherit magit-diff-removed-highlight))))
        `(cargo-process--ok-face ((,class (:inherit magit-diff-added-highlight))))
        `(cargo-process--errno-face ((,class (:weight bold :underline t))))

        ;; evil-quickscope
        `(evil-quickscope-first-face ((,class (:weight bold :underline t))))
        `(evil-quickscope-second-face ((,class (:foreground ,red-lc
                                                :weight bold))))

        `(hl-line ((,class (:background
                            ,(solarized-color-blend base02 base03 0.5)))))
        `(region ((,class (:background ,base02))))

        `(show-paren-match ((,class (:foreground unspecified
                                      :background ,base02
                                      :weight bold
                                      ))))

        ;; smartparents
        `(sp-show-pair-match-face ((,class (:foreground unspecified
                                            :background ,base02
                                            :weight bold))))

        `(sp-show-pair-mismatch-face ((,class (:foreground unspecified
                                                :background ,red
                                                :weight normal))))

        `(highlight-quoted-quote ((,class (:foreground ,red-hc))))

        ;; rainbow delimiters
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

        ;; mmm-mode
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

        `(mode-line-inactive
          ((,class (:inverse-video unspecified
                    :overline ,s-mode-line-underline
                    :underline ,s-mode-line-underline
                    :foreground ,s-mode-line-inactive-fg
                    :background ,s-mode-line-inactive-bg
                    ))))

        ;; evil-goggles
        `(evil-goggles-delete-face
          ((,class (:inherit magit-diff-removed-highlight))))
        `(evil-goggles-paste-face
          ((,class (:inherit magit-diff-added-highlight))))

        ;; smerge
       `(smerge-markers
         ((,class (:inherit magit-diff-hunk-heading-highlight))))
       `(smerge-mine
         ((,class (:inherit magit-diff-removed-highlight))))
       `(smerge-other
         ((,class (:inherit magit-diff-added-highlight))))))
    )

  (my-after-frame (my-load-theme)))

(provide 'conf/theme)
