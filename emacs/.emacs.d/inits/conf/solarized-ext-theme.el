(require 'conf/common)

(deftheme solarized-ext "Solarized with some modifications")

(defface magit-diff-added-highlight nil
  "Face for lines in a diff that have been added."
  :group 'magit-faces)

(defface magit-diff-removed-highlight nil
  "Face for lines in a diff that have been removed."
  :group 'magit-faces)

(defface mode-line-column-face nil
  "Column number in mode-line."
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-zoom-window-face nil
  "Zoom window indicator in mode-line"
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-which-function-face nil
  "Which-function in mode-line."
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-branch-face nil
  "Branch for mode-line."
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-edebug-face nil
  "EDebug indicator for mode-line."
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-anzu-face nil
  "Anzu for mode-line."
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-mode-name-face nil
  "Mode name for header-line."
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-read-only-face nil
  "Read-only marker for mode-line."
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-emacs-mode-indicator-face nil
  "Emacs mode marker for mode-line."
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-evil-mode-indicator-face nil
  "Evil mode marker for mode-line."
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-flycheck-no-errors-face nil
  "Flycheck 'no errors' marker for mode-line."
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-flycheck-warnings-face nil
  "Flycheck 'warnings' marker for mode-line."
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-flycheck-infos-face nil
  "Flycheck 'infos' marker for mode-line."
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-flycheck-checking-face nil
  "Flycheck 'checking' marker for mode-line."
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-flycheck-errors-face nil
  "Flycheck 'errors' marker for mode-line."
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-which-func-separator nil
  "Which-func separator for header-line."
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-modified-face nil
  "Buffer-modified marker for mode-line."
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-remote-face nil
  "Remote marker for mode-line."
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-stem-face nil
  "Buffer file path dirname for mode-line."
  :group 'mode-line-faces
  :group 'basic-faces)

(my-with-solarized-colors
 (custom-theme-set-faces
  'solarized-ext
  ;; magit
  `(magit-diff-added-highlight
    ((,light-class (:background "#cceecc" :foreground "#22aa22"))
     (,dark-class (:background "#336633" :foreground "#cceecc"))))

  `(magit-diff-removed-highlight
    ((,light-class (:background "#eecccc" :foreground "#aa2222"))
     (,dark-class (:background "#663333" :foreground "#eecccc"))))

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
  `(markdown-code-face ((,class (:foreground unspecified :background unspecified))))
  `(markdown-metadata-key-face ((,class (:foreground unspecified
                                         :weight bold
                                         :inherit unspecified))))
  `(markdown-inline-code-face ((,class (:foreground unspecified :weight bold))))
  `(markdown-pre-face ((,class (:foreground unspecified))))
  `(markdown-language-keyword-face ((,class (:weight bold))))
  `(markdown-comment-face ((,class (:strike-through nil))))
  `(markdown-header-face ((,class (:foreground ,blue :weight bold))))
  `(markdown-footnote-marker-face ((,class (:foreground unspecified :weight bold))))
  `(markdown-url-face ((,class (:foreground unspecified))))
  `(markdown-link-title-face ((,class (:foreground unspecified :inherit unspecified))))
  `(markdown-link-face ((,class (:foreground unspecified :weight bold))))
  `(markdown-blockquote-face ((,class (:foreground unspecified
                                       :inherit unspecified
                                       :slant italic))))

  ;; fringe
  `(fringe ((,class (:foreground ,base02))))

  `(bmkp-light-non-autonamed ((,class (:background unspecified))))
  `(bmkp-light-autonamed ((,class (:background unspecified))))
  `(bmkp-light-fringe-non-autonamed ((,class (:background unspecified
                                              :foreground ,s-fringe-fg))))
  `(bmkp-light-fringe-autonamed ((,class (:background unspecified
                                          :foreground ,s-fringe-fg))))

  ;; line numbers
  `(line-number ((,class (:weight normal :underline nil
                          :foreground ,s-fringe-fg
                          :background ,s-fringe-bg))))
  `(line-number-current-line ((,class (:inherit line-number
                                       :foreground ,base1
                                       :weight bold))))
  `(linum-relative-current-face ((,class (:inherit line-number
                                          :foreground ,base1
                                          :weight bold))))

  ;; eyebrowse
  `(eyebrowse-mode-line-active ((,class (:background ,base0 :foreground "white" :weight bold))))
  `(eyebrowse-mode-line-inactive ((,class (:background ,base01 :foreground "white"))))

  ;; whitespace
  `(whitespace-trailing ((,class (:background ,red-l
                                  :foreground unspecified
                                  :inverse-video unspecified))))
  `(whitespace-tab ((,class (:background ,red-l
                             :foreground unspecified
                             :inverse-video unspecified))))
  `(whitespace-line ((,class (:foreground unspecified :underline t))))
  `(whitespace-space-after-tab ((,class (:foreground ,red-l
                                         :weight unspecified
                                         :inverse-video nil))))
  `(whitespace-space-before-tab ((,class (:foreground ,red-l
                                          :background unspecified))))
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

  `(isearch
    ((,class (:foreground unspecified
              :background ,(solarized-color-blend yellow-lc base03 0.3)))))

  `(evil-ex-lazy-highlight ((,class (:inherit isearch))))
  `(evil-ex-search
    ((,class (:background ,(solarized-color-blend yellow-lc base03 0.7)))))
  `(evil-ex-substitute-matches
    ((,class (:inherit magit-diff-removed-highlight))))
  `(evil-ex-substitute-replacement
    ((,class (:inherit magit-diff-added-highlight))))

  ;; avy
  ;; leading chars
  `(avy-lead-face
    ((,class (:background ,(solarized-color-blend yellow-lc base03 0.3)))))

  ;; non-terminating leading chars
  `(avy-lead-face-0
    ((,class (:background ,(solarized-color-blend yellow-lc base03 0.3)))))

  ;; matched leading chars
  `(avy-lead-face-1
    ((,class (:background ,(solarized-color-blend yellow-lc base03 0.7)))))

  ;; leading chars
  `(avy-lead-face-2 ((,class (:background ,green-lc))))

  `(hl-line ((,class (:background ,(solarized-color-blend base02 base03 0.5)))))
  `(region ((,class (:background ,base02 :foreground unspecified))))

  `(show-paren-match ((,class (:foreground unspecified
                               :background ,base02
                               :weight bold))))

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

  `(header-line
    ((,class (:inverse-video unspecified
              :overline ,s-mode-line-underline
              :underline ,s-mode-line-underline
              :foreground ,s-mode-line-fg
              :background ,s-mode-line-bg
              :box unspecified))))

  ;; mode-line
  `(mode-line-column-face
    ((,class (:background ,s-fringe-bg :foreground ,base1 :weight bold))))

  `(mode-line-zoom-window-face ((,class (:background ,magenta :foreground "white" :weight bold))))
  `(mode-line-which-function-face ((,class (:background ,blue-l :foreground "white"))))
  `(mode-line-branch-face ((,class (:background ,base0 :foreground "white" :weight bold))))
  `(mode-line-edebug-face ((,class (:background ,violet :foreground "white" :weight bold))))
  `(mode-line-anzu-face ((,class (:background ,orange-l :foreground "white" :weight bold))))
  `(mode-line-mode-name-face ((,class (:background ,base0 :foreground "white"))))
  `(mode-line-read-only-face ((,class (:background ,red-l :foreground "white"))))
  `(mode-line-emacs-mode-indicator-face
    ((,class (:background ,red-l :foreground "white" :weight bold))))
  `(mode-line-evil-mode-indicator-face
    ((,class (:background ,blue-l :foreground "white" :weight bold))))
  `(mode-line-flycheck-no-errors-face
    ((,class (:background ,base0 :foreground "white" :weight bold))))
  `(mode-line-flycheck-warnings-face
    ((,class (:background ,yellow-lc :foreground "white" :weight bold))))
  `(mode-line-flycheck-infos-face
    ((,class (:background ,blue-l :foreground "white" :weight bold))))
  `(mode-line-flycheck-checking-face
    ((,class (:background ,base01 :foreground "white" :weight bold))))
  `(mode-line-flycheck-errors-face
    ((,class (:background ,red-l :foreground "white" :weight bold))))
  `(mode-line-which-func-separator ((,class (:foreground ,green :weight bold))))
  `(mode-line-modified-face
    ((,class (:background ,green-l :foreground "white" :weight bold))))
  `(mode-line-remote-face
    ((,class (:background ,green-lc :foreground "white" :weight bold))))
  `(mode-line-stem-face ((,class (:foreground ,base0))))

  `(mode-line
    ((,class (:inverse-video unspecified
              :overline ,s-mode-line-underline
              :underline ,s-mode-line-underline
              :foreground ,s-mode-line-fg
              :background ,s-mode-line-bg
              :box unspecified))))

  `(mode-line-inactive
    ((,class (:inverse-video unspecified
              :overline ,s-mode-line-underline
              :underline ,s-mode-line-underline
              :foreground ,s-mode-line-inactive-fg
              :background ,s-mode-line-inactive-bg
              :box unspecified))))

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
    ((,class (:inherit magit-diff-added-highlight)))))
 )

(provide-theme 'solarized-ext)
