(require 'conf/common)

(deftheme atom-one-dark-ext "Solarized with some modifications")

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

(my-with-atom-one-dark-colors
 (custom-theme-set-faces
  'atom-one-dark
  ;; magit
  `(magit-diff-added
    ((,class (:background "#2a3d35" :foreground "#57ab5a"))))

  `(magit-diff-removed
    ((,class (:background "#442d31" :foreground "#e5534b"))))

  `(magit-diff-added-highlight
    ((,class (:background "#2a3d35" :foreground "#79bc7b"))))

  `(magit-diff-removed-highlight
    ((,class (:background "#442d31" :foreground "#eb7e78"))))

  `(magit-diff-context-highlight
    ((,class (:background unspecified :foreground unspecified))))

  ;; diredfl
  `(diredfl-dir-heading ((,class (:foreground ,atom-one-dark-blue
                                  :background unspecified))))
  `(diredfl-dir-priv ((,class (:foreground ,atom-one-dark-blue))))
  `(diredfl-read-priv ((,class (:foreground ,atom-one-dark-green))))
  `(diredfl-write-priv ((,class (:foreground ,atom-one-dark-orange-2))))
  `(diredfl-exec-priv ((,class (:foreground ,atom-one-dark-red-1))))
  `(diredfl-no-priv ((,class (:background unspecified))))
  `(diredfl-number ((,class (:foreground ,atom-one-dark-purple))))
  `(diredfl-date-time ((,class (:foreground unspecified))))
  `(diredfl-dir-name ((,class (:foreground ,atom-one-dark-cyan))))
  `(diredfl-file-name ((,class (:foreground ,atom-one-dark-green))))
  `(diredfl-ignored-file-name ((,class (:foreground unspecified))))
  `(diredfl-file-suffix ((,class (:weight bold))))
  `(diredfl-executable-flag ((,class (:foreground ,atom-one-dark-red-1))))

  ;; rtags
  `(rtags-errline ((,class (:underline "red"))))
  `(rtags-fixitline ((,class (:underline "blue"))))
  `(rtags-skippedline ((,class (:background ,atom-one-dark-bg-hl))))

  ;; ucs char
  `(helm-ucs-char ((,class (:foreground unspecified))))

  ;; pulse highlight
  `(pulse-highlight-start-face ((,class (:background ,atom-one-dark-bg-hl))))

  ;; markdown
  `(markdown-code-face ((,class (:foreground unspecified :background unspecified :inherit unspecified))))
  `(markdown-metadata-key-face ((,class (:foreground unspecified
                                         :weight bold
                                         :inherit unspecified))))
  `(markdown-inline-code-face ((,class (:foreground unspecified :weight bold))))
  `(markdown-pre-face ((,class (:foreground unspecified))))
  `(markdown-language-keyword-face ((,class (:weight bold))))
  `(markdown-comment-face ((,class (:strike-through nil))))
  `(markdown-header-face ((,class (:foreground ,atom-one-dark-blue :weight bold))))
  `(markdown-footnote-marker-face ((,class (:foreground unspecified :weight bold))))
  `(markdown-url-face ((,class (:foreground unspecified))))
  `(markdown-link-title-face ((,class (:foreground unspecified :inherit unspecified))))
  `(markdown-link-face ((,class (:foreground unspecified :weight bold))))
  `(markdown-blockquote-face ((,class (:foreground unspecified
                                       :inherit unspecified
                                       :slant italic))))

  ;; fringe
  `(fringe ((,class (:foreground ,atom-one-dark-bg-hl))))

  `(bmkp-light-non-autonamed ((,class (:background unspecified))))
  `(bmkp-light-autonamed ((,class (:background unspecified))))
  `(bmkp-light-fringe-non-autonamed ((,class (:background unspecified
                                              :foreground ,"white"))))
  `(bmkp-light-fringe-autonamed ((,class (:background unspecified
                                          :foreground ,"white"))))

  ;; line numbers
  `(line-number ((,class (:weight normal :underline nil
                          :foreground ,"white"
                          :background ,atom-one-dark-gutter))))
  `(line-number-current-line ((,class (:inherit line-number
                                       :foreground ,atom-one-dark-mono-1
                                       :weight bold))))
  `(linum-relative-current-face ((,class (:inherit line-number
                                          :foreground ,atom-one-dark-mono-1
                                          :weight bold))))

  ;; eyebrowse
  `(eyebrowse-mode-line-active ((,class (:background ,atom-one-dark-mono-2 :foreground "white" :weight bold))))
  `(eyebrowse-mode-line-inactive ((,class (:background ,atom-one-dark-mono-3 :foreground "white"))))

  ;; whitespace
  `(whitespace-trailing ((,class (:background ,atom-one-dark-red-1
                                  :foreground unspecified
                                  :inverse-video unspecified))))
  `(whitespace-tab ((,class (:background ,atom-one-dark-red-1
                             :foreground unspecified
                             :inverse-video unspecified))))
  `(whitespace-line ((,class (:foreground unspecified :underline t))))
  `(whitespace-space-after-tab ((,class (:foreground ,atom-one-dark-red-1
                                         :weight unspecified
                                         :inverse-video nil))))
  `(whitespace-space-before-tab ((,class (:foreground ,atom-one-dark-red-1
                                          :background unspecified))))
  `(whitespace-indentation ((,class (:background unspecified
                                     :foreground ,atom-one-dark-bg-hl
                                     :inverse-video unspecified
                                     :weight unspecified))))

  ;; cargo
  `(cargo-process--standard-face ((,class (:weight bold))))
  `(cargo-process--warning-face ((,class (:background ,atom-one-dark-bg-hl))))
  `(cargo-process--error-face ((,class (:inherit magit-diff-removed-highlight))))
  `(cargo-process--ok-face ((,class (:inherit magit-diff-added-highlight))))
  `(cargo-process--errno-face ((,class (:weight bold :underline t))))

  ;; evil-quickscope
  `(evil-quickscope-first-face ((,class (:weight bold :underline t))))
  `(evil-quickscope-second-face ((,class (:foreground ,atom-one-dark-red-1
                                          :weight bold))))

  `(isearch
    ((,class (:foreground unspecified
              :background ,atom-one-dark-orange-2))))

  `(evil-ex-lazy-highlight ((,class (:inherit isearch))))
  `(evil-ex-search
    ((,class (:background ,atom-one-dark-orange-2))))
  `(evil-ex-substitute-matches
    ((,class (:inherit magit-diff-removed-highlight))))
  `(evil-ex-substitute-replacement
    ((,class (:inherit magit-diff-added-highlight))))

  ;; avy
  ;; leading chars
  `(avy-lead-face
    ((,class (:background ,atom-one-dark-orange-2))))

  ;; non-terminating leading chars
  `(avy-lead-face-0
    ((,class (:background ,atom-one-dark-orange-2))))

  ;; matched leading chars
  `(avy-lead-face-1
    ((,class (:background ,atom-one-dark-orange-2))))

  ;; leading chars
  `(avy-lead-face-2 ((,class (:background ,atom-one-dark-green))))

  `(hl-line ((,class (:background ,atom-one-dark-bg-hl))))
  `(lsp-ui-doc-background ((,class ((:inherit hl-line)))))
  `(region ((,class (:background ,atom-one-dark-bg-hl :foreground unspecified))))

  `(show-paren-match ((,class (:foreground unspecified
                               :background ,atom-one-dark-bg-hl
                               :weight bold))))

  ;; smartparents
  `(sp-show-pair-match-face ((,class (:foreground unspecified
                                      :background ,atom-one-dark-bg-hl
                                      :weight bold))))

  `(sp-show-pair-mismatch-face ((,class (:foreground unspecified
                                         :background ,atom-one-dark-red-1
                                         :weight normal))))

  `(highlight-quoted-quote ((,class (:foreground ,atom-one-dark-red-1))))

  ;; rainbow delimiters
  `(rainbow-delimiters-depth-1-face ((,class (:foreground ,atom-one-dark-mono-3))))
  `(rainbow-delimiters-depth-2-face ((,class (:foreground ,atom-one-dark-cyan))))
  `(rainbow-delimiters-depth-3-face ((,class (:foreground ,atom-one-dark-orange-2))))
  `(rainbow-delimiters-depth-4-face ((,class (:foreground ,atom-one-dark-blue))))
  `(rainbow-delimiters-depth-5-face ((,class (:foreground ,atom-one-dark-purple))))
  `(rainbow-delimiters-depth-6-face ((,class (:foreground ,atom-one-dark-green))))
  `(rainbow-delimiters-depth-7-face ((,class (:foreground ,atom-one-dark-orange-2))))
  `(rainbow-delimiters-depth-8-face ((,class (:foreground ,atom-one-dark-blue))))
  `(rainbow-delimiters-depth-9-face ((,class (:foreground ,atom-one-dark-purple))))
  `(rainbow-delimiters-depth-10-face ((,class (:foreground ,atom-one-dark-green))))
  `(rainbow-delimiters-depth-11-face ((,class (:foreground ,atom-one-dark-orange-2))))
  `(rainbow-delimiters-depth-12-face ((,class (:foreground ,atom-one-dark-blue))))
  `(rainbow-delimiters-depth-13-face ((,class (:foreground ,atom-one-dark-purple))))
  `(rainbow-delimiters-unmatched-face
    ((,class (:foreground ,atom-one-dark-mono-2 :background ,atom-one-dark-bg-1 :inverse-video t))))

  `(header-line
    ((,class (:inverse-video unspecified
              :foreground "white"
              :background ,atom-one-dark-gutter
              :box unspecified))))

  ;; mode-line
  `(mode-line-column-face
    ((,class (:background ,atom-one-dark-bg :foreground ,atom-one-dark-mono-1 :weight bold))))

  `(mode-line-zoom-window-face ((,class (:background ,atom-one-dark-purple :foreground "white"))))
  `(mode-line-branch-face ((,class (:background ,atom-one-dark-mono-2 :foreground "white" :weight bold))))
  `(mode-line-edebug-face ((,class (:background ,atom-one-dark-purple :foreground "white" :weight bold))))
  `(mode-line-anzu-face ((,class (:background ,atom-one-dark-orange-1 :foreground "white" :weight bold))))
  `(mode-line-mode-name-face ((,class (:background ,atom-one-dark-mono-2 :foreground "white"))))
  `(mode-line-read-only-face ((,class (:background ,atom-one-dark-red-1 :foreground "white" :weight bold))))
  `(mode-line-emacs-mode-indicator-face
    ((,class (:background ,atom-one-dark-red-1 :foreground "white" :weight bold))))
  `(mode-line-evil-mode-indicator-face
    ((,class (:background ,atom-one-dark-blue :foreground "white" :weight bold))))
  `(mode-line-flycheck-no-errors-face
    ((,class (:background ,atom-one-dark-mono-2 :foreground "white" :weight bold))))
  `(mode-line-flycheck-warnings-face
    ((,class (:background ,atom-one-dark-orange-2 :foreground "white" :weight bold))))
  `(mode-line-flycheck-infos-face
    ((,class (:background ,atom-one-dark-blue :foreground "white" :weight bold))))
  `(mode-line-flycheck-checking-face
    ((,class (:background ,atom-one-dark-mono-3 :foreground "white" :weight bold))))
  `(mode-line-flycheck-errors-face
    ((,class (:background ,atom-one-dark-red-1 :foreground "white" :weight bold))))
  `(mode-line-which-func-separator ((,class (:foreground ,atom-one-dark-green :weight bold))))
  `(mode-line-modified-face
    ((,class (:background ,atom-one-dark-green :foreground "white" :weight bold))))
  `(mode-line-remote-face
    ((,class (:background ,atom-one-dark-green :foreground "white" :weight bold))))
  `(mode-line-stem-face ((,class (:foreground ,atom-one-dark-mono-2))))

  `(mode-line
    ((,class (:inverse-video unspecified
              :foreground "white"
              :background ,atom-one-dark-gutter
              :box unspecified))))

  `(mode-line-inactive
    ((,class (:inverse-video unspecified
              :foreground "white"
              :background ,atom-one-dark-gutter
              :box unspecified))))

  ;; evil-goggles
  `(evil-goggles-delete-face
    ((,class (:inherit magit-diff-removed-highlight))))
  `(evil-goggles-paste-face
    ((,class (:inherit magit-diff-added-highlight))))

  ;; smerge
  `(smerge-markers
    ((,class (:inherit magit-diff-hunk-heading-highlight))))

  `(smerge-base
    ((,class (:inherit magit-diff-base-highlight))))

  `(smerge-mine
    ((,class (:inherit magit-diff-removed-highlight))))
  `(smerge-upper
    ((,class (:inherit smerge-mine))))
  `(smerge-refined-removed
    ((,class (:inherit smerge-upper))))

  `(smerge-other
    ((,class (:inherit magit-diff-added-highlight))))
  `(smerge-lower
    ((,class (:inherit smerge-other))))
  `(smerge-refined-added
    ((,class (:inherit smerge-other))))
  ))
(provide-theme 'atom-one-dark-ext)
