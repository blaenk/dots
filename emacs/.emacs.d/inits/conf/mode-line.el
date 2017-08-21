(require 'use-package)
(require 'dash)
(require 'f)
(require 's)

(defun my--render-mode-line (left center right)
  (let* ((available-width (- (window-total-width)
                             (+ (string-width left) (string-width right))))
         (center-fmt
          (if (> (string-width center) available-width)
              (s-truncate (- available-width 1) center)
            (concat
             center
             (propertize
              " "
              'display `((space :width ,(- available-width (string-width center)))))
             ))))
    (concat left center-fmt right)))

;; Note that Eyebrowse uses the same formatters for the mode-line as for the
;; completing-read function, but we want separate formatters for each, so we
;; work around this by locally binding the format strings.

;; For the header-line show the tag name as well.
(defun my--eyebrowse-indicator-header-line ()
  (when (and (my--is-selected-window-p) header-line-format)
    (let ((eyebrowse-slot-format " %s ")
          (eyebrowse-tagged-slot-format " %s:%t "))
      (eyebrowse-mode-line-indicator))))

;; For the mode-line just show the slot number.
(defun my--eyebrowse-indicator-mode-line ()
  (when (and (my--is-selected-window-p) (not header-line-format))
    (let ((eyebrowse-slot-format " %s ")
          (eyebrowse-tagged-slot-format " %s "))
      (eyebrowse-mode-line-indicator))))

(defconst my--header-line-left
  `(
    ))

(defconst my--header-line-center
  `(
    (which-function-mode
     (:eval
      (propertize
       (concat " λ " (gethash (selected-window) which-func-table) " ")
       'face 'mode-line-which-function-face)
       ))
    ))

(defconst my--header-line-right
  `(
    (:propertize
     (:eval (s-wrap mode-name " "))
     face mode-line-mode-name-face)
    (eyebrowse-mode
     (:eval (my--eyebrowse-indicator-header-line)))
    ))

(defconst my--header-line-template
 `(:eval (my--render-mode-line
          (format-mode-line my--header-line-left)
          (format-mode-line my--header-line-center)
          (format-mode-line my--header-line-right))))

(defun my-toggle-header-line ()
  "Toggle visibility of the header-line."
  (interactive)

  (if header-line-format
      (kill-local-variable 'header-line-format)
    (setq-local header-line-format my--header-line-template))

  (force-mode-line-update))

(my-map
  "t h" 'my-toggle-header-line)

(defvar my--selected-window (frame-selected-window)
  "The currently-selected window.")

(defun my--save-selected-window ()
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq my--selected-window (frame-selected-window))))

(add-hook 'window-configuration-change-hook #'my--save-selected-window)
(add-hook 'focus-in-hook #'my--save-selected-window)

(defun my--clear-selected-window ()
  (setq my--selected-window nil)
  (force-mode-line-update))

(add-hook 'focus-out-hook #'my--clear-selected-window)

(define-advice handle-switch-frame
    (:after (event) save-selected-window-after-switch-frame)
  (my--save-selected-window))

(define-advice select-window
    (:after (window &optional norecord) save-selected-window)
  (my--save-selected-window))

(defun my--is-selected-window-p ()
  (eq my--selected-window (selected-window)))

(defconst my--lock-icon
  (if (not (eq system-type 'windows-nt))
    (fontawesome "lock")
    "🔒"))

(defconst my--cloud-icon
  (if (not (eq system-type 'windows-nt))
    (fontawesome "cloud")
    "☁"))

(defun my--is-evil-on-p ()
  (and (or (bound-and-true-p evil-mode)
           (bound-and-true-p evil-local-mode))
       (not (evil-emacs-state-p))))

(defun my--evil-indicator ()
  (let* ((is-evil (my--is-evil-on-p))
         (indicator (if is-evil "V" "E"))
         (face (if is-evil
                   'mode-line-evil-mode-indicator-face
                 'mode-line-emacs-mode-indicator-face)))
    (propertize (s-wrap indicator " ") 'face face)))

(defun my--remote-mode-line ()
  (when (file-remote-p buffer-file-name)
    (let ((host (tramp-file-name-host (tramp-dissect-file-name buffer-file-name))))
      (s-wrap (concat my--cloud-icon " " host) " "))))

(defun my--format-error (count face)
  (when (and count (> count 0))
    (propertize (s-wrap (number-to-string count) " ") 'face face)))

(defun my--format-flycheck-errors ()
  (if (flycheck-has-current-errors-p)
      (let-alist (flycheck-count-errors flycheck-current-errors)
        (let* ((error-counts (flycheck-count-errors flycheck-current-errors)))
          (concat
           (my--format-error .info 'mode-line-flycheck-infos-face)
           (my--format-error .warning 'mode-line-flycheck-warnings-face)
           (my--format-error .error 'mode-line-flycheck-errors-face))))
    (propertize " ✔ " 'face 'mode-line-flycheck-no-errors-face)))

(defun my--flycheck-mode-line ()
  (pcase flycheck-last-status-change
    (`not-checked nil)
    (`no-checker nil)
    (`suspicious (propertize " ⁇ " 'face 'mode-line-flycheck-warnings-face))
    (`errored (propertize " ‼ " 'face 'mode-line-flycheck-errors-face))
    (`interrupted (propertize " ⊣ " 'face 'mode-line-flycheck-errors-face))
    (`running (propertize " … " 'face 'mode-line-flycheck-checking-face))
    (`finished (my--format-flycheck-errors))))

(defun my--compilation-mode-line ()
  (when (derived-mode-p 'compilation-mode)
    (concat
     (my--format-error compilation-num-infos-found 'mode-line-flycheck-infos-face)
     (my--format-error compilation-num-warnings-found 'mode-line-flycheck-warnings-face)
     (my--format-error compilation-num-errors-found 'mode-line-flycheck-errors-face))))

(defun my--vc-git-status ()
  (-when-let* ((_ buffer-file-name)
               (rev (vc-working-revision buffer-file-name 'Git))
               (state
                (when buffer-file-name
                  ;; I wanted to use `vc-state' here but for some reason, on ignored
                  ;; files, it straight-up returns nil, whereas `vc-git-state' does
                  ;; correctly return `ignored'.
                  (pcase (vc-git-state buffer-file-name)
                    ('ignored      '("." . mode-line-branch-face))
                    ('unregistered '("." . mode-line-branch-face))
                    ('removed      '("-" . mode-line-branch-face))
                    ('edited       '("#" . mode-line-branch-face))
                    ('added        '("+" . mode-line-branch-face))
                    ('conflict     '("‼" . mode-line-branch-face))
                    (_ nil)
                    )))
               ((label . face) state))
    (propertize (s-wrap label " ") 'face face)))

(defvar-local my--vc-git-status-cache ""
  "Cache the vc-git-status response")

(defun my--vc-git-mode ()
  (-when-let* ((_ buffer-file-name)
               (rev (vc-working-revision buffer-file-name 'Git))
               (disp-rev (or (vc-git--symbolic-ref buffer-file-name)
                             (substring rev 0 7))))
    (concat (propertize (s-wrap disp-rev " ") 'face 'mode-line-branch-face))))

(defvar-local my--vc-git-mode-cache ""
  "Cache the vc-git-mode response")

(defun my--cache-vc-info ()
  (setq-local my--vc-git-status-cache (my--vc-git-status))
  (setq-local my--vc-git-mode-cache (my--vc-git-mode)))

(add-hook 'find-file-hook #'my--cache-vc-info)
(add-hook 'after-revert-hook #'my--cache-vc-info)

(defun my--is-modified-p ()
  (and (not buffer-read-only) (buffer-modified-p)))

;; TODO
;; Remove tramp conditions for projectile, it seems to have been resolved
;; upstream with PR bbatsov/projectile#1129
(defun my--frame-title-format ()
  (cond
   ((and buffer-file-name (file-remote-p buffer-file-name))
    (let ((tramp-vec (tramp-dissect-file-name buffer-file-name)))
      (concat
       (tramp-file-name-host tramp-vec)
       " — "
       (f-short (tramp-file-name-localname tramp-vec)))))

   ((and (featurep 'projectile) (projectile-project-p))
    (concat
     (projectile-project-name)
     " — "
     (if buffer-file-name
         (f-relative buffer-file-name (projectile-project-root))
       (buffer-name))))

   (t (my--regular-identification))))

(defun my--file-identification (path)
  (let* ((path (if (file-remote-p buffer-file-name)
                   (tramp-file-name-localname (tramp-dissect-file-name buffer-file-name))
                 path))
         (dirname (file-name-as-directory (f-short (f-dirname path))))
         (filename (f-filename path)))
    (concat
     (unless (equal dirname "./")
       (propertize dirname 'face 'mode-line-stem-face))
     (propertize filename 'face 'mode-line-buffer-id))))

(defun my--buffer-identification ()
  (concat
   (when (and buffer-file-name (not (file-remote-p buffer-file-name))
              (featurep 'projectile) (projectile-project-p))
     (propertize
      (s-wrap (projectile-project-name) " ")
      'face 'mode-line-branch-face))
   " "
   (cond
    ((and buffer-file-name (file-remote-p buffer-file-name))
     (my--file-identification
      (tramp-file-name-localname (tramp-dissect-file-name buffer-file-name))))

    ((and (featurep 'projectile) (and buffer-file-name (projectile-project-p)))
     (my--file-identification
      (f-relative buffer-file-name (projectile-project-root))))

    (t (my--regular-identification)))))

(defun my--regular-identification ()
  (if (and (featurep 'projectile)
           buffer-file-name
           (not (file-remote-p buffer-file-name)))
      (my--file-identification buffer-file-name)
    (propertize "%b" 'face 'mode-line-buffer-id)))

(defun my--column-number--linum ()
  (propertize "%4c " 'face 'mode-line-column-face))

(defun my--column-number--native ()
  (propertize (format "%%%dc " (+ (line-number-display-width) 2))
              'face 'mode-line-column-face))

(fset 'my--column-number (if (< emacs-major-version 26)
                             'my--column-number--linum
                           'my--column-number--native))

(defconst my--mode-line-left
      `(
        (my--display-column-number
         (:eval (my--column-number)))
        (anzu-mode
         (:propertize
          (:eval (anzu--update-mode-line))
          face mode-line-anzu-face))
        (:eval (my--evil-indicator))
        (buffer-file-name
         (:propertize
          (:eval (my--remote-mode-line))
          face mode-line-remote-face))
        ))

(defconst my--mode-line-center
      `(
        (:eval (my--buffer-identification))
        ))

(defconst my--mode-line-right
      `(
        (:propertize
         (:eval (when (my--is-modified-p) " + "))
         face mode-line-modified-face)
        (edebug-mode
         (:propertize " DBG " face mode-line-edebug-face))
        (global-flycheck-mode
         (:eval (my--flycheck-mode-line)))
        (buffer-read-only
         (:propertize
          (:eval (s-wrap my--lock-icon " "))
          face mode-line-read-only-face))
        (:eval (my--compilation-mode-line))
        (:eval my--vc-git-status-cache)
        (:eval my--vc-git-mode-cache)
        (eyebrowse-mode
         (:eval (my--eyebrowse-indicator-mode-line)))
        ))

(defconst my--default-mode-line-format mode-line-format
  "The default mode-line-format.")

(defconst my--custom-mode-line-format
  `(:eval (my--render-mode-line
           (format-mode-line my--mode-line-left)
           (format-mode-line my--mode-line-center)
           (format-mode-line my--mode-line-right)))
  "My custom mode-line-format.")

(defun my-toggle-custom-mode-line (arg)
  "Toggle between custom and default mode-line.

If ARG is given, do this for all buffers."
  (interactive "P")

  (let ((format (if (eq mode-line-format my--default-mode-line-format)
                   my--custom-mode-line-format
                 my--default-mode-line-format)))
    (if arg
        (progn
          (setq-default mode-line-format format)

          (dolist (buffer (buffer-list))
            (with-current-buffer buffer
              (kill-local-variable 'mode-line-format)))

          (force-mode-line-update 'all))
      (progn
        (setq-local mode-line-format format)
        (force-mode-line-update)))))

(my-map
  "t m" 'my-toggle-custom-mode-line)

(setq-default mode-line-format my--custom-mode-line-format)

(setq frame-title-format '(:eval (my--frame-title-format)))

(provide 'conf/mode-line)
