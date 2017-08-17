(require 'use-package)
(require 'dash)
(require 'f)
(require 's)

(defun my-toggle-header-line ()
  (interactive)
  (if header-line-format
      (progn
        (setq header-line-format-save header-line-format)
        (setq header-line-format nil))
    (setq header-line-format header-line-format-save)))

;; (defvar my-selected-window (frame-selected-window))

;; (defun my-set-selected-window ()
;;   "sets the variable `my-selected-window` appropriately"
;;   (when (not (minibuffer-window-active-p (frame-selected-window)))
;;     (setq my-selected-window (frame-selected-window))))

;; (defun my-unset-selected-window ()
;;   "Unsets the variable `my-selected-window` and updates the modeline"
;;   (setq my-selected-window nil)
;;   (force-mode-line-update))

;; (add-hook 'window-configuration-change-hook 'my-set-selected-window)

;; (add-hook 'focus-in-hook 'my-set-selected-window)
;; (add-hook 'focus-out-hook 'my-unset-selected-window)

;;  ;; Executes after the window manager requests that the user's events
;; ;; be directed to a different frame.
;; (defadvice handle-switch-frame
;;     (after my-set-selected-window-after-switch-frame activate)
;;   (my-set-selected-window))

;; (defadvice select-window (after my-select-window activate)
;;   "makes powerline aware of window changes"
;;   (my-set-selected-window))

;; (defun my-selected-window-active ()
;;   "Return whether the current window is active."
;;   (eq my-selected-window (selected-window)))

(defconst my--lock-icon
  (if (not (eq system-type 'windows-nt))
    (fontawesome "lock")
    "🔒"))

(defconst my--cloud-icon
  (if (not (eq system-type 'windows-nt))
    (fontawesome "cloud")
    "☁"))

(defun my--is-evil-on ()
  (and (or
        (bound-and-true-p evil-mode)
        (bound-and-true-p evil-local-mode))
       (not (evil-emacs-state-p))))

(defun my--evil-indicator ()
  (let* ((is-evil (my--is-evil-on))
         (indicator (if is-evil "V" "E")))
    (propertize
     (s-wrap indicator " ")
     'face
     (if is-evil
         'mode-line-evil-mode-indicator-face
       'mode-line-emacs-mode-indicator-face))))

(defun my--render-mode-line (left center right)
  (let* ((available-width (- (window-total-width)
                             (+ (string-width left) (string-width right))))
         (center-fmt
          (if (> (string-width center) available-width)
              (s-truncate (- available-width 1) center)
            (concat
             center
             (propertize
              " " 'display
              `((space :width ,(- available-width (string-width center)))))))))
    (concat left center-fmt right)))

(defun my--remote-mode-line ()
  (when (and buffer-file-name (file-remote-p buffer-file-name))
    (let ((host (tramp-file-name-host (tramp-dissect-file-name buffer-file-name))))
      (s-wrap (concat my--cloud-icon " " host) " "))))

(defun my--format-flycheck (count face)
  (when count
    (propertize (s-wrap (number-to-string count) " ") 'face face)))

(defun my--format-flycheck-errors ()
  (if (flycheck-has-current-errors-p)
      (let-alist (flycheck-count-errors flycheck-current-errors)
        (let* ((error-counts (flycheck-count-errors flycheck-current-errors)))
          (concat
           (my--format-flycheck .info 'mode-line-flycheck-infos-face)
           (my--format-flycheck .warning 'mode-line-flycheck-warnings-face)
           (my--format-flycheck .error 'mode-line-flycheck-errors-face))))
    (propertize " ✔ " 'face 'mode-line-flycheck-no-errors-face)))

(defun my--flycheck-mode-line ()
  (pcase flycheck-last-status-change
    (`not-checked nil)
    (`no-checker nil)
    (`suspicious (propertize " suspicious " 'face 'mode-line-flycheck-warnings-face))
    (`errored (propertize " errored " 'face 'mode-line-flycheck-errors-face))
    (`interrupted (propertize " interrupted " 'face 'mode-line-flycheck-errors-face))
    (`running (propertize
               " R "
               'face 'mode-line-flycheck-checking-face))
    (`finished (my--format-flycheck-errors))))

(defun my--format-error (count face)
  (when (> count 0)
    (propertize (s-wrap (number-to-string count) " ") 'face face)))

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
                    ('ignored      '("I" . mode-line-flycheck-checking-face))
                    ('unregistered '("." . mode-line-flycheck-checking-face))
                    ('removed      '("-" . mode-line-flycheck-errors-face))
                    ('edited       '("#" . mode-line-branch-face))
                    ('added        '("+" . mode-line-branch-face))
                    ('conflict     '("C" . mode-line-flycheck-errors-face))
                    (_ nil)
                    ))))
    (-when-let ((label . face) state)
      (propertize (s-wrap label " ") 'face face))))

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

(defun my-cache-vc-info ()
  (setq-local my--vc-git-status-cache (my--vc-git-status))
  (setq-local my--vc-git-mode-cache (my--vc-git-mode)))

(add-hook 'find-file-hook #'my-cache-vc-info)
(add-hook 'after-revert-hook #'my-cache-vc-info)

(defun my--is-modified ()
  (and (not buffer-read-only) (buffer-modified-p)))

(setq-default
 header-line-format-save
 `(
   (:propertize
    (:eval (s-wrap (format-mode-line mode-name) " "))
    face mode-line-mode-name-face)
   (which-function-mode
    (:eval (concat " → " (gethash (selected-window) which-func-table))))
   ))

;; TODO
;; make functions private
(defun my--title-format ()
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

(defun my--regular-identification ()
  (if (and (featurep 'projectile)
           buffer-file-name
           (not (file-remote-p buffer-file-name)))
      (my--file-identification buffer-file-name)
    (propertize "%b" 'face 'mode-line-buffer-id)))

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

(defun my--column-number--linum ()
  (when my-display-column-number
    (propertize "%4c " 'face 'mode-line-column-face)))

(defun my--column-number--native ()
  (when my-display-column-number
    (propertize (format "%%%dc " (+ (line-number-display-width) 2))
                'face 'mode-line-column-face)))

(fset 'my--column-number
      (if (< emacs-major-version 26)
          'my--column-number--linum
        'my--column-number--native))

(defvar my-mode-line-left
      `(
        (:eval (my--column-number))
        (anzu-mode
         (:propertize
          (:eval (anzu--update-mode-line))
          face mode-line-anzu-face))
        (:eval (my--evil-indicator))
        (:propertize
         (:eval (my--remote-mode-line))
         face mode-line-remote-face)
        ))

(defvar my-mode-line-center
      `(
        (:eval (my--buffer-identification))
        ))

(defvar my-mode-line-right
      `(
        (:propertize
         (:eval (when (my--is-modified) " + ")) face mode-line-modified-face)
        (:propertize
         (:eval (when buffer-read-only (s-wrap my--lock-icon " ")))
         face mode-line-read-only-face)
        (global-flycheck-mode
         (:eval (my--flycheck-mode-line)))
        (:eval (my--compilation-mode-line))
        (:eval my--vc-git-status-cache)
        (:eval my--vc-git-mode-cache)
        (eyebrowse-mode
         (:eval
          ;; Eyebrowse uses the same formatters for the mode-line as for the
          ;; completing-read function, but we want separate formatters for each,
          ;; so we create local bindings here.
          (let ((eyebrowse-slot-format " %s ")
                (eyebrowse-tagged-slot-format " %s "))
            (eyebrowse-mode-line-indicator))))))

(setq-default
 mode-line-format
 `(:eval (my--render-mode-line
          (format-mode-line my-mode-line-left)
          (format-mode-line my-mode-line-center)
          (format-mode-line my-mode-line-right))))

(setq frame-title-format '(:eval (my--title-format)))

(provide 'conf/mode-line)
