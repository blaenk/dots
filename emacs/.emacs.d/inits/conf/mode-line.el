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
  (when (and (stringp default-directory)
             (file-remote-p default-directory))
    (s-wrap (fontawesome "cloud") " ")))

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

(defun my--vc-mode ()
  (let ((noback (replace-regexp-in-string
                 (format "^ %s[-:@!?]" (vc-backend buffer-file-name))
                 ""
                 vc-mode)))
    (s-wrap noback " ")))

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
  (if (and (featurep 'projectile)
           (projectile-project-p))
      (concat
       (projectile-project-name)
       " — "
       (if buffer-file-name
           (f-relative buffer-file-name (projectile-project-root))
         (buffer-name)))
    (my--regular-identification)))

(defun my--regular-identification ()
  (if (and (featurep 'projectile)
           buffer-file-name)
      (my--file-identification buffer-file-name)
    (propertize "%b" 'face 'mode-line-buffer-id)))

(defun my--file-identification (path)
  (let* ((dirname (f-short (f-dirname path)))
         (filename (f-filename path)))
    (concat
     (propertize (f-slash dirname) 'face 'mode-line-stem-face)
     (propertize filename 'face 'mode-line-buffer-id))))

(defun my--buffer-identification ()
  (concat
   (when (and (featurep 'projectile)
              (projectile-project-p))
     (propertize
      (s-wrap (projectile-project-name) " ")
      'face 'mode-line-branch-face))
   " "
   (if (and (featurep 'projectile)
            (and buffer-file-name (projectile-project-p)))
       (let* ((root (projectile-project-root))
              (root-relative (f-relative buffer-file-name root)))
         (my--file-identification root-relative))
     (my--regular-identification))))

(defvar my-mode-line-left
      `(
        (:propertize "%3c " face mode-line-column-face)
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
         (:eval (when buffer-read-only (s-wrap (fontawesome "lock") " ")))
         face mode-line-read-only-face)
        (global-flycheck-mode
         (:eval (my--flycheck-mode-line)))
        (vc-mode
         (:propertize (:eval (my--vc-mode)) face mode-line-branch-face))
        ))

(setq-default
 mode-line-format
 `(:eval (my--render-mode-line
          (format-mode-line my-mode-line-left)
          (format-mode-line my-mode-line-center)
          (format-mode-line my-mode-line-right))))

(setq frame-title-format '(:eval (my--title-format)))

(provide 'conf/mode-line)
