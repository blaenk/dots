(require 'use-package)
(require 'f)
(require 's)
(require 'dash)

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

(defun my-setup-mode-line ()
  (defun my-is-evil-on ()
    (if (evil-emacs-state-p)
        nil
      (or
       (bound-and-true-p evil-mode)
       (bound-and-true-p evil-local-mode))))

  ;; TODO
  ;; also take 'center' param
  ;; it won't be aligned center or anything, it will simply follow
  ;; 'left' but it'll be truncated to fit
  ;; should happen in stages, e.g. the file-name should show
  ;; basename for better fit, then not show which-func
  (defun my-render-mode-line (left center right)
    (let* ((available-width (-
                             (window-total-width)
                             (+ (string-width left) (string-width right))))
           (center-fmt
            (if (> (string-width center) available-width)
                (s-truncate available-width center)
              (concat center
                (propertize " " 'display
                  `((space :width ,(- available-width (string-width center))))))
              )))
      (concat left center-fmt right)))

  (defun my-is-remote-buffer ()
    (and (stringp default-directory)
         (file-remote-p default-directory)))

  (defun my-remote-mode-line ()
    (when (my-is-remote-buffer)
      (concat " " (fontawesome "cloud") " ")))

  (defun my-evil-indicator ()
    (let* ((is-evil (my-is-evil-on))
           (indicator (if is-evil "V" "E")))
      (propertize
       (format " %s " indicator)
       'face
       (if is-evil
           'mode-line-evil-mode-indicator-face
         'mode-line-emacs-mode-indicator-face))))

  (defun my-emacs-indicator ()
    (when (not (my-is-evil-on))
      (propertize
       " E "
       'face
       'mode-line-emacs-mode-indicator-face)))

  (defun my-format-flycheck-errors ()
    (if (flycheck-has-current-errors-p)
        (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
               (errors (or (cdr (assq 'error error-counts)) 0))
               (warnings (or (cdr (assq 'warning error-counts)) 0))
               (infos (or (cdr (assq 'info error-counts)) 0))
               (info-str (if (= infos 0)
                             ""
                           (propertize (format " %s " infos)
                                       'face 'mode-line-flycheck-infos-face)))
               (error-str (if (= errors 0)
                              ""
                            (propertize (format " %s " errors)
                                        'face 'mode-line-flycheck-errors-face)))
               (warning-str (if (= warnings 0)
                                ""
                              (propertize (format " %s " warnings)
                                          'face 'mode-line-flycheck-warnings-face))))
          (format "%s%s%s" info-str warning-str error-str))
      ;; FIXME
      ;; if there's no branch then this can't omit the space
      (propertize " ✔ " 'face 'mode-line-flycheck-no-errors-face)))

  (defun my-flycheck-mode-line ()
    (pcase flycheck-last-status-change
      (`not-checked nil)
      (`no-checker nil)
      (`suspicious (propertize "suspicious" 'face 'error))
      (`errored (propertize "errored" 'face 'error))
      (`interrupted (propertize "interrupted" 'face 'error))
      (`running (propertize
                 " R "
                 'face 'mode-line-flycheck-checking-face))
      (`finished (my-format-flycheck-errors))))

  ;; TODO
  ;; this goes stale easily, have to revert-buffer to fix
  (defun my-vc-branch ()
    (or
     (when (and vc-mode (buffer-file-name))
       (let ((backend (vc-backend (buffer-file-name))))
         (when backend
           (let* ((rev (vc-working-revision (buffer-file-name) backend))
                  (rev (if (string-match "[0-9a-f]\\{7,40\\}" rev)
                           (substring rev 0 7)
                         rev)))
             (format " %s " rev)))))
     ""))

  (defun my-is-modified ()
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

  ;; TODO
  ;; would be nice to not show the directory if it didn't fit
  (defun my-file-name (for-title)
    (let* ((name (buffer-file-name)))
      (if name
          (let* ((project-root (when (projectile-project-p)
                                 (projectile-project-root)))
                 (name (if project-root
                           (replace-regexp-in-string
                            (regexp-quote project-root) ""
                            name)
                         (f-short name)))
                 (directory (or (f-slash (f-dirname name)) ""))
                 (file-name (f-filename name)))
            (format "%s%s %s%s "
                    (if project-root
                        (propertize
                         (format " %s " (projectile-project-name))
                         'face 'mode-line-branch-face)
                      "")
                    (if for-title "—" "")
                    (propertize directory 'face 'mode-line-stem-face)
                    (propertize file-name 'face 'mode-line-buffer-id)))
        (propertize " %b " 'face 'mode-line-buffer-id))))

  (defun my-which-func ()
    (let ((loc (gethash (selected-window) which-func-table))
          (arrow (propertize "→" 'face 'mode-line-which-func-arrow-face)))
      (if loc
          (format "%s %s " arrow loc)
        "")))

  (setq mode-line-left
        `(
          (:propertize "%3c " face mode-line-column-face)
          (anzu-mode
           (:propertize
            (:eval (anzu--update-mode-line))
            face
            mode-line-anzu-face))
          (:eval (my-emacs-indicator))
          (:propertize
           (:eval (my-remote-mode-line))
           face mode-line-remote-face)
          ))

  (setq mode-line-center
        `(
          ;; TODO
          ;; truncate this to fit
          (:eval (my-file-name nil))
          (which-func-mode (:eval (my-which-func)))
          ))

  (setq mode-line-right
        `(
          (:propertize
           (:eval
            (when (my-is-modified) " + "))
           face mode-line-modified-face)
          (:propertize
           (:eval (when buffer-read-only
                    (concat " " (fontawesome "lock") " ")))
           face mode-line-read-only-face)
          (:eval (my-flycheck-mode-line))
          (:propertize (:eval (my-vc-branch))
                       face mode-line-branch-face)
          ))

  (setq-default
   mode-line-format
   `(:eval (my-render-mode-line
            (format-mode-line mode-line-left)
            (format-mode-line mode-line-center)
            (format-mode-line mode-line-right)))))

(my-setup-mode-line)
(setq frame-title-format '(:eval (my-file-name t)))

(provide 'conf/mode-line)
