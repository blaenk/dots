(require 'use-package)

(defun blaenk/toggle-header-line ()
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

  ;; TODO
  ;; also take 'center' param
  ;; it won't be aligned center or anything, it will simply follow
  ;; 'left' but it'll be truncated to fit
  ;; should happen in stages, e.g. the file-name should show
  ;; basename for better fit, then not show which-func
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
       (format " %s " indicator)
       'face
       (if is-evil
           'mode-line-evil-mode-indicator-face
         'mode-line-emacs-mode-indicator-face))))

  (defun blaenk/format-flycheck-errors ()
    (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
           (errors (or (cdr (assq 'error error-counts)) 0))
           (warnings (or (cdr (assq 'warning error-counts)) 0))
           (error-str (if (= errors 0)
                          ""
                        (propertize (format " %s " errors)
                                    'face 'mode-line-flycheck-errors-face)))
           (warning-str (if (= warnings 0)
                            ""
                          (propertize (format " %s " warnings)
                                      'face 'mode-line-flycheck-warnings-face))))
      (format "%s%s" warning-str error-str)))

  (defun blaenk/flycheck-mode-line ()
    (pcase flycheck-last-status-change
      (`not-checked nil)
      (`no-checker nil)
      (`suspicious (propertize "suspicious" 'face 'error))
      (`errored (propertize "errored" 'face 'error))
      (`interrupted (propertize "interrupted" 'face 'error))
      (`running (propertize
                 (format " %s " (fontawesome "refresh"))
                 'face 'mode-line-flycheck-checking-face))
      (`finished (blaenk/format-flycheck-errors))))

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
          (let* ((project-root (when (projectile-project-p)
                                 (projectile-project-root)))
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
                    (if for-title "—" "")
                    (propertize directory 'face 'mode-line-stem-face)
                    (propertize file-name 'face 'mode-line-buffer-id)))
        (propertize " %b " 'face 'mode-line-buffer-id))))

  (defun blaenk/which-func ()
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
          (:eval (blaenk/evil-indicator))
          (:propertize
           (:eval (blaenk/remote-mode-line))
           face mode-line-remote-face)
          ;; TODO
          ;; truncate this to fit
          (:eval (blaenk/file-name nil))
          (which-func-mode (:eval (blaenk/which-func)))
          ))

  (setq mode-line-right
        `(
          (:propertize
           (:eval
            (when (blaenk/is-modified) " + "))
           face mode-line-modified-face)
          (:eval (blaenk/flycheck-mode-line))
          (:propertize
           (:eval (when buffer-read-only
                    (concat " " (fontawesome "lock") " ")))
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
(setq frame-title-format '(:eval (blaenk/file-name t)))
