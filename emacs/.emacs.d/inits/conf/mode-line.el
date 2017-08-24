(require 'use-package)
(require 'dash)
(require 'f)
(require 's)

(defun my--render-mode-line (left-line right-line &optional identifier-func)
  (let* ((left-length (string-width left-line))
         (right-length (string-width right-line))
         (max-identifier-width (- (window-total-width)
                                  (+ left-length right-length)))
         (identifier (when identifier-func
                       (funcall identifier-func max-identifier-width)))
         (align (1- (string-width right-line))))
    (concat
     left-line
     (when identifier identifier)
     (propertize " " 'display `(space :align-to (- right ,align)))
     right-line
     )))

;; Note that Eyebrowse uses the same formatters for the mode-line as for the
;; completing-read function, but we want separate formatters for each, so we
;; work around this by locally binding the format strings.

;; For the header-line show the tag name as well.
(defun my--header-line-eyebrowse-component ()
  (when (and (my--is-selected-window-p) header-line-format)
    (let ((eyebrowse-slot-format " %s ")
          (eyebrowse-tagged-slot-format " %s:%t "))
      (eyebrowse-mode-line-indicator))))

;; For the mode-line just show the slot number.
(defun my--mode-line-eyebrowse-component ()
  (when (and (my--is-selected-window-p) (not header-line-format))
    (let ((eyebrowse-slot-format " %s ")
          (eyebrowse-tagged-slot-format " %s "))
      (eyebrowse-mode-line-indicator))))

(defun my--mode-line-which-func-component ()
  (propertize
   (concat " λ " (gethash (selected-window) which-func-table) " ")
   'face 'mode-line-which-function-face))

(defconst my--header-line-left
  `(
    (which-function-mode
     (:eval (my--mode-line-which-func-component)))
    ))

(defconst my--header-line-right
  `(
    (:propertize (:eval (s-wrap mode-name " ")) face mode-line-mode-name-face)
    (eyebrowse-mode
     (:eval (my--header-line-eyebrowse-component)))
    ))

(defconst my--header-line-template
 `(:eval (my--render-mode-line
          (format-mode-line my--header-line-left)
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

;; Currently-selected window code.
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
  "Set the newly selected window after switching frames."

  (my--save-selected-window))

(define-advice select-window
    (:after (window &optional norecord) save-selected-window)
  "Set the newly selected window."

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

(defun my--is-vim-state-enabled-p ()
  (and (or (bound-and-true-p evil-mode)
           (bound-and-true-p evil-local-mode))
       (not (evil-emacs-state-p))))

(defun my--mode-line-evil-component ()
  (let* ((is-vim (my--is-vim-state-enabled-p))
         (indicator (if is-vim "V" "E"))
         (face (if is-vim
                   'mode-line-evil-mode-indicator-face
                 'mode-line-emacs-mode-indicator-face)))
    (propertize (s-wrap indicator " ") 'face face)))

(defun my--is-sudo-edit-p (method host)
  (and (string= method "sudo") (string= host "localhost")))

(defun my--mode-line-remote-component ()
  (when (and (file-remote-p buffer-file-name)
             (tramp-tramp-file-p buffer-file-name))
    (let* ((tramp-file-name-struct (tramp-dissect-file-name buffer-file-name))
           (host (tramp-file-name-host tramp-file-name-struct))
           (method (tramp-file-name-method tramp-file-name-struct)))
      (if (my--is-sudo-edit-p method host)
          (propertize " SUDO " 'face 'mode-line-emacs-mode-indicator-face)
        (propertize
         (s-wrap (concat my--cloud-icon " " host) " ")
         'face 'mode-line-remote-face)
        ))))

(defun my--mode-line-format-error (count face)
  (when (and count (> count 0))
    (propertize (s-wrap (number-to-string count) " ") 'face face)))

(defun my--mode-line-format-flycheck-errors ()
  (if (flycheck-has-current-errors-p)
      (let-alist (flycheck-count-errors flycheck-current-errors)
        (let* ((error-counts (flycheck-count-errors flycheck-current-errors)))
          (concat
           (my--mode-line-format-error .info 'mode-line-flycheck-infos-face)
           (my--mode-line-format-error .warning 'mode-line-flycheck-warnings-face)
           (my--mode-line-format-error .error 'mode-line-flycheck-errors-face))))
    (propertize " ✔ " 'face 'mode-line-flycheck-no-errors-face)))

(defun my--mode-line-flycheck-component ()
  (pcase flycheck-last-status-change
    (`not-checked nil)
    (`no-checker nil)
    (`suspicious (propertize " ⁇ " 'face 'mode-line-flycheck-warnings-face))
    (`errored (propertize " ‼ " 'face 'mode-line-flycheck-errors-face))
    (`interrupted (propertize " ⊣ " 'face 'mode-line-flycheck-errors-face))
    (`running (propertize " … " 'face 'mode-line-flycheck-checking-face))
    (`finished (my--mode-line-format-flycheck-errors))))

(defun my--mode-line-compile-component ()
  (when (derived-mode-p 'compilation-mode)
    (concat
     (my--mode-line-format-error compilation-num-infos-found 'mode-line-flycheck-infos-face)
     (my--mode-line-format-error compilation-num-warnings-found 'mode-line-flycheck-warnings-face)
     (my--mode-line-format-error compilation-num-errors-found 'mode-line-flycheck-errors-face))))

(defun my--mode-line-git-status-component ()
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

(defvar-local my--mode-line-git-status-component-cache ""
  "Cache the vc-git-status response")

(defun my--mode-line-git-branch-component ()
  (-when-let* ((_ buffer-file-name)
               (rev (vc-working-revision buffer-file-name 'Git))
               (disp-rev (or (vc-git--symbolic-ref buffer-file-name)
                             (substring rev 0 7))))
    (concat (propertize (s-wrap disp-rev " ") 'face 'mode-line-branch-face))))

(defvar-local my--mode-line-git-branch-component-cache ""
  "Cache the vc-git-mode response")

(defun my--mode-line-cache-git-info ()
  (setq-local my--mode-line-git-status-component-cache (my--mode-line-git-status-component))
  (setq-local my--mode-line-git-branch-component-cache (my--mode-line-git-branch-component)))

(add-hook 'find-file-hook #'my--mode-line-cache-git-info)
(add-hook 'after-revert-hook #'my--mode-line-cache-git-info)

(defun my--is-buffer-modified-p ()
  (and (not buffer-read-only) (buffer-modified-p)))

;; Construct the buffer identifier for a buffer backed by a file. This is done
;; by combining: dirname/ + filename, each propertized separately.
(defun my--mode-line-file-identifier (path &optional max-width)
  (let* ((path (if (file-remote-p buffer-file-name)
                   (tramp-file-name-localname (tramp-dissect-file-name buffer-file-name))
                 path))
         (dirname (file-name-as-directory (f-short (file-name-directory path))))
         (filename (f-filename path))
         (propertized-filename
          (propertize filename 'face 'mode-line-buffer-id)))
    (if (> (+ (length dirname) (length filename) 2) max-width)
        propertized-filename
      (concat
       (unless (string= dirname "./")
         (propertize dirname 'face 'mode-line-stem-face))
       propertized-filename))))

;; Construct the buffer identifier for a regular, simple buffer that is not
;; backed by a file nor remote.
(defun my--mode-line-buffer-identifier (&optional max-width)
  (if buffer-file-name
      (my--mode-line-file-identifier buffer-file-name max-width)
    (propertize "%b" 'face 'mode-line-buffer-id)))

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

   (t (my--mode-line-buffer-identifier))))

(defun my--mode-line-buffer-identifier-component (&optional max-width)
  (concat
   " "
   (cond
    ((and buffer-file-name (file-remote-p buffer-file-name))
     (my--mode-line-file-identifier
      (tramp-file-name-localname (tramp-dissect-file-name buffer-file-name))
      max-width))

    ((and (featurep 'projectile) (and buffer-file-name (projectile-project-p)))
     (my--mode-line-file-identifier
      (f-relative buffer-file-name (projectile-project-root))
      max-width))

    (t (my--mode-line-buffer-identifier max-width)))
   " "))

(defun my--mode-line-column-component--linum ()
  (propertize "%4c " 'face 'mode-line-column-face))

(defun my--mode-line-column-component--native ()
  (propertize (format "%%%dc " (+ (line-number-display-width) 2))
              'face 'mode-line-column-face))

(fset 'my--mode-line-column-component (if (< emacs-major-version 26)
                             'my--mode-line-column-component--linum
                           'my--mode-line-column-component--native))

(defun my--mode-line-project-component ()
  (when (and (not (file-remote-p buffer-file-name))
             (featurep 'projectile) (projectile-project-p))
    (propertize
     (s-wrap (projectile-project-name) " ")
     'face 'mode-line-branch-face)))

(defconst my--mode-line-left
      `(
        (my--display-column-number
         (:eval (my--mode-line-column-component)))
        (anzu-mode
         (:propertize
          (:eval (anzu--update-mode-line))
          face mode-line-anzu-face))
        (:eval (my--mode-line-evil-component))
        (buffer-file-name
         (:eval (my--mode-line-project-component)))
        (buffer-file-name
          (:eval (my--mode-line-remote-component)))
        ))

(defconst my--mode-line-right
      `(
        (:propertize
         (:eval (when (my--is-buffer-modified-p) " + "))
         face mode-line-modified-face)
        (edebug-mode
         (:propertize " DBG " face mode-line-edebug-face))
        (global-flycheck-mode
         (:eval (my--mode-line-flycheck-component)))
        (buffer-read-only
         (:propertize
          (:eval (s-wrap my--lock-icon " "))
          face mode-line-read-only-face))
        (:eval (my--mode-line-compile-component))
        (:eval my--mode-line-git-status-component-cache)
        (:eval my--mode-line-git-branch-component-cache)
        (eyebrowse-mode
         (:eval (my--mode-line-eyebrowse-component)))
        ))

(defconst my--default-mode-line-format mode-line-format
  "The default mode-line-format.")

(defconst my--custom-mode-line-format
  `(:eval (my--render-mode-line
           (format-mode-line my--mode-line-left)
           (format-mode-line my--mode-line-right)
           #'my--mode-line-buffer-identifier-component))
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
