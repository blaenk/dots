(require 'straight)
(require 'use-package)
(require 'conf/common)
(require 'conf/atom-one-dark-ext)

(use-package atom-one-dark-theme
  :demand t

  :general
  (my-map
    "t t" 'my-load-atom-one-dark-theme)

  :init
  (defun my-load-atom-one-dark-theme ()
    "Load theme if not already loaded, else re-enable.

This loads the chosen Solarized variant as well as solarized-ext,
which includes additions and modifications to solarized."
    (interactive)

    (if (memq 'atom-one-dark custom-enabled-themes)
        (enable-theme 'atom-one-dark)
      (load-theme 'atom-one-dark t))

    (load-theme 'atom-one-dark t)

    (my-atom-one-dark-customizations))

  :config
  (my-after-frame (my-load-atom-one-dark-theme)))

(use-package solarized
  ;; NOTE
  ;; This is for using the fork.
  ;; :load-path "~/code/emacs/solarized-emacs"
  ;; :straight nil

  :straight solarized-theme

  :disabled t

  ;; :general
  ;; (my-map
  ;;   "t t" 'my-load-solarized-theme)

  :init
  (defun my-load-solarized-theme ()
    "Load theme if not already loaded, else re-enable.

This loads the chosen Solarized variant as well as solarized-ext,
which includes additions and modifications to solarized."
    (interactive)

    (if (memq my--solarized-theme-name custom-enabled-themes)
        (enable-theme my--solarized-theme-name)
      (load-theme my--solarized-theme-name t))

    (load-theme 'solarized-ext t))

  :config
  (my-after-frame (my-load-solarized-theme)))

(provide 'conf/theme)
