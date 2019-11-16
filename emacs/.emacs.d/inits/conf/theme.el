(require 'straight)
(require 'use-package)
(require 'conf/common)

(use-package solarized
  ;; NOTE
  ;; This is for using the fork.
  ;; :load-path "~/code/emacs/solarized-emacs"
  ;; :straight nil

  :straight solarized-theme

  :demand t

  :general
  (my-map
    "t t" 'my-load-theme)

  :init
  (defun my-load-theme ()
    "Load theme if not already loaded, else re-enable.

This loads the chosen Solarized variant as well as solarized-ext,
which includes additions and modifications to solarized."
    (interactive)

    (if (memq my--solarized-theme-name custom-enabled-themes)
        (enable-theme my--solarized-theme-name)
      (load-theme my--solarized-theme-name t))

    (load-theme 'solarized-ext t))

  :config
  (my-after-frame (my-load-theme)))

(provide 'conf/theme)
