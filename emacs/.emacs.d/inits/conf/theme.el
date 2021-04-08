(require 'straight)
(require 'use-package)
(require 'conf/common)

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

    (load-theme 'atom-one-dark-ext t))

  :config
  (my-after-frame (my-load-atom-one-dark-theme)))

(provide 'conf/theme)
