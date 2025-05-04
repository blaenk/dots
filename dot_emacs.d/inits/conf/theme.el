(require 'straight)
(require 'use-package)
(require 'conf/common)

(use-package atom-one-dark-theme
  :demand t

  :general-config
  (my-map
    "t t" 'my-load-atom-one-dark-theme)

  :init
  (defun my-load-atom-one-dark-theme ()
    "Load theme if not already loaded, else re-enable."
    (interactive)

    (if (memq 'atom-one-dark custom-enabled-themes)
        (enable-theme 'atom-one-dark)
      (load-theme 'atom-one-dark t))

    (load-theme 'atom-one-dark-ext t)
    )

  :config
  (my-load-atom-one-dark-theme))

(provide 'conf/theme)
