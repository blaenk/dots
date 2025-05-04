;; Sources:
;; https://github.com/hlissner/doom-emacs/blob/develop/early-init.el
;; https://github.com/noctuid/dotfiles/blob/master/emacs/.emacs.d/init.el

(setq package-enable-at-startup nil)

;; ** Disable Tool Bar, Menu Bar, and Scroll Bar
;; doing this here reduces init time by ~0.2 seconds
;; disabling `tool-bar-mode' in normal init file takes ~0.1s
;; https://github.com/raxod502/radian/issues/180
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; ** Set Font
;; https://github.com/hlissner/doom-emacs/blob/develop/early-init.el
;; "Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default."
;; I haven't actually noticed any difference for fonts I've tried
(setq frame-inhibit-implied-resize t)

;; ** Don't Load Site Startup File or Default Init File
;; not loading default.el actually does have an impact (sometimes? it seems like
;; it's loaded after init and not always?)
(setq site-run-file nil
      inhibit-default-init t)

(setq native-comp-deferred-compilation t)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

(unless (or (daemonp) noninteractive)
  ;; (let ((old-file-name-handler-alist file-name-handler-alist))
  ;;   ;; `file-name-handler-alist' is consulted on each `require', `load' and
  ;;   ;; various path/io functions. You get a minor speed up by unsetting this.
  ;;   ;; Some warning, however: this could cause problems on builds of Emacs where
  ;;   ;; its site lisp files aren't byte-compiled and we're forced to load the
  ;;   ;; *.el.gz files (e.g. on Alpine).
  ;;   (setq-default file-name-handler-alist nil)
  ;;   ;; ...but restore `file-name-handler-alist' later, because it is needed for
  ;;   ;; handling encrypted or compressed files, among other things.
  ;;   (defun doom-reset-file-handler-alist-h ()
  ;;     (setq file-name-handler-alist
  ;;           ;; Merge instead of overwrite because there may have bene changes to
  ;;           ;; `file-name-handler-alist' since startup we want to preserve.
  ;;           (delete-dups (append file-name-handler-alist
  ;;                                old-file-name-handler-alist))))
  ;;   (add-hook 'emacs-startup-hook #'doom-reset-file-handler-alist-h 101))

  ;; Premature redisplays can substantially affect startup times and produce
  ;; ugly flashes of unstyled Emacs.
  ;; TODO: Disabled because it causes a jarring-white flash.
  ;; (setq-default inhibit-redisplay t
  ;;               inhibit-message t)
  ;; (add-hook 'window-setup-hook
  ;;           (lambda ()
  ;;             (setq-default inhibit-redisplay nil
  ;;                           inhibit-message nil)
  ;;             (redisplay)))

  ;; Site files tend to use `load-file', which emits "Loading X..." messages in
  ;; the echo area, which in turn triggers a redisplay. Redisplays can have a
  ;; substantial effect on startup times and in this case happens so early that
  ;; Emacs may flash white while starting up.
  (define-advice load-file (:override (file) silence)
    (load file nil 'nomessage)))

;; * Prevent Default Mode Line from Showing
;; https://github.com/hlissner/doom-emacs/blob/7460e9e7989c9b219879073690e6f43ac535d274/modules/ui/modeline/config.el#L16
;; doesn't actually need to be set this early but it still makes sense to put it
;; here
(unless after-init-time
  ;; prevent flash of unstyled modeline at startup
  (setq-default mode-line-format nil))

;; Contrary to what many Emacs users have in their configs, you don't need
;; more than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

;; set-language-enviornment sets default-input-method, which is unwanted
(setq default-input-method nil)
