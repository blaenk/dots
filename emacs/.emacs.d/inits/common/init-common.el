(defun blaenk/emacs-dir (path)
  (expand-file-name path user-emacs-directory))

(defun blaenk/cache-dir (path)
  (blaenk/emacs-dir (concat "cache/" path)))

(defun blaenk/inits-dir (path)
  (blaenk/emacs-dir (concat "inits/" path)))

(defun blaenk/is-fullscreen ()
  (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth)))

(defun blaenk/go-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen 'fullboth))

(defun blaenk/un-fullscreen ()
  (set-frame-parameter nil 'fullscreen nil))

(defvar blaenk/was-fullscreen)

(defun blaenk/fullscreen-if-wasnt ()
  (if (blaenk/is-fullscreen)
      (setq blaenk/was-fullscreen t)
    (progn
      (setq blaenk/was-fullscreen nil)
      (blaenk/go-fullscreen))))

(defun blaenk/unfullscreen-if-wasnt ()
  (when (not blaenk/was-fullscreen)
    (blaenk/un-fullscreen)))

(defmacro blaenk/setq-append (var &rest elems)
  `(setq ,var (append ,var '(,@elems))))

(defmacro blaenk/after-frame (body)
  `(if (daemonp)
       (add-hook 'after-make-frame-functions
                 (lambda (frame)
                   (with-selected-frame frame
                     ,body)))
     ;; can get current frame with
     ;; (window-frame (get-buffer-window))
     ,body))

(defun blaenk/get-faces (pos)
  "Get the font faces at POS."
  (remq nil
        (list
         (get-char-property pos 'read-face-name)
         (get-char-property pos 'face)
         (plist-get (text-properties-at pos) 'face))))

(defun blaenk/what-face (pos)
  (interactive "d")
  (let ((face (blaenk/get-faces pos)))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

; (defun blaenk/eval-after-load-all (features body)
;   (if (null features)
;       body
;     (let ((feat (car features))
;           (nested (blaenk/eval-after-load-all (cdr features) body)))
;       `(eval-after-load (quote ,feat) (quote ,nested)))))

; (defmacro blaenk/eval-after-load-all-macro (features body)
;   (if (null features)
;       body
;     `(eval-after-load (quote ,(car features))
;        (quote (blaenk/eval-after-load-all-macro ,(cdr features) ,body)))))

(provide 'init-common)
